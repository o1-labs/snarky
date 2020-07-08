open Core_kernel
open Fold_lib
open Tuple_lib

let ( = ) = `Don't_use_polymorphic_equality

module type Basic_intf = sig
  module Nat : Nat_intf.S

  type t [@@deriving eq]

  val order : Nat.t

  val one : t

  val zero : t

  val ( + ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( / ) : t -> t -> t

  val square : t -> t
end

module type Intf = sig
  type t [@@deriving bin_io, sexp, yojson, compare, hash]

  include Basic_intf with type t := t

  val gen : t Quickcheck.Generator.t

  val gen_incl : t -> t -> t Quickcheck.Generator.t

  val gen_uniform : t Quickcheck.Generator.t

  val gen_uniform_incl : t -> t -> t Quickcheck.Generator.t

  val random : unit -> t

  val negate : t -> t

  val inv : t -> t

  val parity : t -> bool
end

module type Sqrt_field_intf = sig
  include Intf

  val is_square : t -> bool

  val sqrt : t -> t
end

module type Extended_intf = sig
  include Sqrt_field_intf

  val ( ** ) : t -> Nat.t -> t
end

module type Fp_intf = sig
  include Intf

  include Stringable.S with type t := t

  include Stringable.S with type t := t

  val of_int : int -> t

  val of_bits : bool list -> t option

  val to_bigint : t -> Nat.t

  val of_bigint : Nat.t -> t

  val fold_bits : t -> bool Fold.t

  val fold : t -> bool Triple.t Fold.t

  val to_bits : t -> bool list

  val length_in_bits : int

  val is_square : t -> bool

  val sqrt : t -> t
end

module type Extension_intf = sig
  type base

  include Extended_intf

  val scale : t -> base -> t

  val of_base : base -> t

  val project_to_base : t -> base

  val to_list : t -> base list
end

module Extend (F : Basic_intf) = struct
  open F

  let ( ** ) x n =
    let k = Nat.num_bits n in
    let rec go acc i =
      if Int.(i < 0) then acc
      else
        let acc = square acc in
        let acc = if Nat.test_bit n i then acc * x else acc in
        go acc Int.(i - 1)
    in
    go one Int.(k - 1)

  let is_square =
    let euler = Nat.((order - of_int 1) // of_int 2) in
    fun x -> equal (x ** euler) one

  module Sqrt_params = struct
    let two_adicity n =
      let rec go i = if Nat.test_bit n i then i else go Int.(i + 1) in
      go 0

    type nonrec t =
      {two_adicity: int; quadratic_non_residue_to_t: t; t_minus_1_over_2: Nat.t}

    let first f =
      let rec go i = match f i with Some x -> x | None -> go (i + one) in
      go one

    let create () =
      let p_minus_one = Nat.(order - of_int 1) in
      let s = two_adicity p_minus_one in
      let t = Nat.shift_right p_minus_one s in
      let quadratic_non_residue =
        first (fun i -> Option.some_if (not (is_square i)) i)
      in
      { two_adicity= s
      ; quadratic_non_residue_to_t= quadratic_non_residue ** t
      ; t_minus_1_over_2= Nat.((t - of_int 1) // of_int 2) }

    let t = lazy (create ())
  end

  let rec loop ~while_ ~init f =
    if while_ init then loop ~while_ ~init:(f init) f else init

  let rec pow2 b n = if n > 0 then pow2 (square b) Int.(n - 1) else b

  let sqrt =
    let pow2_order b =
      loop
        ~while_:(fun (b2m, _) -> not (equal b2m one))
        ~init:(b, 0)
        (fun (b2m, m) -> (square b2m, Int.succ m))
      |> snd
    in
    let module Loop_params = struct
      type nonrec t = {z: t; b: t; x: t; v: int}
    end in
    let open Loop_params in
    fun a ->
      let { Sqrt_params.two_adicity= v
          ; quadratic_non_residue_to_t= z
          ; t_minus_1_over_2 } =
        Lazy.force Sqrt_params.t
      in
      let w = a ** t_minus_1_over_2 in
      let x = a * w in
      let b = x * w in
      let {x; _} =
        loop
          ~while_:(fun p -> not (equal p.b one))
          ~init:{z; b; x; v}
          (fun {z; b; x; v} ->
            let m = pow2_order b in
            let w = pow2 z Int.(v - m - 1) in
            let z = square w in
            {z; b= b * z; x= x * w; v= m} )
      in
      x
end

module Make_fp
    (N : Nat_intf.S) (Info : sig
        val order : N.t
    end) : Fp_intf with module Nat = N and type t = private N.t = struct
  include Info

  module T = struct
    let zero = N.of_int 0

    let one = N.of_int 1

    let length_in_bits = N.num_bits N.(Info.order - one)

    module Nat = N
    open Nat

    let order = Info.order

    (* TODO version *)
    type t = N.t [@@deriving eq, sexp, yojson, compare, hash]

    let length_in_bytes = Int.((length_in_bits + 7) / 8)

    (** serialization meant to be identical to field serializations in snarky *)
    include Bin_prot.Utils.Of_minimal (struct
      type nonrec t = t

      let bin_shape_t =
        Bin_prot.Shape.basetype
          (Bin_prot.Shape.Uuid.of_string
             (sprintf "snarkette_field_%d" length_in_bytes))
          []

      let __bin_read_t__ _buf ~pos_ref _vint =
        Bin_prot.Common.raise_variant_wrong_type "Fp.t" !pos_ref

      let bin_size_t _ = length_in_bytes

      let bin_write_t buf ~pos t =
        let bs = Bigstring.of_string (N.to_bytes t) in
        let n = Bigstring.length bs in
        Bigstring.blit ~src:bs ~dst:buf ~src_pos:0 ~dst_pos:pos ~len:n ;
        if Int.(n < length_in_bytes) then
          for i = n to Int.(length_in_bytes - 1) do
            Bigstring.set buf Int.(pos + i) '\000'
          done ;
        Int.(pos + length_in_bytes)

      let bin_read_t buf ~pos_ref =
        let open Int in
        let remaining_bytes = Bigstring.length buf - !pos_ref in
        if remaining_bytes < length_in_bytes then
          failwithf "Field.bin_read_t: Expected %d bytes, got %d"
            length_in_bytes remaining_bytes () ;
        let t =
          N.of_bytes
            (Bigstring.to_string buf ~pos:!pos_ref ~len:length_in_bytes)
        in
        pos_ref := length_in_bytes + !pos_ref ;
        t
    end)

    let ( + ) x y = (x + y) % Info.order

    let ( - ) x y = (x - y) % Info.order

    let ( * ) x y = x * y % Info.order

    let square x = x * x

    let rec extended_euclidean a b =
      if equal b zero then (a, one, zero)
      else
        match extended_euclidean b (a % b) with
        | d, x, y ->
            (d, y, x - (a // b * y))

    let inv_no_mod x =
      let _, a, _b = extended_euclidean x Info.order in
      a

    let inv x = inv_no_mod x % Info.order

    let ( / ) x y = x * inv_no_mod y
  end

  include Extend (T)
  include T

  let of_bigint x = N.(x % Info.order)

  let to_bigint = Fn.id

  let parity t = N.test_bit (to_bigint t) 0

  let make_gen gen lo hi =
    let t_of_bignum_bigint n = Bigint.to_string n |> N.of_string in
    Quickcheck.Generator.map (gen lo hi) ~f:t_of_bignum_bigint

  (* fix zero, size - 1 bounds *)
  let make_gen_full gen =
    let size = order |> N.to_string |> Bigint.of_string in
    make_gen gen Bigint.zero Bigint.(size - one)

  let gen = make_gen_full Bigint.gen_incl

  let gen_incl lo hi =
    let bignum_bigint_of_t t = N.to_string t |> Bigint.of_string in
    make_gen Bigint.gen_incl (bignum_bigint_of_t lo) (bignum_bigint_of_t hi)

  let gen_uniform = make_gen_full Bigint.gen_uniform_incl

  let gen_uniform_incl lo hi =
    let bignum_bigint_of_t t = N.to_string t |> Bigint.of_string in
    make_gen Bigint.gen_uniform_incl (bignum_bigint_of_t lo)
      (bignum_bigint_of_t hi)

  let random () = Quickcheck.random_value gen_uniform

  let fold_bits n : bool Fold_lib.Fold.t =
    { fold=
        (fun ~init ~f ->
          let rec go acc i =
            if Int.(i = length_in_bits) then acc
            else go (f acc (N.test_bit n i)) Int.(i + 1)
          in
          go init 0 ) }

  let to_bits = Fn.compose Fold_lib.Fold.to_list fold_bits

  let fold n = Fold_lib.Fold.group3 ~default:false (fold_bits n)

  let of_bits bits =
    let rec go acc i = function
      | [] ->
          acc
      | b :: bs ->
          let acc = if b then N.log_or acc (N.shift_left one i) else acc in
          go acc Int.(i + 1) bs
    in
    let r = go zero 0 bits in
    if N.( < ) r Info.order then Some r else None

  open N

  let of_int = N.of_int

  let of_string = N.of_string

  let to_string = N.to_string

  let negate x = N.( - ) Info.order x

  let%test_unit "exp test" = [%test_eq: t] (of_int 8) (of_int 2 ** of_int 3)

  let%test_unit "pow2" =
    let b = 7 in
    if N.(of_int Int.(7 ** 8) < order) then
      [%test_eq: t] (pow2 (of_int b) 3) (of_int Int.(7 ** 8))
    else ()

  let%test_unit "sqrt agrees with integer square root on small values" =
    let rec mem a = function
      | [] ->
          ()
      | x :: xs -> (
        try [%test_eq: t] a x with _ -> mem a xs )
    in
    let gen = Int.gen_incl 1 Int.max_value_30_bits in
    Quickcheck.test ~trials:10 gen ~f:(fun n ->
        let n = abs n in
        let n2 = Int.(n * n) in
        mem (sqrt (of_int n2)) [of_int n; Info.order - of_int n] )
end

module type Degree_2_extension_intf = sig
  type base

  include Extension_intf with type base := base and type t = base * base
end

module type Degree_3_extension_intf = sig
  type base

  include Extension_intf with type base := base and type t = base * base * base
end

let ( % ) x n =
  let r = x mod n in
  if r < 0 then r + n else r

let find_wnaf (type t) (module N : Nat_intf.S with type t = t) window_size
    scalar =
  let one = N.of_int 1 in
  let first_k_bits c k =
    let k_bits = N.(shift_left one k - one) in
    N.to_int_exn (N.log_and k_bits c)
  in
  let length = N.num_bits scalar in
  let res = Array.init (length + 1) ~f:(fun _ -> 0) in
  let zero = N.of_int 0 in
  let rec go c j =
    if N.equal zero c then ()
    else
      let u, c =
        if N.test_bit c 0 then
          let u =
            let u = first_k_bits c (window_size + 1) in
            if u > 1 lsl window_size then u - (1 lsl (window_size + 1)) else u
          in
          let c = N.(c - of_int u) in
          (u, c)
        else (0, c)
      in
      res.(j) <- u ;
      go (N.shift_right c 1) (j + 1)
  in
  go scalar 0 ; res

module Make_fp3
    (Fp : Intf) (Info : sig
        val non_residue : Fp.t

        val frobenius_coeffs_c1 : Fp.t array

        val frobenius_coeffs_c2 : Fp.t array
    end) : sig
  include Degree_3_extension_intf with type base = Fp.t and module Nat = Fp.Nat

  val non_residue : Fp.t

  val frobenius : t -> int -> t
end = struct
  include Info

  type base = Fp.t

  let componentwise f (x1, x2, x3) (y1, y2, y3) = (f x1 y1, f x2 y2, f x3 y3)

  let of_base x = (x, Fp.zero, Fp.zero)

  module T = struct
    module Nat = Fp.Nat

    let order = Nat.(Fp.order * Fp.order * Fp.order)

    type t = Fp.t * Fp.t * Fp.t
    [@@deriving eq, bin_io, sexp, yojson, compare, hash]

    let ( + ) = componentwise Fp.( + )

    let ( - ) = componentwise Fp.( - )

    let ( * ) (a1, b1, c1) (a2, b2, c2) =
      let a = Fp.(a1 * a2) in
      let b = Fp.(b1 * b2) in
      let c = Fp.(c1 * c2) in
      let open Fp in
      ( a + (non_residue * (((b1 + c1) * (b2 + c2)) - b - c))
      , ((a1 + b1) * (a2 + b2)) - a - b + (non_residue * c)
      , ((a1 + c1) * (a2 + c2)) - a + b - c )

    let square (a, b, c) =
      let s0 = Fp.square a in
      let ab = Fp.(a * b) in
      let s1 = Fp.(ab + ab) in
      let s2 = Fp.(square (a - b + c)) in
      let bc = Fp.(b * c) in
      let s3 = Fp.(bc + bc) in
      let s4 = Fp.square c in
      let open Fp in
      (s0 + (non_residue * s3), s1 + (non_residue * s4), s1 + s2 + s3 - s0 - s4)

    let inv (a, b, c) =
      let open Fp in
      let t0 = square a in
      let t1 = square b in
      let t2 = square c in
      let t3 = a * b in
      let t4 = a * c in
      let t5 = b * c in
      let c0 = t0 - (non_residue * t5) in
      let c1 = (non_residue * t2) - t3 in
      let c2 = t1 - t4 in
      let t6 = (a * c0) + (non_residue * ((c * c1) + (b * c2))) |> inv in
      (t6 * c0, t6 * c1, t6 * c2)

    let ( / ) x y = x * inv y

    let one = of_base Fp.one

    let zero = of_base Fp.zero
  end

  include T
  include Extend (T)

  let gen = Quickcheck.Generator.tuple3 Fp.gen Fp.gen Fp.gen

  let gen_incl (lo1, lo2, lo3) (hi1, hi2, hi3) =
    Quickcheck.Generator.tuple3 (Fp.gen_incl lo1 hi1) (Fp.gen_incl lo2 hi2)
      (Fp.gen_incl lo3 hi3)

  let gen_uniform =
    Quickcheck.Generator.tuple3 Fp.gen_uniform Fp.gen_uniform Fp.gen_uniform

  let gen_uniform_incl (lo1, lo2, lo3) (hi1, hi2, hi3) =
    Quickcheck.Generator.tuple3
      (Fp.gen_uniform_incl lo1 hi1)
      (Fp.gen_uniform_incl lo2 hi2)
      (Fp.gen_uniform_incl lo3 hi3)

  let random () = Quickcheck.random_value gen_uniform

  let to_list (x, y, z) = [x; y; z]

  let project_to_base (x, _, _) = x

  let parity = Fn.compose Fp.parity project_to_base

  let scale (x1, x2, x3) s = Fp.(s * x1, s * x2, s * x3)

  let negate (x1, x2, x3) = Fp.(negate x1, negate x2, negate x3)

  let frobenius (c0, c1, c2) power =
    let open Fp in
    let open Info in
    let i = power mod 3 in
    (c0, frobenius_coeffs_c1.(i) * c1, frobenius_coeffs_c2.(i) * c2)
end

module Make_fp2
    (Fp : Intf) (Info : sig
        val non_residue : Fp.t
    end) : sig
  include Degree_2_extension_intf with type base = Fp.t and module Nat = Fp.Nat
end = struct
  type base = Fp.t

  let of_base x = (x, Fp.zero)

  let componentwise f (x1, x2) (y1, y2) = (f x1 y1, f x2 y2)

  module T = struct
    type t = Fp.t * Fp.t [@@deriving eq, yojson, bin_io, sexp, compare, hash]

    module Nat = Fp.Nat

    let order = Nat.(Fp.order * Fp.order)

    let one = of_base Fp.one

    let zero = of_base Fp.zero

    let ( + ) = componentwise Fp.( + )

    let ( - ) = componentwise Fp.( - )

    let square (a, b) =
      let open Info in
      let ab = Fp.(a * b) in
      Fp.
        (((a + b) * (a + (non_residue * b))) - ab - (non_residue * ab), ab + ab)

    let ( * ) (a1, b1) (a2, b2) =
      let open Fp in
      let a = a1 * a2 in
      let b = b1 * b2 in
      (a + (Info.non_residue * b), ((a1 + b1) * (a2 + b2)) - a - b)

    let inv (a, b) =
      let open Fp in
      let t0 = square a in
      let t1 = square b in
      let t2 = t0 - (Info.non_residue * t1) in
      let t3 = inv t2 in
      let c0 = a * t3 in
      let c1 = negate (b * t3) in
      (c0, c1)

    let ( / ) x y = x * inv y
  end

  include T
  include Extend (T)

  let gen = Quickcheck.Generator.tuple2 Fp.gen Fp.gen

  let gen_incl (lo1, lo2) (hi1, hi2) =
    Quickcheck.Generator.tuple2 (Fp.gen_incl lo1 hi1) (Fp.gen_incl lo2 hi2)

  let gen_uniform = Quickcheck.Generator.tuple2 Fp.gen_uniform Fp.gen_uniform

  let gen_uniform_incl (lo1, lo2) (hi1, hi2) =
    Quickcheck.Generator.tuple2
      (Fp.gen_uniform_incl lo1 hi1)
      (Fp.gen_uniform_incl lo2 hi2)

  let random () = Quickcheck.random_value gen_uniform

  let to_list (x, y) = [x; y]

  let project_to_base (x, _) = x

  let parity = Fn.compose Fp.parity project_to_base

  let scale (x1, x2) s = Fp.(s * x1, s * x2)

  let negate (a, b) = Fp.(negate a, negate b)
end

module Make_fp6
    (N : Nat_intf.S)
    (Fp : Intf)
    (Fp2 : Degree_2_extension_intf with type base = Fp.t) (Fp3 : sig
        include Degree_3_extension_intf with type base = Fp.t

        val frobenius : t -> int -> t

        val non_residue : Fp.t
    end) (Info : sig
      val non_residue : Fp.t

      val frobenius_coeffs_c1 : Fp.t array
    end) : sig
  include
    Degree_2_extension_intf with type base = Fp3.t and module Nat = Fp.Nat

  val mul_by_2345 : t -> t -> t

  val frobenius : t -> int -> t

  val cyclotomic_exp : t -> N.t -> t

  val unitary_inverse : t -> t
end = struct
  module T = struct
    module Nat = Fp.Nat

    let of_base x = (x, Fp3.zero)

    let componentwise f (x1, x2) (y1, y2) = (f x1 y1, f x2 y2)

    type t = Fp3.t * Fp3.t [@@deriving eq, yojson, bin_io, sexp, compare, hash]

    let order =
      let open Nat in
      let square x = x * x in
      let p = Fp.order in
      square (p * square p)

    let zero = of_base Fp3.zero

    let one = of_base Fp3.one

    let ( + ) = componentwise Fp3.( + )

    let ( - ) = componentwise Fp3.( - )

    let mul_by_non_residue ((c0, c1, c2) : Fp3.t) =
      Fp.(Info.non_residue * c2, c0, c1)

    let square (a, b) =
      let ab = Fp3.(a * b) in
      let open Fp3 in
      ( ((a + b) * (a + mul_by_non_residue b)) - ab - mul_by_non_residue ab
      , ab + ab )

    let ( * ) (a1, b1) (a2, b2) =
      let a = Fp3.(a1 * a2) in
      let b = Fp3.(b1 * b2) in
      let beta_b = mul_by_non_residue b in
      Fp3.(a + beta_b, ((a1 + b1) * (a2 + b2)) - a - b)

    let inv (a, b) =
      let t1 = Fp3.square b in
      let t0 = Fp3.(square a - mul_by_non_residue t1) in
      let new_t1 = Fp3.inv t0 in
      Fp3.(a * new_t1, negate (b * new_t1))

    let ( / ) x y = x * inv y
  end

  include T
  include Extend (T)

  type base = Fp3.t

  let gen = Quickcheck.Generator.tuple2 Fp3.gen Fp3.gen

  let gen_incl (lo1, lo2) (hi1, hi2) =
    Quickcheck.Generator.tuple2 (Fp3.gen_incl lo1 hi1) (Fp3.gen_incl lo2 hi2)

  let gen_uniform = Quickcheck.Generator.tuple2 Fp3.gen_uniform Fp3.gen_uniform

  let gen_uniform_incl (lo1, lo2) (hi1, hi2) =
    Quickcheck.Generator.tuple2
      (Fp3.gen_uniform_incl lo1 hi1)
      (Fp3.gen_uniform_incl lo2 hi2)

  let random () = Quickcheck.random_value gen_uniform

  let to_list (x, y) = [x; y]

  let project_to_base (x, _) = x

  let parity = Fn.compose Fp3.parity project_to_base

  let scale (x1, x2) s = Fp3.(s * x1, s * x2)

  let mul_by_2345 (a1, b1) (a2, b2) =
    let open Info in
    let a1_0, a1_1, a1_2 = a1 in
    let _, _, a2_2 = a2 in
    (let a2_0, a2_1, _ = a2 in
     assert (Fp.(equal a2_0 zero)) ;
     assert (Fp.(equal a2_1 zero))) ;
    let a =
      Fp.(a1_1 * a2_2 * non_residue, a1_2 * a2_2 * non_residue, a1_0 * a2_2)
    in
    let b = Fp3.(b1 * b2) in
    let beta_b = mul_by_non_residue b in
    Fp3.(a + beta_b, ((a1 + b2) * (a2 + b2)) - a - b)

  let negate (a, b) = Fp3.(negate a, negate b)

  let unitary_inverse (x, y) = (x, Fp3.negate y)

  let cyclotomic_square ((c00, c01, c02), (c10, c11, c12)) =
    let a : Fp2.t = (c00, c11) in
    let b : Fp2.t = (c10, c02) in
    let c : Fp2.t = (c01, c12) in
    let asq = Fp2.square a in
    let bsq = Fp2.square b in
    let csq = Fp2.square c in
    let a_a =
      let open Fp in
      let a_a = fst asq - fst a in
      a_a + a_a + fst asq
    in
    let a_b =
      let open Fp in
      let a_b = snd asq + snd a in
      a_b + a_b + snd asq
    in
    let b_a =
      let open Fp in
      let b_tmp = Fp3.non_residue * snd csq in
      let b_a = b_tmp + fst b in
      b_a + b_a + b_tmp
    in
    let b_b =
      let open Fp in
      let b_b = fst csq - snd b in
      b_b + b_b + fst csq
    in
    let c_a =
      let open Fp in
      let c_a = fst bsq - fst c in
      c_a + c_a + fst bsq
    in
    let c_b =
      let open Fp in
      let c_b = snd bsq + snd c in
      c_b + c_b + snd bsq
    in
    ((a_a, c_a, b_b), (b_a, a_b, c_b))

  let cyclotomic_exp x exponent =
    let x_inv = inv x in
    let naf = find_wnaf (module N) 1 exponent in
    let rec go found_nonzero res i =
      if i < 0 then res
      else
        let res = if found_nonzero then cyclotomic_square res else res in
        if naf.(i) <> 0 then
          let found_nonzero = true in
          let res = if naf.(i) > 0 then res * x else res * x_inv in
          go found_nonzero res Int.(i - 1)
        else go found_nonzero res Int.(i - 1)
    in
    go false one Int.(Array.length naf - 1)

  let frobenius (c0, c1) power =
    ( Fp3.frobenius c0 power
    , Fp3.(scale (frobenius c1 power) Info.frobenius_coeffs_c1.(power mod 6))
    )
end
