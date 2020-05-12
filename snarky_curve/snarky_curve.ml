module Field_intf = struct
  module type Basic = sig
    type t

    val ( * ) : t -> t -> t

    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val inv_exn : t -> t

    val negate : t -> t

    val square : t -> t
  end

  module type Constant = sig
    include Basic
  end

  module type Checked = sig
    type field

    type bool

    include Basic

    val if_ : bool -> then_:t -> else_:t -> t

    val scale : t -> field -> t
  end
end

module type Constant_intf = sig
  type field

  type t

  val random : unit -> t

  val to_affine_exn : t -> field * field

  val of_affine : field * field -> t

  val ( + ) : t -> t -> t

  val negate : t -> t
end

module type Inputs_intf = sig
  module Impl : Snarky.Snark_intf.Run with type prover_state = unit

  module F : sig
    include
      Field_intf.Checked
      with type bool := Impl.Boolean.var
       and type field := Impl.field

    module Constant : Field_intf.Constant

    val assert_square : t -> t -> unit

    val assert_r1cs : t -> t -> t -> unit

    val typ : (t, Constant.t) Impl.Typ.t

    val constant : Constant.t -> t
  end

  module Constant : Constant_intf with type field := F.Constant.t

  module Params : sig
    val one : F.Constant.t * F.Constant.t

    val group_size_in_bits : int

    val a : F.Constant.t

    val b : F.Constant.t
  end
end

module Make_checked (Inputs : Inputs_intf) = struct
  open Inputs
  open Impl

  type t = F.t * F.t

  let double ((ax, ay) : t) : t =
    let open F in
    (* A: 1
       B: 1
       C: 1 *)
    let x_squared = square ax in
    let lambda =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let x_squared = read typ x_squared in
              let ay = read typ ay in
              let open F.Constant in
              (x_squared + x_squared + x_squared + Params.a) * inv_exn (ay + ay))
    in
    let bx =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let lambda = read typ lambda in
              let ax = read typ ax in
              let open F.Constant in
              square lambda - (ax + ax))
    in
    let by =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let lambda = read typ lambda
              and ax = read typ ax
              and ay = read typ ay
              and bx = read typ bx in
              F.Constant.((lambda * (ax - bx)) - ay))
    in
    let two = Field.Constant.of_int 2 in
    (* A: 1
       B: 1
       C: 2 *)
    assert_r1cs (F.scale lambda two) ay
      (F.scale x_squared (Field.Constant.of_int 3) + F.constant Params.a) ;
    (* A: 1
       B: 1
       C: 2
    *)
    assert_square lambda (bx + F.scale ax two) ;
    (* A: 1
       B: 2
       C: 2
    *)
    assert_r1cs lambda (ax - bx) (by + ay) ;
    (* Overall:
       A: 4
       B: 5
       C: 7 *)
    (bx, by)

  let add' ~div (ax, ay) (bx, by) : t =
    let open F in
    (* 
     lambda * (bx - ax) = (by - ay)

      A: 1
      B: 2
      C: 2
    *)
    let lambda = div (by - ay) (bx - ax) in
    let cx =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let ax = read typ ax
              and bx = read typ bx
              and lambda = read typ lambda in
              Constant.(square lambda - (ax + bx)))
    in
    (* lambda^2 = cx + ax + bx

       A: 1
       B: 1
       C: 3
    *)
    assert_square lambda F.(cx + ax + bx) ;
    let cy =
      exists typ
        ~compute:
          As_prover.(
            fun () ->
              let ax = read typ ax
              and ay = read typ ay
              and cx = read typ cx
              and lambda = read typ lambda in
              Constant.((lambda * (ax - cx)) - ay))
    in
    (* A: 1
       B: 2
       C: 2 *)
    assert_r1cs lambda (ax - cx) (cy + ay) ;
    (* Overall
       A: 2
       B: 5
       C: 7 *)
    (cx, cy)

  let add_exn p q = add' ~div:(fun x y -> F.(inv_exn y * x)) p q

  let to_affine_exn x = x

  let constant t =
    let x, y = Constant.to_affine_exn t in
    (F.constant x, F.constant y)

  let negate (x, y) = (x, F.negate y)

  let one =
    let x, y = Params.one in
    F.(constant x, constant y)

  let assert_on_curve (x, y) =
    let open F in
    let x2 = square x in
    let x3 = x2 * x in
    let ax = constant Params.a * x in
    assert_square y (x3 + ax + constant Params.b)

  let typ : (t, Constant.t) Typ.t =
    let unchecked =
      Typ.transport
        Typ.(tuple2 F.typ F.typ)
        ~there:Constant.to_affine_exn ~back:Constant.of_affine
    in
    { unchecked with
      check= (fun t -> make_checked (fun () -> assert_on_curve t)) }

  let if_ c ~then_:(tx, ty) ~else_:(ex, ey) =
    (F.if_ c ~then_:tx ~else_:ex, F.if_ c ~then_:ty ~else_:ey)

  open Bitstring_lib.Bitstring

  module Scalar = struct
    type t = Boolean.var Lsb_first.t

    let of_field = Field.unpack_full

    let to_field t = Field.project (Lsb_first.to_list t)
  end

  module type Shifted_intf = sig
    type t

    val zero : t

    val unshift_nonzero : t -> F.t * F.t

    val add : t -> F.t * F.t -> t

    val if_ : Boolean.var -> then_:t -> else_:t -> t
  end

  module Shifted (M : sig
    val shift : t
  end)
  () : Shifted_intf = struct
    type t = F.t * F.t

    let zero = M.shift

    let unshift_nonzero t = add_exn t (negate M.shift)

    let add t pt = add_exn t pt

    let if_ = if_
  end

  let shifted () =
    let shift = exists typ ~compute:(fun () -> Constant.random ()) in
    let module S =
      Shifted (struct
          let shift = shift
        end)
        ()
    in
    (module S : Shifted_intf)

  (* This doesn't have great performance because it uses add_exn. Should be optimized *)
  let scale ?init t (c : Boolean.var Bitstring_lib.Bitstring.Lsb_first.t) =
    let (module S) = shifted () in
    let c = Bitstring_lib.Bitstring.Lsb_first.to_list c in
    let rec go i bs0 acc pt =
      match bs0 with
      | [] ->
          acc
      | b :: bs ->
          let acc' =
            let add_pt = S.add acc pt in
            let don't_add_pt = acc in
            S.if_ b ~then_:add_pt ~else_:don't_add_pt
          and pt' = double pt in
          go (i + 1) bs acc' pt'
    in
    let init =
      match init with None -> S.zero | Some init -> S.(add zero init)
    in
    S.unshift_nonzero (go 0 c init t)
end

module type Native_base_field_inputs = sig
  module Impl : Snarky.Snark_intf.Run with type prover_state = unit

  include
    Inputs_intf
    with module Impl := Impl
     and type F.t = Impl.Field.t
     and type F.Constant.t = Impl.field
end

module For_native_base_field (Inputs : Native_base_field_inputs) = struct
  open Core_kernel
  open Inputs
  open Impl
  include Make_checked (Inputs)

  module Window_table = struct
    open Tuple_lib

    type t = Constant.t Quadruple.t array

    let window_size = 2

    let windows = (Params.group_size_in_bits + window_size - 1) / window_size

    let shift_left_by_window_size =
      let rec go i acc =
        if i = 0 then acc else go (i - 1) Constant.(acc + acc)
      in
      go window_size

    (* (create g).(i) = ( unrelated_base^i + 2^{window_size*i} 0 g, unrelated_base^i + 2^{window_size*i} g, unrelated_base^i + 2^{window_size*i} (2 g), unrelated_base^i + 2^{window_size*i} (3 g) ) *)
    let create ~shifts g =
      let pure_windows =
        (* pure_windows.(i) = 2^{window_size * i} ( g, 2 g, 3 g) *)
        let w0 =
          let g2 = Constant.(g + g) in
          let g3 = Constant.(g2 + g) in
          (g, g2, g3)
        in
        let a = Array.init windows ~f:(fun _ -> w0) in
        for i = 1 to windows - 1 do
          a.(i) <- Triple.map ~f:shift_left_by_window_size a.(i - 1)
        done ;
        a
      in
      Array.mapi pure_windows ~f:(fun i (a, b, c) ->
          let shift = shifts.(i) in
          Constant.(shift, shift + a, shift + b, shift + c) )
  end

  let pow2s g =
    let n = Window_table.windows + 1 in
    assert (n < Params.group_size_in_bits) ;
    let a = Array.init n ~f:(fun _ -> g) in
    for i = 1 to n - 1 do
      let x = a.(i - 1) in
      a.(i) <- Constant.(x + x)
    done ;
    a

  module Scaling_precomputation = struct
    type t = {base: Constant.t; shifts: Constant.t array; table: Window_table.t}

    let group_map =
      lazy
        (let params =
           Group_map.Params.create
             (module Field.Constant)
             ~a:Params.a ~b:Params.b
         in
         Group_map.to_group (module Field.Constant) ~params)

    let string_to_bits s =
      List.concat_map (String.to_list s) ~f:(fun c ->
          let c = Char.to_int c in
          List.init 8 ~f:(fun i -> (c lsr i) land 1 = 1) )

    let create base =
      let unrelated_base =
        let x, y = Constant.to_affine_exn base in
        Digestif.BLAKE2S.digest_string
          Field.Constant.(to_string x ^ "," ^ to_string y)
        |> Digestif.BLAKE2S.to_raw_string |> string_to_bits
        |> Field.Constant.project |> Lazy.force group_map |> Constant.of_affine
      in
      let shifts = pow2s unrelated_base in
      {base; shifts; table= Window_table.create ~shifts base}
  end

  let add_unsafe =
    let div_unsafe x y =
      let z =
        exists Field.typ
          ~compute:
            As_prover.(
              fun () -> Field.Constant.( / ) (read_var x) (read_var y))
      in
      (* z = x / y <=> z * y = x *)
      assert_r1cs z y x ; z
    in
    add' ~div:div_unsafe

  let lookup_point (b0, b1) (t1, t2, t3, t4) =
    let b0_and_b1 = Boolean.( && ) b0 b1 in
    let lookup_one (a1, a2, a3, a4) =
      let open Field.Constant in
      let ( * ) x b = F.scale b x in
      let ( +^ ) = Field.( + ) in
      F.constant a1
      +^ ((a2 - a1) * (b0 :> Field.t))
      +^ ((a3 - a1) * (b1 :> Field.t))
      +^ ((a4 + a1 - a2 - a3) * (b0_and_b1 :> Field.t))
    in
    let x1, y1 = Constant.to_affine_exn t1
    and x2, y2 = Constant.to_affine_exn t2
    and x3, y3 = Constant.to_affine_exn t3
    and x4, y4 = Constant.to_affine_exn t4 in
    (* This is optimized for Marlin-style constraints. *)
    let seal a =
      let a' = exists Field.typ ~compute:(fun () -> As_prover.read_var a) in
      Field.Assert.equal a a' ; a'
    in
    (seal (lookup_one (x1, x2, x3, x4)), seal (lookup_one (y1, y2, y3, y4)))

  let rec pairs = function
    | [] ->
        []
    | x :: y :: xs ->
        (x, y) :: pairs xs
    | [x] ->
        [(x, Boolean.false_)]

  type shifted = {value: t; shift: Constant.t}

  let scale_known (pc : Scaling_precomputation.t) bs =
    let bs =
      let bs = Array.of_list bs in
      let num_bits = Array.length bs in
      Array.init
        ((Array.length bs + 1) / 2)
        ~f:(fun i ->
          let get j = if j < num_bits then bs.(j) else Boolean.false_ in
          (get (2 * i), get ((2 * i) + 1)) )
    in
    let windows_required = Array.length bs in
    let terms =
      Array.mapi bs ~f:(fun i bit_pair -> lookup_point bit_pair pc.table.(i))
    in
    let with_shifts = Array.reduce_exn terms ~f:add_unsafe in
    let shift =
      let unrelated_base = pc.shifts.(0) in
      (* 
         0b1111... * unrelated_base = (2^windows_required - 1) * unrelated_base

         is what we added and need to take away for the final result. *)
      Constant.(pc.shifts.(windows_required) + negate unrelated_base)
    in
    {value= with_shifts; shift}

  let unshift {value; shift} = add_exn value (constant (Constant.negate shift))

  let multiscale_known pairs =
    Array.map pairs ~f:(fun (s, g) -> scale_known g s)
    |> Array.reduce_exn ~f:(fun t1 t2 ->
           { value= add_exn t1.value t2.value
           ; shift= Constant.(t1.shift + t2.shift) } )
    |> unshift

  let scale_known pc bs = unshift (scale_known pc bs)

  (* b ? t : -t *)
  let conditional_negation (b : Boolean.var) (x, y) =
    let y' =
      exists Field.typ
        ~compute:
          As_prover.(
            fun () ->
              if read Boolean.typ b then read Field.typ y
              else Field.Constant.negate (read Field.typ y))
    in
    assert_r1cs y Field.((of_int 2 * (b :> Field.t)) - of_int 1) y' ;
    (x, y')

  let p_plus_q_plus_p ((x1, y1) : t) ((x2, y2) : t) =
    let open Field in
    let ( ! ) = As_prover.read typ in
    let lambda_1 =
      exists typ ~compute:Constant.(fun () -> (!y2 - !y1) / (!x2 - !x1))
    in
    let x3 =
      exists typ
        ~compute:Constant.(fun () -> (!lambda_1 * !lambda_1) - !x1 - !x2)
    in
    let lambda_2 =
      exists typ
        ~compute:
          Constant.(fun () -> (of_int 2 * !y1 / (!x1 - !x3)) - !lambda_1)
    in
    let x4 =
      exists typ
        ~compute:Constant.(fun () -> (!lambda_2 * !lambda_2) - !x3 - !x1)
    in
    let y4 =
      exists typ ~compute:Constant.(fun () -> ((!x1 - !x4) * !lambda_2) - !y1)
    in
    (* Determines lambda_1 *)
    assert_r1cs (x2 - x1) lambda_1 (y2 - y1) ;
    (* Determines x_3 *)
    assert_square lambda_1 (x1 + x2 + x3) ;
    (* Determines lambda_2 *)
    assert_r1cs (x1 - x3) (lambda_1 + lambda_2) (of_int 2 * y1) ;
    (* Determines x4 *)
    assert_square lambda_2 (x3 + x1 + x4) ;
    (* Determines y4 *)
    assert_r1cs (x1 - x4) lambda_2 (y4 + y1) ;
    (x4, y4)

  (* Input:
     t, r (LSB)

     Output:
    (2*r + 1 + 2^len(r)) t
  *)
  let scale (t : Field.t * Field.t) (`Times_two_plus_1_plus_2_to_len r) :
      Field.t * Field.t =
    let n = Array.length r in
    let acc = ref (double t) in
    let () =
      for i = 0 to n - 1 do
        let q = conditional_negation r.(i) t in
        acc := p_plus_q_plus_p !acc q
      done
    in
    !acc

  (* Input:
     t, k (LSB)

     Output:
     (k + 2^{len(k) - 1}) t

     Based on [Daira's algorithm](https://github.com/zcash/zcash/issues/3924)
  *)
  let scale t (`Plus_two_to_len_minus_1 k) =
    let m = Array.length k - 1 in
    let r = Array.init m ~f:(fun i -> k.(i + 1)) in
    let two_r_plus_1_plus_two_to_m =
      scale t (`Times_two_plus_1_plus_2_to_len r)
    in
    if_ k.(0) ~then_:two_r_plus_1_plus_two_to_m
      ~else_:(add_exn two_r_plus_1_plus_two_to_m (negate t))

  let%test_unit "scale" =
    let scale_constant (t, bs) =
      let rec go acc bs =
        match bs with
        | [] ->
            acc
        | b :: bs ->
            let acc = Constant.(acc + acc) in
            let acc = if b then Constant.(acc + t) else acc in
            go acc bs
      in
      let k = Array.length bs in
      go Constant.(t + negate t) (List.init k ~f:(fun i -> bs.(k - 1 - i)))
    in
    let module A = struct
      type t = Impl.Field.Constant.t * Impl.Field.Constant.t
      [@@deriving sexp, compare]
    end in
    let one = Constant.of_affine Params.one in
    [%test_eq: A.t]
      (Constant.to_affine_exn (scale_constant (one, [|false; false; true|])))
      Constant.(to_affine_exn (one + one + one + one)) ;
    [%test_eq: A.t]
      (Constant.to_affine_exn (scale_constant (one, [|true; false; true|])))
      Constant.(to_affine_exn (one + one + one + one + one)) ;
    let g = Typ.tuple2 Field.typ Field.typ in
    let two_to_the m =
      scale_constant (one, Array.init (m + 1) ~f:(fun i -> i = m))
    in
    [%test_eq: A.t]
      (Constant.to_affine_exn (two_to_the 2))
      Constant.(to_affine_exn (one + one + one + one)) ;
    let n = 4 in
    let bits = Array.init n ~f:(fun _ -> Random.bool ()) in
    Internal_Basic.Test.test_equal
      ~equal:[%eq: Field.Constant.t * Field.Constant.t]
      ~sexp_of_t:[%sexp_of: Field.Constant.t * Field.Constant.t]
      (Typ.tuple2 g (Typ.array ~length:n Boolean.typ))
      g
      (fun (t, bs) ->
        make_checked (fun () -> scale t (`Plus_two_to_len_minus_1 bs)) )
      (fun (t, bs) ->
        let open Constant in
        let t = of_affine t in
        to_affine_exn (scale_constant (t, bs) + two_to_the (n - 1)) )
      (Params.one, bits)
end

let%test_unit "mnt4" =
  let module T = For_native_base_field (struct
    module Impl =
      Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core.Unit)

    module F = struct
      include (
        Impl.Field :
          module type of Impl.Field with module Constant := Impl.Field.Constant )

      module Constant = struct
        include Impl.Field.Constant

        let inv_exn = inv
      end

      let assert_r1cs a b c = Impl.assert_r1cs a b c

      let assert_square a b = Impl.assert_square a b

      let negate x = zero - x

      let inv_exn = inv
    end

    module Params = struct
      let one = Snarky.Backends.Mnt6.G1.(to_affine_exn one)

      include Snarky.Backends.Mnt6.G1.Coefficients

      let group_size_in_bits = Snarky.Backends.Mnt6.Field.size_in_bits
    end

    module Constant = Snarky.Backends.Mnt6.G1
  end) in
  ()
