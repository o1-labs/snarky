type 'a params = {a: 'a; b: 'a; one: 'a * 'a}

let bn128 =
  { a=
      "7296080957279758407415468581752425029516121466805344781232734728849116493472"
  ; b=
      "16213513238399463127589930181672055621146936592900766180517188641980520820846"
  ; one=
      ( "6142832666126200108502958025802235821181485456421382423166796358396261897243"
      , "11248300941377020445838278434488928391563663598577146687858078672191633663164"
      ) }

let params_of_curve : type f. f Curve.t -> string params = function
  | Bn128 ->
      bn128

module Bn128 = struct
  module B = Snarky.Backend_extended.Make (Snarky.Backends.Bn128.Default)

  type field = B.Field.t

  module Str = struct
    type t = string [@@deriving yojson]
  end

  module Scalar = struct
    module T =
      Snarkette.Fields.Make_fp
        (Snarkette.Nat)
        (struct
          let order =
            Snarkette.Nat.of_string
              "2736030358979909402780800718157159386076813972158567259200215660948447373041"
        end)

    let size_in_bits = 251

    include T

    let num_bits (t : t) = Snarkette.Nat.num_bits (t :> Nat.t)

    let test_bit (t : t) i = Snarkette.Nat.test_bit (t :> Nat.t) i

    let to_bits t = List.init size_in_bits (test_bit t)

    let of_bits bits =
      let _, res =
        let nat1 = Snarkette.Nat.of_int 1 in
        List.fold_left
          (fun (i, acc) b ->
            let acc =
              if b then Snarkette.Nat.(log_or acc (shift_left nat1 i)) else acc
            in
            (Core_kernel.Int.(i + 1), acc) )
          (0, Snarkette.Nat.of_int 0)
          bits
      in
      of_bigint res

    let to_field (t : t) = B.Field.of_string (to_string t)

    let of_field (t : B.Field.t) =
      B.Field.to_string t |> Snarkette.Nat.of_string |> of_bigint
  end

  module Params = struct
    let a = B.Field.of_string bn128.a

    let b = B.Field.of_string bn128.b
  end

  include Snarkette.Elliptic_curve.Make
            (Scalar)
            (struct
              include B.Field

              let to_yojson x = Str.to_yojson (to_string x)

              let of_yojson s =
                Core_kernel.Result.map (Str.of_yojson s) ~f:of_string
            end)
            (Params)

  let one = of_affine (Tuple_lib.Double.map bn128.one ~f:B.Field.of_string)

  let scale ?init t s =
    let p = scale t s in
    match init with None -> p | Some init -> p + init

  let add_exn = ( + )

  let random () =
    (* y^2 = x^3 + ax + b *)
    let rec go x =
      let open B.Field in
      let y2 =
        let res = square x in
        res += Params.a ;
        (* x^2 + a *)
        res *= x ;
        (* x^3 + ax *)
        res += Params.b ;
        (* x^3 + ax + b *)
        res
      in
      if is_square y2 then of_affine (x, sqrt y2) else go (x + one)
    in
    go (B.Field.random ())
end

module type Constant_intf = sig
  type t [@@deriving yojson]

  include Snarky_curve.Constant_intf with type t := t

  val negate : t -> t

  val one : t

  val add_exn : t -> t -> t

  module Scalar : sig
    type t [@@deriving yojson]

    val size_in_bits : int

    val to_field : t -> field

    val of_field : field -> t

    val ( + ) : t -> t -> t

    val ( * ) : t -> t -> t

    val negate : t -> t

    val to_bits : t -> bool list

    val of_bits : bool list -> t
  end

  val scale : ?init:t -> t -> Scalar.t -> t
end

let constant (type f) (c : f Curve.t) :
    (module Constant_intf with type field = f) =
  match c with Bn128 -> (module Bn128)

(* let constant_of_curve =  *)

module Make (C : sig
  type field

  val curve : field Curve.t
end)
(Impl : Snarky.Snark_intf.Run
        with type prover_state = unit
         and type field = C.field)
() =
struct
  let {a; b; one} = params_of_curve C.curve

  module Constant = (val constant C.curve)

  module Inputs = struct
    module Impl = Impl
    module Constant = Constant

    module F = struct
      module T = Impl.Field

      type t = T.t

      let zero = T.zero

      let negate x = T.scale x T.Constant.(negate one)

      let assert_square x y = Impl.assert_square x y

      let assert_r1cs x y = Impl.assert_r1cs x y

      let inv_exn = T.inv

      let scale = T.scale

      (* TODO: Check the cofactor! *)
      let typ = T.typ

      let square = T.square

      let constant = T.constant

      let if_ = T.if_

      let ( - ) = T.( - )

      let ( + ) = T.( + )

      let ( * ) = T.( * )

      module Constant = struct
        include T.Constant

        let inv_exn = inv
      end
    end

    module Params = struct
      let a = F.Constant.of_string a

      let b = F.Constant.of_string b

      let one = Tuple_lib.Double.map one ~f:F.Constant.of_string
    end
  end

  include Snarky_curve.Make_checked (Inputs)
end
