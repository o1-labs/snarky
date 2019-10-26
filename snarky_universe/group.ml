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

  module Str = struct
    type t = string [@@deriving yojson]
  end

  module Nat = struct
    (* It's sort of an abuse to use this, but it is a bigint of the appropriate size. *)
    include B.Bigint

    let num_bits _ = 254

    let op f x y =
      B.Bigint.(of_bignum_bigint (f (to_bignum_bigint x) (to_bignum_bigint y)))

    let op1 f x y = B.Bigint.(of_bignum_bigint (f (to_bignum_bigint x) y))

    let fn f x = B.Bigint.(f (to_bignum_bigint x))

    let nf f y = B.Bigint.(of_bignum_bigint (f y))

    let ( + ) = op Bigint.( + )

    let ( * ) = op Bigint.( * )

    let ( - ) = op Bigint.( - )

    let ( // ) = op Bigint.( / )

    let ( % ) = op Bigint.( % )

    let log_or = op Bigint.( lor )

    let log_and = op Bigint.( land )

    let shift_left = op1 Bigint.shift_left

    let shift_right = op1 Bigint.shift_right

    let ( < ) x y = compare x y < 0

    let equal x y = compare x y = 0

    module T = struct
      let to_string = fn Bigint.to_string

      let of_string = nf Bigint.of_string
    end

    include T

    include Core_kernel.Sexpable.Of_stringable (struct
      type nonrec t = t

      include T
    end)

    let to_int_exn = fn Bigint.to_int_exn

    let of_int = nf Bigint.of_int

    let to_yojson x = Str.to_yojson (to_string x)

    let of_yojson s = Core_kernel.Result.map (Str.of_yojson s) ~f:of_string
  end

  module Params = struct
    let a = B.Field.of_string bn128.a

    let b = B.Field.of_string bn128.b
  end

  include Snarkette.Elliptic_curve.Make
            (Nat)
            (struct
              include B.Field

              let to_yojson x = Str.to_yojson (to_string x)

              let of_yojson s =
                Core_kernel.Result.map (Str.of_yojson s) ~f:of_string
            end)
            (Params)

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

  type field = B.Field.t
end

module type Constant_intf = sig
  type t [@@deriving yojson]

  include Snarky_curve.Constant_intf with type t := t
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
