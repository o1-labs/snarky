module B = Bigint

module Cond (Impl : sig
  module Boolean : sig
    type var
  end
end) (M : sig
  type t

  val if_ : Impl.Boolean.var -> then_:t -> else_:t -> t
end) : Intf.Cond_intf with type bool := Impl.Boolean.var and type t := M.t =
struct
  open Impl
  open M

  type cond = Boolean.var * t

  let ( -? ) c x : cond = (c, x)

  let ( -: ) (c, x) y = if_ c ~then_:x ~else_:y

  let select b x y = if_ b ~then_:x ~else_:y
end

type proof_system = Groth16 | GrothMaller17

let impl (type f) (curve : f Curve.t) system :
    (module Snarky.Snark_intf.Run
       with type prover_state = unit
        and type field = f) =
  let open Snarky in
  let module M (B : Backend_intf.S) =
    Snark.Run.Make
      (B)
      (struct
        type t = unit
      end)
  in
  let f (module B : Backend_intf.S with type Field.t = f) =
    ( module Snark.Run.Make
               (B)
               (struct
                 type t = unit
               end)
    : Snark_intf.Run
      with type prover_state = unit
       and type field = f )
  in
  let system : (module Backend_intf.S with type Field.t = f) =
    match curve with
    | Bn128 -> (
      match system with
      | Groth16 ->
          (module Backends.Bn128.Default)
      | GrothMaller17 ->
          (module Backends.Bn128.GM) )
  in
  f system

module Make (C : sig
  type field

  val curve : field Curve.t

  val system : proof_system
end)
() : Intf.S with type Impl.field = C.field = struct
  module Impl = (val impl C.curve C.system)

  open Impl

  module Bool = struct
    open Boolean

    module Constant = struct
      type t = bool [@@deriving yojson]
    end

    type t = var

    let typ = typ

    let true_ = true_

    let false_ = false_

    let ( && ) = ( && )

    let ( || ) = ( || )

    let any = any

    let all = all

    let equal = Boolean.equal

    let ( = ) = equal

    let not = not

    let negate = not

    let exactlyOne (bs : t list) =
      Field.equal Field.one
        (Core_kernel.List.reduce_exn ~f:Field.add (bs :> Field.t list))

    let assertEqual (x : t) (y : t) = Field.(Assert.equal (x :> t) (y :> t))

    let assertTrue = assertEqual true_

    let assertFalse = assertEqual false_

    let assertAny = Assert.any

    let assertAll = Assert.all

    let assertExactlyOne = Assert.exactly_one
  end

  module Field = struct
    open Field

    module Checked = struct
      type t = Field.t

      let typ = typ

      let ( = ) = equal

      let equal = ( = )

      let ( * ) = ( * )

      let ( + ) = ( + )

      let ( - ) = ( - )

      let ( / ) = ( / )

      let mul = mul

      let add = add

      let sub = sub

      let div = div

      let negate x = scale x Constant.(negate one)

      let square = square

      let sqrt = sqrt

      let isSquare = is_square

      let sqrtCheck = sqrt_check

      let invert = inv

      let one = one

      let zero = zero

      let ofString x = constant (Constant.of_string x)

      let ofInt = of_int

      let ofBits arr = project (Array.to_list arr)

      let toBits ?length x =
        let length = match length with None -> size_in_bits | Some n -> n in
        Array.of_list (choose_preimage_var ~length x)

      let assertEqual x y = Field.Assert.equal x y

      let assertR1 a b c = assert_r1cs a b c

      let parity = parity

      let size_in_bits = size_in_bits

      include Cond (Impl) (Field)
    end

    include Checked

    module Constant = struct
      open Field.Constant

      let size_in_bits = size_in_bits

      type t = Constant.t

      let to_yojson x = `String (to_string x)

      let of_yojson = function
        | `String s ->
            Ok (of_string s)
        | _ ->
            Error "Field.of_yojson: expected string"

      let ( = ) = equal

      let equal = ( = )

      let ( * ) = ( * )

      let ( + ) = ( + )

      let ( - ) = ( - )

      let ( / ) = ( / )

      let mul = mul

      let add = add

      let sub = sub

      let div = Field.Constant.( / )

      let negate = negate

      let square = square

      let sqrt = sqrt

      let invert = inv

      let one = one

      let zero = zero

      let ofString x = of_string x

      let ofInt = of_int

      let ofBits arr = project (Array.to_list arr)

      let toBits x = Array.of_list (unpack x)

      let toString = to_string

      let parity = parity
    end
  end

  module Hash = struct
    module T =
      Hash.Make
        (Impl)
        (struct
          let curve = Curve.E C.curve
        end)

    include Field.Checked

    let hash = T.hash

    module Constant = struct
      type t = Field.Constant.t [@@deriving yojson, eq]

      let hash = T.Constant.hash
    end
  end

  module MerkleTree = struct
    module Index = struct
      open Core_kernel

      (* LSB first *)
      type t = Boolean.var array

      let to_bits ~depth n =
        let test_bit n i = (n lsr i) land 1 = 1 in
        Array.init depth ~f:(fun i -> test_bit n i)

      let of_bits bs =
        Array.foldi bs ~init:0 ~f:(fun i acc b ->
            if b then acc lor (1 lsl i) else acc )

      let typ ~depth =
        Typ.transport
          (Typ.array ~length:depth Boolean.typ)
          ~there:(to_bits ~depth) ~back:of_bits
    end

    module Path = struct
      type t = Hash.t array

      let typ ~depth = Typ.array ~length:depth Field.typ
    end

    module MembershipProof = struct
      type ('index, 'hash) t_ = ('index, 'hash) Membership_proof.t_ =
        {index: 'index; path: 'hash array}
      [@@deriving yojson]

      type t = (Index.t, Hash.t) t_

      module Constant = struct
        type t = (int, Field.Constant.t) t_ [@@deriving yojson]
      end

      let typ ~depth : (t, Constant.t) Typ.t =
        let open Snarky.H_list in
        let to_hlist {index; path} = [index; path] in
        let of_hlist ([index; path] : (unit, _) t) = {index; path} in
        Typ.of_hlistable
          [Index.typ ~depth; Path.typ ~depth]
          ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
          ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

      let merge ~height:_ l r = Hash.hash [|l; r|]

      let implied_root entry_hash addr0 path0 =
        let n = Array.length addr0 in
        let rec go height acc =
          if height < n then
            let b = addr0.(height) in
            let h = path0.(height) in
            let l = Impl.Field.if_ b ~then_:h ~else_:acc
            and r = Impl.Field.if_ b ~then_:acc ~else_:h in
            let acc' = merge ~height l r in
            go (height + 1) acc'
          else acc
        in
        go 0 entry_hash

      let check {index; path} root elt_hash =
        Field.equal (implied_root elt_hash index path) root
    end

    type 'a t = {hashElt: 'a -> Hash.t; root: Hash.t}

    let ofRoot hashElt root = {hashElt; root}

    module Constant = Merkle_tree_unchecked.Make (Hash.Constant)
  end

  module Integer = struct
    open Snarky_integer.Integer

    type nonrec t = field t

    let m : field Snarky.Snark.m = (module Impl)

    let ofBigint x = constant ~m x

    let ofInt n = ofBigint (B.of_int n)

    let ofString s = ofBigint (B.of_string s)

    let one = ofBigint B.one

    let equal = equal ~m

    let ( = ) = equal

    let ( <= ) = lte ~m

    let ( < ) = lt ~m

    let ( >= ) = gte ~m

    let ( > ) = gt ~m

    let toBits ?length t =
      Bitstring_lib.Bitstring.Lsb_first.to_array (to_bits ?length ~m t)

    let ofBits bits =
      of_bits ~m
        (Bitstring_lib.Bitstring.Lsb_first.of_list (Array.to_list bits))

    let toField = to_field

    let add = add ~m

    let mul = mul ~m

    let ( + ) = add

    let ( * ) = mul

    let divMod = div_mod ~m
  end

  module InputSpec = Input_spec.Make (Impl)

  let runMain = InputSpec.run_main

  module Group = Group.Make (C) (Impl) ()

  module Schnorr = struct
    module Scalar = struct
      include Group.Scalar

      module Constant = struct
        include Group.Constant.Scalar
      end

      let typ =
        Typ.transport
          (Typ.list ~length:Constant.size_in_bits Boolean.typ)
          ~there:Constant.to_bits ~back:Constant.of_bits
        |> Typ.transport_var ~there:Bitstring_lib.Bitstring.Lsb_first.to_list
             ~back:Bitstring_lib.Bitstring.Lsb_first.of_list
    end

    module T = struct
      include Snarky_signature.Signature.Make0 (struct
        module Bool = Bool
        module Hash = Hash
        module Scalar = Group.Scalar
        module Group = Group

        module Field = struct
          include Impl.Field

          let is_even y = Bool.not (parity y)
        end
      end)
    end

    module Signature = struct
      include T.Signature

      let check = T.check

      module Constant = struct
        type t = Field.Constant.t * Scalar.Constant.t [@@deriving yojson]
      end

      let typ = Typ.tuple2 Field.typ Scalar.typ
    end

    module PublicKey = struct
      include T.Public_key

      let ofPrivateKey = of_private_key

      module Constant = Group.Constant

      let typ = Group.typ
    end

    module PrivateKey = struct
      include T.Private_key

      module Constant = struct
        include Scalar.Constant
      end
    end

    module Constant = struct
      module Signer = Snarky_signature.Signature.Make_signer (struct
        module Hash = Hash.Constant
        module Group = Group.Constant
        module Scalar = Scalar.Constant

        module Field = struct
          include Field.Constant

          let is_even t = not (parity t)
        end

        module Bool = struct
          type t = bool

          let ( && ) = ( && )
        end
      end)

      let sign = Signer.sign

      let check = Signer.check
    end
  end
end

let create (type f) (curve : f Curve.t) system =
  let module M =
    Make (struct
        type field = f

        let curve = curve

        let system = system
      end)
      ()
  in
  (module M : Intf.S with type Impl.field = f)

let default () = create Bn128 Groth16

module Bn128 = Make (struct
  type field = Snarky.Backends.Bn128.Field.t

  let curve = Curve.Bn128

  let system = Groth16
end)
