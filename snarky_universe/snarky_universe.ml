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
end

let impl_of_curve = function
  | Curve.Bn128 ->
      let module Backend = Snarky.Backends.Bn128.Default in
      let module Run =
        Snarky.Snark.Run.Make
          (Backend)
          (struct
            type t = unit
          end)
      in
      (module Run : Snarky.Snark_intf.Run with type prover_state = unit)

module Make (C : sig
  val curve : Curve.t
end)
() : Intf.S = struct
  module Impl = (val impl_of_curve C.curve)

  open Impl

  module Bool = struct
    open Boolean

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

    let exactlyOne (bs : t list) =
      Field.equal Field.one
        (Core_kernel.List.reduce_exn ~f:Field.add (bs :> Field.t list))

    let assertEqual (x : t) (y : t) = Field.(Assert.equal (x :> t) (y :> t))

    let assertAny = Assert.any

    let assertAll = Assert.all

    let assertExactlyOne = Assert.exactly_one
  end

  module Field = struct
    open Field

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

    let sqrt x =
      let y =
        exists typ ~compute:(fun () -> Constant.sqrt (As_prover.read_var x))
      in
      assert_square y x ; y

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

    include Cond (Impl) (Field)

    module Constant = struct
      open Field.Constant

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
    end
  end

  module Hash = Hash.Make (Impl) (C)

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
      type ('index, 'hash) t_ = {index: 'index; path: 'hash array}
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
        Field.assertEqual (implied_root elt_hash index path) root
    end

    type 'a t = {hashElt: 'a -> Hash.t; root: Hash.t}

    let ofRoot hashElt root = {hashElt; root}
  end
end

module Bn128 = Make (struct
  let curve = Curve.Bn128
end)
