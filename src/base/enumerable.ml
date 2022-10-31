open Core_kernel

let int_to_bits ~length n =
  let ith_bit i = (n lsr i) land 1 = 1 in
  List.init length ~f:ith_bit

let int_of_bits bs =
  List.foldi bs ~init:0 ~f:(fun i acc b -> if b then acc + (1 lsl i) else acc)

module Make
    (Impl : Snark_intf.Basic) (M : sig
      type t [@@deriving enum]
    end) =
struct
  open Impl

  (* TODO: Make this throw when the elt is too big *)
  let field_to_int x = int_of_bits (List.take (Field.unpack x) 62)

  open M

  let _unused = M.min (* generate unused variable error *)

  let bit_length =
    let n = Int.ceil_log2 (M.max + 1) in
    assert (n < Field.size_in_bits) ;
    n

  type var = Field.Var.t

  let to_field t = Field.of_int (to_enum t)

  let of_field x = Option.value_exn (of_enum (field_to_int x))

  let assert_equal x y = Field.Checked.Assert.equal x y

  let typ : (var, t) Typ.t =
    let check =
      if M.max = 1 then fun x -> assert_ (Constraint.boolean x)
      else fun x ->
        Field.Checked.Assert.lte ~bit_length x
          (Field.Var.constant (Field.of_int M.max))
    in
    let (Typ typ) = Typ.transport Field.typ ~there:to_field ~back:of_field in
    Typ { typ with check = (fun x -> make_checked_ast (check x)) }

  let var_to_bits : var -> Boolean.var list Checked.t =
    Field.Checked.unpack ~length:bit_length

  let to_bits t = int_to_bits ~length:bit_length (to_enum t)

  let if_ b ~(then_ : var) ~(else_ : var) = Field.Checked.if_ b ~then_ ~else_

  let var t : var = Field.Var.constant (to_field t)

  let ( = ) = Field.Checked.equal
end

module Run = struct
  module Make
      (Impl : Snark_intf.Run_basic) (M : sig
        type t [@@deriving enum]
      end) =
  struct
    open Impl
    include Make (Impl.Internal_Basic) (M)

    let assert_equal x y = run_checked (assert_equal x y)

    let var_to_bits x = run_checked (var_to_bits x)

    let if_ x ~then_ ~else_ = run_checked (if_ x ~then_ ~else_)

    let ( = ) x y = run_checked (x = y)
  end
end
