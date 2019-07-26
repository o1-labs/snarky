module type S = sig
  type (_, _) checked

  type (_, _) typ

  type field

  type field_var

  type bool_var

  type bitstring

  (** The type of unpacked field variables.

      The field variable is represented internally in the R1CS as a sequence of
      bit variables, so that it can be converted to either a bitstring or a
      field variable multiple times with no extra cost.
  *)
  type t

  val typ : (t, field) typ
  (** Convert between field element values and R1CS unpacked field variables.

      Storing a field element requires 1 constraint for each bit in the field size.
  *)

  val constant : field -> t
  (** The unpacked field representing the given field element. *)

  (** Caution: Passing the bit representation of a number larger than the
      maximum field element to any of these functions will result in a field
      element distinct from the number represented.
  *)
  module Unsafe : sig
    val of_bits : bool list -> t
    (** The unpacked field element representing the given bits.
        Raises [AssertionError] if the number of bits is greater than the
        number of bits in the field.
    *)

    val of_bitstring : bitstring -> t
    (** The unpacked field representing the given bitstring.
        Raises [AssertionError] if the number of bits is greater than the
        number of bits in the field.
    *)
  end

  val to_bitstring : t -> bitstring
  (** Convert an unpacked field to a bitstring. *)

  val unpack : field_var -> (t, _) checked
  (** Convert a field variable into an unpacked field variable.
      Requires 1 constraint for each bit in the field size.
  *)

  val pack : t -> field_var
  (** Convert an unpacked field variable into a field variable. *)

  val if_ : bool_var -> then_:t -> else_:t -> (t, _) checked
  (** [if_ b ~then_ ~else_] returns [then_] if [b] is true, or [else_]
      otherwise.
  *)
end

open Core_kernel

module Make (Snark : Snark_intf.Basic) :
  S
  with type ('a, 's) checked := ('a, 's) Snark.Checked.t
   and type ('var, 'value) typ := ('var, 'value) Snark.Typ.t
   and type field := Snark.Field.t
   and type field_var := Snark.Field.Var.t
   and type bool_var := Snark.Boolean.var
   and type bitstring := Snark.Bitstring_checked.t = struct
  open Snark

  type t = Bitstring_checked.t

  let typ : (t, Field.t) Typ.t =
    Typ.transport
      (Typ.list ~length:Field.size_in_bits Boolean.typ)
      ~there:Field.unpack ~back:Field.project

  let constant x = List.map ~f:Boolean.var_of_value (Field.unpack x)

  let rec pad n x = if n > 0 then pad (n - 1) (Boolean.false_ :: x) else x

  module Unsafe = struct
    let of_bits l =
      let pad_size = Field.size_in_bits - List.length l in
      assert (pad_size >= 0) ;
      pad pad_size (List.map ~f:Boolean.var_of_value l)

    let of_bitstring l =
      let pad_size = Field.size_in_bits - List.length l in
      assert (pad_size >= 0) ;
      pad pad_size l
  end

  let to_bitstring l =
    l

  let unpack f =
    let open Checked in
    let%map x = Field.Checked.unpack_full f in
    (Bitstring_lib.Bitstring.Msb_first.of_lsb_first x :> Bitstring_checked.t)

  let pack x = Field.Var.project x

  (* TODO: We could do this bit-by-bit to save a constraint. *)
  let if_ b ~then_ ~else_ =
    let%bind res =
      Field.Checked.if_ b ~then_:(pack then_) ~else_:(pack else_)
    in
    unpack res
end

module Run = struct
  module type S = sig
    type (_, _) typ

    type field

    type field_var

    type bool_var

    type bitstring

    (** The type of unpacked field variables.

      The field variable is represented internally in the R1CS as a sequence of
      bit variables, so that it can be converted to either a bitstring or a
      field variable multiple times with no extra cost.
  *)
    type t

    val typ : (t, field) typ
    (** Convert between field element values and R1CS unpacked field variables.

        Storing a field element requires 1 constraint for each bit in the field
        size.
    *)

    val constant : field -> t
    (** The unpacked field representing the given field element. *)

    (** Caution: Passing the bit representation of a number larger than the
        maximum field element to any of these functions will result in a field
        element distinct from the number represented.
    *)
    module Unsafe : sig
      val of_bits : bool list -> t
      (** The unpacked field element representing the given bits.
          Raises [AssertionError] if the number of bits is greater than the
          number of bits in the field.
      *)

      val of_bitstring : bitstring -> t
      (** The unpacked field representing the given bitstring.
          Raises [AssertionError] if the number of bits is greater than the
          number of bits in the field.
      *)
    end

    val to_bitstring : t -> bitstring
    (** Convert an unpacked field to a bitstring. *)

    val unpack : field_var -> t
    (** Convert a field variable into an unpacked field variable.
        Requires 1 constraint for each bit in the field size.
    *)

    val pack : t -> field_var
    (** Convert an unpacked field variable into a field variable. *)

    val if_ : bool_var -> then_:t -> else_:t -> t
    (** [if_ b ~then_ ~else_] returns [then_] if [b] is true, or [else_]
        otherwise.
    *)
  end

  module Make (Snark : Snark_intf.Run_basic) :
    S
    with type ('var, 'value) typ := ('var, 'value) Snark.Typ.t
     and type field := Snark.field
     and type field_var := Snark.Field.t
     and type bool_var := Snark.Boolean.var
     and type bitstring := Snark.Bitstring_checked.t = struct
    include Make (Snark.Internal_Basic)

    let unpack f = Snark.run_checked (unpack f)

    let if_ b ~then_ ~else_ = Snark.run_checked (if_ b ~then_ ~else_)
  end
end
