(** Sets the [eval_constraints] state. If [true], {!val:run_unchecked} and
    {!val:prove} will check that the constraint system is satisfied while
    evaluating the {!type:Checked.t}. The default value is [false].

    Note: This flag will not evaluate any constraints added by
    {!val:with_constraint_system} (or the underlying
    {!const:Types.Checked.With_constraint_system}). For these, you should
    modify your code to use the normal {!val:run_and_check} function. *)
val set_eval_constraints : bool -> unit

(** The exception raised when evaluating a checked computation raises an
    exception.

    The arguments are:
    1. the backtrace of all active labels at the point in the checked
       computation when the exception was raised
    2. the original exception that was raised
    3. the backtrace of the original exception
*)
exception Runtime_error of string list * exn * string

module Make (Backend : Backend_intf.Input_intf) :
  Snark_intf.S
    with type field = Backend.Field.t
     and type field_var = Backend.Cvar.t
     and type Bigint.t = Backend.Bigint.t
     and type R1CS_constraint_system.t = Backend.R1CS_constraint_system.t
     and type Field.Vector.t = Backend.Field.Vector.t
     and type Constraint.t = Backend.Constraint.t

module Run : sig
  module Make (Backend : Backend_intf.Input_intf) :
    Snark_intf.Run
      with type field = Backend.Field.t
       and type field_var = Backend.Cvar.t
       and type Bigint.t = Backend.Bigint.t
       and type R1CS_constraint_system.t = Backend.R1CS_constraint_system.t
       and type Field.Constant.Vector.t = Backend.Field.Vector.t
       and type Constraint.t = Backend.Constraint.t
end

type ('field, 'field_var) m =
  (module Snark_intf.Run
     with type field = 'field
      and type field_var = 'field_var )