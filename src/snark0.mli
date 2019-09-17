val set_eval_constraints : bool -> unit
(** Sets the [eval_constraints] state. If [true], {!val:run_unchecked} and
    {!val:prove} will check that the constraint system is satisfied while
    evaluating the {!type:Checked.t}. The default value is [false].

    Note: This flag will not evaluate any constraints added by
    {!val:with_constraint_system} (or the underlying
    {!const:Types.Checked.With_constraint_system}). For these, you should
    modify your code to use the normal {!val:run_and_check} function. *)

val set_reduce_to_prover : bool -> unit
(** Sets the [reduce_to_prover] state. If [true], the [Proof_system] interface
    will run optimised versions of the checked computations whenever possible.

    Note: This optimisation pre-evaluates and caches some parts of the checked
    computation, to speed up subsequent calls. *DO NOT USE* if your checked
    computation uses mutability outside of [As_prover] blocks. *)

(** The exception raised when evaluating a checked computation raises an
    exception.

    The arguments are:
    1. a generic error message, with a presentation of the backtrace
    2. the backtrace of all active labels at the point in the checked
       computation when the exception was raised
    3. the original exception that was raised
    4. the backtrace of the original exception
*)
exception Runtime_error of string * string list * exn * string

module Make (Backend : Backend_intf.S) :
  Snark_intf.S
  with type field = Backend.Field.t
   and type Bigint.t = Backend.Bigint.R.t
   and type R1CS_constraint_system.t = Backend.R1CS_constraint_system.t
   and type Var.t = Backend.Var.t
   and type Field.Vector.t = Backend.Field.Vector.t
   and type Verification_key.t = Backend.Verification_key.t
   and type Proving_key.t = Backend.Proving_key.t
   and type Proof.t = Backend.Proof.t
   and type Proof.message = Backend.Proof.message

module Run : sig
  val throw_on_id : int -> unit
  (** [throw_on_id id] set an internal flag that causes [Make] to throw an
      error if its internal id would be the same as [id].

      This can be used to identify where different instances come from, so that
      the same instance can be used for creating and calling functions.
  *)

  module Make
      (Backend : Backend_intf.S) (Prover_state : sig
          type t
      end) :
    Snark_intf.Run
    with type field = Backend.Field.t
     and type prover_state = Prover_state.t
     and type Bigint.t = Backend.Bigint.R.t
     and type R1CS_constraint_system.t = Backend.R1CS_constraint_system.t
     and type Var.t = Backend.Var.t
     and type Field.Constant.Vector.t = Backend.Field.Vector.t
     and type Verification_key.t = Backend.Verification_key.t
     and type Proving_key.t = Backend.Proving_key.t
     and type Proof.t = Backend.Proof.t
     and type Proof.message = Backend.Proof.message
end

type 'field m = (module Snark_intf.Run with type field = 'field)

val make : (module Backend_intf.S with type Field.t = 'field) -> 'field m

type ('prover_state, 'field) m' =
  (module Snark_intf.Run
     with type field = 'field
      and type prover_state = 'prover_state)

val make' :
     (module Backend_intf.S with type Field.t = 'field)
  -> ('prover_state, 'field) m'

val ignore_state : ('prover_state, 'field) m' -> 'field m
