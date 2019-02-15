val set_eval_constraints : bool -> unit
(** Sets the [eval_constraints] state. If [true], {!val:run_unchecked} and
    {!val:prove} will check that the constraint system is satisfied while
    evaluating the {!type:Checked.t}. The default value is [false].

    Note: This flag will not evaluate any constraints added by
    {!val:with_constraint_system} (or the underlying
    {!const:Types.Checked.With_constraint_system}). For these, you should
    modify your code to use the normal {!val:run_and_check} function. *)

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
