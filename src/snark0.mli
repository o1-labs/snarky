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
   and type Proof.message = Backend.Proof.message
   and type ('a, 's) Checked.t = ('a, 's, Backend.Field.t) Checked.t
   and type ('a, 's) As_prover.t = ('a, Backend.Field.t, 's) As_prover.t

module Make_full
    (Backend : Backend_extended.S)
    (Checked : Checked_intf.Basic with type 'f field := Backend.Field.t)
                                                                       (Runner : sig
        val run :
             ('a, 's, Backend.Field.t) Checked.t
          -> ('s, Backend.Field.t) Run_state.t
          -> ('s, Backend.Field.t) Run_state.t * 'a

        val reduce_to_prover :
             int ref
          -> ('a, 's, Backend.Field.t) Checked.t
          -> ('a, 's, Backend.Field.t) Checked.t
    end)
    (As_prover : As_prover_intf.Basic
                 with type 'f field := Backend.Field.t
                 with module Types := Checked.Types) :
  Snark_intf.S
  with type field = Backend.Field.t
   and type Bigint.t = Backend.Bigint.t
   and type R1CS_constraint_system.t = Backend.R1CS_constraint_system.t
   and type Var.t = Backend.Var.t
   and type Field.Vector.t = Backend.Field.Vector.t
   and type Verification_key.t = Backend.Verification_key.t
   and type Proving_key.t = Backend.Proving_key.t
   and type Proof.t = Backend.Proof.t
   and type Proof.message = Backend.Proof.message
   and type ('a, 's) Checked.t = ('a, 's, Backend.Field.t) Checked.t
   and type ('a, 's) As_prover.t = ('a, Backend.Field.t, 's) As_prover.t

module Checked_runner (Backend : Backend_intf.S) : sig
  module Backend : Backend_extended.S

  module Checked :
    Checked_intf.Basic
    with type ('a, 's, 'f) Types.Checked.t =
                ('s, 'f) Run_state.t -> ('s, 'f) Run_state.t * 'a
     and type ('a, 'f, 's) Types.As_prover.t = ('a, 'f, 's) As_prover0.t

  module Runner : sig
    val run :
         ('a, 's, Backend.Field.t) Checked.t
      -> ('s, Backend.Field.t) Run_state.t
      -> ('s, Backend.Field.t) Run_state.t * 'a

    val reduce_to_prover :
         int ref
      -> ('a, 's, Backend.Field.t) Checked.t
      -> ('a, 's, Backend.Field.t) Checked.t
  end
end

module Make_noast (Backend : Backend_intf.S) :
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
   and type ('a, 's) Checked.t =
                 ('s, Backend.Field.t) Run_state.t
              -> ('s, Backend.Field.t) Run_state.t * 'a
   and type ('a, 's) As_prover.t = ('a, Backend.Field.t, 's) As_prover.t

type ('a, 's, 'f) checked_rtp

module Make_reduce_to_prover (Backend : Backend_intf.S) :
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
   and type ('a, 's) Checked.t =
              int ref -> ('a, 's, Backend.Field.t) checked_rtp
   and type ('a, 's) As_prover.t = ('a, Backend.Field.t, 's) As_prover.t

module Run : sig
  module Make (Backend : Backend_intf.S) :
    Snark_intf.Run
    with type field = Backend.Field.t
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
