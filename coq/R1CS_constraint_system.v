Require Import String.

Module Md5.
  Definition t := string.
End Md5.

Section R1CS_constraint_system.
  Variable (field : Type).
  Variable (constraint : Type).
  Variable (sys : Type).

  Record t :={primary_input_size : nat;
            auxiliary_input_size : nat;
            constraints : sys}.

  Class Create :={create : unit -> t}.

  Class Add_constraint_aux :={add_constraint_aux : sys -> constraint -> sys}.

  Definition add_constraint (x : t) (c : constraint) `{Add_constraint_aux} :=
  {|
  primary_input_size := primary_input_size x;
  auxiliary_input_size := auxiliary_input_size x;
  constraints := add_constraint_aux (constraints x) c |}.

  Class Add_constraint_with_annotation_aux :={add_constraint_with_annotation_aux :
                                             sys ->
                                             constraint -> string -> sys}.

  Definition add_constraint_with_annotation
  `{Add_constraint_with_annotation_aux} (x : t) (c : constraint)
  (s : string) :=
  {|
  primary_input_size := primary_input_size x;
  auxiliary_input_size := auxiliary_input_size x;
  constraints := add_constraint_with_annotation_aux (constraints x) c s |}.

  Definition set_primary_input_size (x : t) (n : nat) :=
  {|
  primary_input_size := n;
  auxiliary_input_size := auxiliary_input_size x;
  constraints := constraints x |}.

  Definition get_primary_input_size := primary_input_size.

  Definition set_secondary_input_size (x : t) (n : nat) :=
  {|
  primary_input_size := primary_input_size x;
  auxiliary_input_size := n;
  constraints := constraints x |}.

  Definition get_auxiliary_input_size := auxiliary_input_size.

  Class Check_aux :={check_aux : sys -> bool}.

  Definition check `{Check_aux} (x : t) := check_aux (constraints x).

  Class Is_satisfied_aux :={is_satisfied_aux :
                           sys -> list field -> list field -> bool}.

  Definition is_satisfied `{Is_satisfied_aux} (x : t)
  (primary secondary : list field) :=
  is_satisfied_aux (constraints x) primary secondary.

  Class Digest_aux :={digest_aux : sys -> Md5.t}.

  Definition digest `{Digest_aux} (x : t) := digest_aux (constraints x).

End R1CS_constraint_system.

Arguments add_constraint_with_annotation {constraint} {sys} {H}.

Module Class.
  Class R1CS_constraint_system field constraint sys :={system_create :>
                                                      Create sys;
                                                     system_add_constraint :>
                                                      Add_constraint_aux
                                                        constraint sys;
                                                     system_add_constraint_with_annotation :>
                                                      Add_constraint_with_annotation_aux
                                                        constraint sys;
                                                     system_check :>
                                                      Check_aux sys;
                                                     system_is_satisfied :>
                                                      Is_satisfied_aux field
                                                        sys;
                                                     system_digest_aux :>
                                                      Digest_aux sys}.
End Class.
Include Class.
