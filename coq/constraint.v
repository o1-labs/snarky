Require Snarky.Util.
Require Import List.
Import List.ListNotations.
Require Snarky.types.
Import Snarky.types.Constraint.Basic.
Import Snarky.types.Constraint.

Section T.
  Context {V : Type}.

  Definition t := t V.

  Definition create_basic label basic : basic_with_annotation V :=
    {| basic := basic; annotation := label |}.

  Definition override_label x label_opt : basic_with_annotation V :=
    {| basic := basic x
    ; annotation := (match label_opt with Some x => Some x | None => annotation x end) |}.

  Definition equal label x y := [create_basic label (Equal x y)].

  Definition boolean label x := [create_basic label (Boolean x)].

  Definition r1cs label a b c := [create_basic label (R1CS a b c)].

  Definition square label a b := [create_basic label (Square a b)].

  Definition annotation (t : t) :=
    String.concat "; " (Util.List.filter_map annotation t).

End T.