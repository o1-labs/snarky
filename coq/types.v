Require Import String.

Module Constraint.
  Module Basic.
    Inductive t var : Type :=
      | Boolean : var -> t var
      | Equal : var -> var -> t var
      | Square : var -> var -> t var
      | R1CS : var -> var -> var -> t var.
  End Basic.

  Record basic_with_annotation var : Type := {
    basic : Basic.t var; annotation : option string
  }.

  Definition t var := list (basic_with_annotation var).
End Constraint.

Module As_prover.
  Definition t f v s a := (v -> f) -> s -> s * a.
End As_prover.

Parameter request_handler : Type.

Parameter provider : forall (a env s : Type), Type.

Parameter handle : forall (var value : Type), Type.

Module Store.
  Inductive t (var f v : Type) := Store : f -> (v -> var) -> t var f v.
End Store.

Module Read.
  Inductive t (value f v : Type) := Read : v -> (f -> value) -> t value f v.
End Read.

Module Alloc.
  Inductive t (var v : Type) := Alloc : (v -> var) -> t var v.
End Alloc.

Module Types.
  Inductive checked (f : Type) (v : Type) (sys : Type) :
    forall (a s : Type), Type :=
    | Pure a s : a -> checked f v sys a s
    | Add_constraint a s : Constraint.t v -> checked f v sys a s
    | As_prover a s :
        As_prover.t f v sys unit ->
        checked f v sys a s ->
        checked f v sys a s
    | With_label a b s :
        string ->
        checked f v sys a s ->
        (a -> checked f v sys b s) ->
        checked f v sys b s
    | With_state a b s1 s2 :
        As_prover.t f v s1 s2 ->
        (s2 -> As_prover.t f v s1 unit) ->
        checked b s2 f v sys ->
        (b -> checked f v sys a s1) ->
        checked f v sys a s1
    | With_handler a b s :
        request_handler ->
        checked f v sys a s ->
        (a -> checked f v sys b s) ->
        checked f v sys b s
    | Clear_handler a b s :
      checked f v sys a s ->
      (a -> checked f v sys b s) ->
      checked f v sys b s
    | Exists a s var value :
      typ f v sys var value ->
      provider value (v -> f) s ->
      (handle var value -> checked f v sys a s) ->
      checked f v sys a s
    | Next_auxiliary a s :
      (nat -> checked f v sys a s) -> checked f v sys a s

    with typ (f : Type) (v : Type) (sys : Type) :
      forall (var : Type) (value : Type), Type :=
    | Mk_typ var value :
      forall (store : value -> Store.t var f v)
      (read : var -> Read.t value f v)
      (alloc : Alloc.t var v)
      (check : var -> checked f v sys unit unit),
        typ f v sys var value.
End Types.

Module Checked.
  Definition t f v sys a s := Types.checked f v sys a s.
End Checked.

Module Typ.
  Definition t f v sys var value := Types.typ f v sys var value.
End Typ.