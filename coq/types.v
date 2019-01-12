Require Import String.
Require Import Snarky.typ_monads.

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

Module Cvar.
  Inductive t f v :=
  | Constant : f -> t f v
  | Var : v -> t f v
  | Add : t f v -> t f v -> t f v
  | Scale : f -> t f v -> t f v.
End Cvar.

Module As_prover.
  Definition t f v s a := (v -> f) -> s -> s * a.
End As_prover.

Parameter request_handler : Type.

Parameter provider : forall (a env s : Type), Type.

Parameter handle : forall (var value : Type), Type.

Module Types.
  Section type.
    Polymorphic Variables f v sys : Type.

    Polymorphic Inductive checked : forall (s a : Type), Type :=
      | Pure a s : a -> checked s a
      | Add_constraint a s : Constraint.t v -> checked s a -> checked s a
      | As_prover a s :
          As_prover.t f v sys unit ->
          checked s a ->
          checked s a
      | With_label a b s :
          string ->
          checked s a ->
          (a -> checked s b) ->
          checked s b
      | With_state a b s1 s2 :
          As_prover.t f v s1 s2 ->
          (s2 -> As_prover.t f v s1 unit) ->
          checked s2 b ->
          (b -> checked s1 a) ->
          checked s1 a
      | With_handler a b s :
          request_handler ->
          checked s a ->
          (a -> checked s b) ->
          checked s b
      | Clear_handler a b s :
        checked s a ->
        (a -> checked s b) ->
        checked s b
      | Exists a s var value :
        typ var value ->
        provider value (v -> f) s ->
        (handle var value -> checked s a) ->
        checked s a
      | Next_auxiliary a s :
        (nat -> checked s a) -> checked s a
  
      with typ : forall (var : Type) (value : Type), Type :=
      | Mk_typ var value :
        forall (store : value -> Store.t f v var)
        (read : var -> Read.t f v value)
        (alloc : Alloc.t v var)
        (check : var -> checked unit unit),
          typ var value.
  End type.

  Arguments Pure {f v sys a s}.
  Arguments Add_constraint {f v sys a s}.
  Arguments As_prover {f v sys a s}.
  Arguments With_label {f v sys a b s}.
  Arguments With_state {f v sys a b s1 s2}.
  Arguments With_handler {f v sys a b s}.
  Arguments Clear_handler {f v sys a b s}.
  Arguments Exists {f v sys a s var value}.
  Arguments Next_auxiliary {f v sys a s}.
  Arguments Add_constraint {f v sys a s}.
End Types.

Module Checked.
  Definition t f v sys a s := Types.checked f v sys a s.
End Checked.

Module Typ.
  Definition t f v sys var value := Types.typ f v sys var value.

  Definition store {f v sys var value} (t : t f v sys var value) : value -> Store.t f v var :=
    match t in Types.typ _ _ _ var value return value -> Store.t f v var with
    | Types.Mk_typ _ _ _ _ _ store read alloc check => store
    end.

  Definition read {f v sys var value} (t : t f v sys var value) : var -> Read.t f v value :=
    match t in Types.typ _ _ _ var value return var -> Read.t f v value with
    | Types.Mk_typ _ _ _ _ _ store read alloc check => read
    end.

  Definition alloc {f v sys var value} (t : t f v sys var value) : Alloc.t v var :=
    match t in Types.typ _ _ _ var value return Alloc.t v var with
    | Types.Mk_typ _ _ _ _ _ store read alloc check => alloc
    end.

  Definition check {f v sys var value} (t : t f v sys var value) : var -> Checked.t f v sys unit unit :=
    match t in Types.typ _ _ _ var value return var -> Checked.t f v sys unit unit with
    | Types.Mk_typ _ _ _ _ _ store read alloc check => check
    end.
End Typ.