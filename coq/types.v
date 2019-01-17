Require Import String.
Require Import Snarky.typ_monads.
Require Snarky.request.
Require Utils.

Module Constraint.
  Module Basic.
    Inductive t var : Type :=
  | Boolean : var -> t var
  | Equal : var -> var -> t var
  | Square : var -> var -> t var
  | R1CS : var -> var -> var -> t var.


    Arguments Boolean {var}.
    Arguments Equal {var}.
    Arguments Square {var}.
    Arguments R1CS {var}.
  End Basic.

  Record basic_with_annotation var : Type := {
    basic : Basic.t var; annot : option string
  }.

  Arguments basic {var}.
  Arguments annot {var}.

  Definition t var := list (basic_with_annotation var).
End Constraint.

Module Cvar.
  Inductive t f v :=
  | Constant : f -> t f v
  | Var : v -> t f v
  | Add : t f v -> t f v -> t f v
  | Scale : f -> t f v -> t f v.

  Arguments Constant {f} {v}.
  Arguments Var {f} {v}.
  Arguments Add {f} {v}.
  Arguments Scale {f} {v}.
End Cvar.

Module As_prover.
  Definition t f v s a := (v -> f) -> s -> s * a.
End As_prover.

Module Provider.
  Inductive t F V S request A :=
  | Request : As_prover.t F V S (request A) -> t F V S request A
  | Compute : As_prover.t F V S A -> t F V S request A
  | Both :
      As_prover.t F V S (request A) ->
      As_prover.t F V S A -> t F V S request A.

  Arguments Request {F} {V} {S} {request} {A}.
  Arguments Compute {F} {V} {S} {request} {A}.
  Arguments Both {F} {V} {S} {request} {A}.
End Provider.

Module Handle.

  Inductive t var value :={var : var; value : option value}.

End Handle.

Module Types.
  Section type.
    Polymorphic Variables (f v sys : Type).
    Polymorphic Variable (request : Type -> Type).

    Polymorphic
Inductive checked : forall s a : Type, Type :=
  | Pure : forall a s, a -> checked s a
  | Add_constraint : forall a s, Constraint.t v -> checked s a -> checked s a
  | With_constraint_system :
      forall a s, (sys -> sys) -> checked s a -> checked s a
  | As_prover :
      forall a s, As_prover.t f v s unit -> checked s a -> checked s a
  | With_label :
      forall a b s,
      string -> checked s a -> (a -> checked s b) -> checked s b
  | With_state :
      forall a b s1 s2,
      As_prover.t f v s1 s2 ->
      (s2 -> As_prover.t f v s1 unit) ->
      checked s2 b -> (b -> checked s1 a) -> checked s1 a
  | With_handler :
      forall a b s,
      request.Handler.single request ->
      checked s a -> (a -> checked s b) -> checked s b
  | Clear_handler :
      forall a b s, checked s a -> (a -> checked s b) -> checked s b
  | Exists :
      forall a s var value,
      typ var value ->
      Provider.t f v s request value ->
      (Handle.t var value -> checked s a) -> checked s a
  | Next_auxiliary : forall a s, (nat -> checked s a) -> checked s a
with typ : forall var value : Type, Type :=
    Mk_typ :
      forall var value (store : value -> Store.t f v var)
        (read : var -> Read.t f v value) (alloc : Alloc.t v var)
        (check : var -> checked unit unit), typ var value.
  End type.

  Arguments Pure {f} {v} {sys} {request} {a} {s}.
  Arguments Add_constraint {f} {v} {sys} {request} {a} {s}.
  Arguments With_constraint_system {f} {v} {sys} {request} {a} {s}.
  Arguments As_prover {f} {v} {sys} {request} {a} {s}.
  Arguments With_label {f} {v} {sys} {request} {a} {b} {s}.
  Arguments With_state {f} {v} {sys} {request} {a} {b} {s1} {s2}.
  Arguments With_handler {f} {v} {sys} {request} {a} {b} {s}.
  Arguments Clear_handler {f} {v} {sys} {request} {a} {b} {s}.
  Arguments Exists {f} {v} {sys} {request} {a} {s} {var} {value}.
  Arguments Next_auxiliary {f} {v} {sys} {request} {a} {s}.
  Arguments Add_constraint {f} {v} {sys} {request} {a} {s}.
End Types.

Module Checked.
  Definition t := Types.checked.
End Checked.

Module Typ.
  Definition t := Types.typ.

  Section Typ.
    Context {f v sys var value : Type} {request : Type -> Type}.

    Definition store (t : t f v sys request var value) :
  value -> Store.t f v var :=
  match
    t in (Types.typ _ _ _ _ var value) return (value -> Store.t f v var)
  with
  | Types.Mk_typ _ _ _ _ _ _ store read alloc check => store
  end.

    Definition read (t : t f v sys request var value) :
  var -> Read.t f v value :=
  match
    t in (Types.typ _ _ _ _ var value) return (var -> Read.t f v value)
  with
  | Types.Mk_typ _ _ _ _ _ _ store read alloc check => read
  end.

    Definition alloc (t : t f v sys request var value) : Alloc.t v var :=
  match t in (Types.typ _ _ _ _ var value) return (Alloc.t v var) with
  | Types.Mk_typ _ _ _ _ _ _ store read alloc check => alloc
  end.

    Definition check (t : t f v sys request var value) :
  var -> Checked.t f v sys request unit unit :=
  match
    t in (Types.typ _ _ _ _ var value)
    return (var -> Checked.t f v sys request unit unit)
  with
  | Types.Mk_typ _ _ _ _ _ _ store read alloc check => check
  end.
  End Typ.
End Typ.
