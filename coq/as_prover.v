Require Import Snarky.types.
Require Import Snarky.monad.

Definition t := As_prover.t.

Section Instances.

  Variables f v s : Type.

  Global Instance as_prover_map f v s : Map (t f v s) := {
    map := fun A B t f tbl s =>
      let (s', x) := t tbl s in
      (s', f x)
  }.

  Global Instance as_prover_bind f v s : Bind (t f v s) := {
    bind := fun A B t f tbl s =>
      let (s', x) := t tbl s in
      f x tbl s'
  }.

  Global Instance as_prover_ret f v s : Return (t f v s) := {
    ret := fun A x _ s => (s, x)
  }.

End Instances.

Module T.

  Section T.

    Context {F V S : Type}.

    Definition run {S A : Type} : t F V S A -> (V -> F) -> S -> S * A :=
      fun t tbl s => t tbl s.

    Definition get_state {S : Type} : t F V S S :=
      fun _tbl s => (s, s).

    Definition set_state {S : Type} : S -> t F V S unit :=
      fun s _tbl _ => (s, tt).

    Definition modify_state {S : Type} : (S -> S) -> t F V S unit :=
      fun f _tbl s => (f s, tt).

    Definition read_var (v : V) : t F V S F :=
      fun tbl s => (s, tbl v).

    Definition read {f v sys s var value : Type}
      (typ : Snarky.types.Typ.t f v sys var value) (var : var)
    : t f v s value :=
      fun tbl s => (s, typ_monads.Read.run (Typ.read typ var) tbl).

  End T.
    
End T.

Export T.