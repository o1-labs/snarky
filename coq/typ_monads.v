Require Import Snarky.monad.

Set Universe Polymorphism.

Module Store.

Inductive t (F V A : Type) : Type :=
  | Pure : A -> t F V A
  | Free : F -> (V -> t F V A) -> t F V A.

  Arguments Pure {F} {V} {A} _.
  Arguments Free {F} {V} {A} _ _.

  Global
Instance store_return  F V: (Return (t F V)) := { ret :=fun A a => Pure a}.

  Global
Instance store_bind  F V: (Bind (t F V)) := {
 bind :=fun A B t f =>
        (fix bind' t :=
           match t with
           | Pure a => f a
           | Free x k => Free x (fun v => bind' (k v))
           end) t}.

  Global
Instance store_map  F V: (Map (t F V)) := {
 map :=fun A B t f =>
       (fix map' t :=
          match t with
          | Pure a => Pure (f a)
          | Free x k => Free x (fun v => map' (k v))
          end) t}.

  Module Monad.
    Section Monad.
      Context {F V A B : Type}.

      Definition map : t F V A -> (A -> B) -> t F V B := map.
      Definition bind : t F V A -> (A -> t F V B) -> t F V B := bind.
      Definition both : t F V A -> t F V B -> t F V (A * B) := both.
      Definition ret : A -> t F V A := ret.
      Definition join : t F V (t F V A) -> t F V A := join.
      Definition ignore_m : t F V A -> t F V unit := ignore_m.
      Definition all : list (t F V A) -> t F V (list A) := all.
    End Monad.
  End Monad.

  Include Monad.

Definition store {F} {V} (x : F) : t F V V := Free x (fun v => Pure v).

  Fixpoint run {F V S A} (x : t F V A) (f : S -> F -> S * V) (s : S) : S * A :=
    match x with
    | Pure x => (s, x)
    | Free x k =>
      let (s, v) := f s x in
      run (k v) f s
    end.

End Store.

Module Read.

Inductive t (F V A : Type) : Type :=
  | Pure : A -> t F V A
  | Free : V -> (F -> t F V A) -> t F V A.

  Arguments Pure {F} {V} {A} _.
  Arguments Free {F} {V} {A} _ _.

  Global
Instance read_return  F V: (Return (t F V)) := { ret :=fun A a => Pure a}.

  Global
Instance read_bind  F V: (Bind (t F V)) := {
 bind :=fun A B t f =>
        (fix bind' t :=
           match t with
           | Pure a => f a
           | Free x k => Free x (fun v => bind' (k v))
           end) t}.

  Global
Instance read_map  F V: (Map (t F V)) := {
 map :=fun A B t f =>
       (fix map' t :=
          match t with
          | Pure a => Pure (f a)
          | Free x k => Free x (fun v => map' (k v))
          end) t}.

  Module Monad.
    Section Monad.
      Context {F V A B : Type}.

      Definition map : t F V A -> (A -> B) -> t F V B := map.
      Definition bind : t F V A -> (A -> t F V B) -> t F V B := bind.
      Definition both : t F V A -> t F V B -> t F V (A * B) := both.
      Definition ret : A -> t F V A := ret.
      Definition join : t F V (t F V A) -> t F V A := join.
      Definition ignore_m : t F V A -> t F V unit := ignore_m.
      Definition all : list (t F V A) -> t F V (list A) := all.
    End Monad.
  End Monad.

  Definition read {F} {V} (x : V) : t F V F := Free x ret.

  Fixpoint run {F V A} (t : t F V A) (f : V -> F) : A :=
    match t with
    | Pure x => x
    | Free x k => run (k (f x)) f
    end.

End Read.

Module Alloc.

Inductive t (V A : Type) : Type :=
  | Pure : A -> t V A
  | Free : (V -> t V A) -> t V A.

  Arguments Pure {V} {A} _.
  Arguments Free {V} {A} _.

  Global
Instance alloc_return  V: (Return (t V)) := { ret :=fun A a => Pure a}.

  Global
Instance alloc_bind  V: (Bind (t V)) := {
 bind :=fun A B t f =>
        (fix bind' t :=
           match t with
           | Pure a => f a
           | Free k => Free (fun v => bind' (k v))
           end) t}.

  Global
Instance alloc_map  V: (Map (t V)) := {
 map :=fun A B t f =>
       (fix map' t :=
          match t with
          | Pure a => Pure (f a)
          | Free k => Free (fun v => map' (k v))
          end) t}.

  Module Monad.
    Section Monad.
      Context {V A B : Type}.

      Definition map : t V A -> (A -> B) -> t V B := map.
      Definition bind : t V A -> (A -> t V B) -> t V B := bind.
      Definition both : t V A -> t V B -> t V (A * B) := both.
      Definition ret : A -> t V A := ret.
      Definition join : t V (t V A) -> t V A := join.
      Definition ignore_m : t V A -> t V unit := ignore_m.
      Definition all : list (t V A) -> t V (list A) := all.
    End Monad.
  End Monad.

  Definition alloc {V} : t V V := Free (fun v => Pure v).


  Fixpoint run {V S A} (t : t V A) (f : S -> S * V) (s : S) : S * A :=
    match t with
    | Pure x => (s, x)
    | Free k =>
      let (s, v) := f s in
      run (k v) f s
    end.

End Alloc.
