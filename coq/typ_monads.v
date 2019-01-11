Require Import Snarky.monad.

Module Store.

  Inductive t (F V A : Type) : Type :=
  | Pure : A -> t F V A
  | Free : F -> (V -> t F V A) -> t F V A.

  Arguments Pure {F V A} _.
  Arguments Free {F V A} _ _.

  Global Instance store_return F V : Return (t F V) := {
    ret A a := Pure a
  }.

  Global Instance store_bind F V : Bind (t F V) := {
    bind A B t f :=
      (fix bind' t :=
        match t with
        | Pure a => f a
        | Free x k => Free x (fun v => bind' (k v))
        end) t
  }.

  Global Instance store_map F V : Map (t F V) := {
    map A B t f :=
      (fix map' t :=
        match t with
        | Pure a => Pure (f a)
        | Free x k => Free x (fun v => map' (k v))
        end) t
  }.

  Definition store {F V} (x : F) : t F V V :=
    Free x (fun v => Pure v).

  Fixpoint run {F V A} (t : t F V A) (f : F -> V) : A :=
    match t with
    | Pure x => x
    | Free x k => run (k (f x)) f
    end.

End Store.

Module Read.

  Inductive t (F V A : Type) : Type :=
  | Pure : A -> t F V A
  | Free : V -> (F -> t F V A) -> t F V A.

  Arguments Pure {F V A} _.
  Arguments Free {F V A} _ _.

  Global Instance read_return F V : Return (t F V) := {
    ret A a := Pure a
  }.

  Global Instance read_bind F V : Bind (t F V) := {
    bind A B t f :=
      (fix bind' t :=
        match t with
        | Pure a => f a
        | Free x k => Free x (fun v => bind' (k v))
        end) t
  }.

  Global Instance read_map F V : Map (t F V) := {
    map A B t f :=
      (fix map' t :=
        match t with
        | Pure a => Pure (f a)
        | Free x k => Free x (fun v => map' (k v))
        end) t
  }.

  Definition read {F V} (x : V) : t F V F :=
    Free x ret.

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

  Arguments Pure {V A} _.
  Arguments Free {V A} _.

  Global Instance alloc_return V : Return (t V) := {
    ret A a := Pure a
  }.

  Global Instance alloc_bind V : Bind (t V) := {
    bind A B t f :=
      (fix bind' t :=
        match t with
        | Pure a => f a
        | Free k => Free (fun v => bind' (k v))
        end) t
  }.

  Global Instance alloc_map V : Map (t V) := {
    map A B t f :=
      (fix map' t :=
        match t with
        | Pure a => Pure (f a)
        | Free k => Free (fun v => map' (k v))
        end) t
  }.

  Definition alloc {V} : t V V :=
    Free (fun v => Pure v).

  Fixpoint run {V A} (t : t V A) (f : unit -> V) : A :=
    match t with
    | Pure x => x
    | Free k => run (k (f tt)) f
    end.

End Alloc.