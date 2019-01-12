Require Import Snarky.monad.
Require Snarky.types.
Export Snarky.types.Checked.
Export Snarky.types.Types.

Section Instances.

  Variables F V sys S : Type.

  Global Instance checked_return : Return (t F V sys S) := {
    ret A x := Pure x
  }.

  Global Instance checked_map : Map (t F V sys S) := {
    map :=
      (fix map' S A B (t : checked F V sys S A) : (A -> B) -> checked F V sys S B :=
        match t in checked _ _ _ s a return (a -> B) -> checked F V sys s B with
        | Pure x => fun f => Pure (f x)
        | With_label s t k => fun f => With_label s t (fun b => map' _ _ _ (k b) f)
        | As_prover x k => fun f => As_prover x (map' _ _ _ k f)
        | Add_constraint c t1 => fun f => Add_constraint c (map' _ _ _ t1 f)
        | With_state p and_then t_sub k =>
            fun f => With_state p and_then t_sub (fun b => map' _ _ _ (k b) f)
        | With_handler h t k => fun f => With_handler h t (fun b => map' _ _ _ (k b) f)
        | Clear_handler t k => fun f => Clear_handler t (fun b => map' _ _ _ (k b) f)
        | Exists typ c k => fun f => Exists typ c (fun v => map' _ _ _ (k v) f)
        | Next_auxiliary k => fun f => Next_auxiliary (fun x => map' _ _ _ (k x) f)
        end) S
  }.

  Global Instance checked_bind : Bind (t F V sys S) := {
    bind :=
      (fix bind' S A B (t : checked F V sys S A) : (A -> checked F V sys S B) -> checked F V sys S B :=
        match t in checked _ _ _ s a return (a -> checked F V sys s B) -> checked F V sys s B with
        | Pure x => fun f => f x
        | With_label s t k => fun f => With_label s t (fun b => bind' _ _ _ (k b) f)
        | As_prover x k => fun f => As_prover x (bind' _ _ _ k f)
        | Add_constraint c t1 => fun f => Add_constraint c (bind' _ _ _ t1 f)
        | With_state p and_then t_sub k =>
            fun f => With_state p and_then t_sub (fun b => bind' _ _ _ (k b) f)
        | With_handler h t k => fun f => With_handler h t (fun b => bind' _ _ _ (k b) f)
        | Clear_handler t k => fun f => Clear_handler t (fun b => bind' _ _ _ (k b) f)
        | Exists typ c k => fun f => Exists typ c (fun v => bind' _ _ _ (k v) f)
        | Next_auxiliary k => fun f => Next_auxiliary (fun x => bind' _ _ _ (k x) f)
        end) S
  }.

End Instances.

Definition as_prover {F V sys S} x : t F V sys S unit := As_prover x ((ret : _ -> t _ _ _ _ _) tt).