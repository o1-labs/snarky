Require Import String.
Require Import List.
Import List.ListNotations.
Require Import String.
Require Import Snarky.monad.
Require Import Snarky.types.
Require Snarky.field.
Require Snarky.cvar.
Require Snarky.as_prover.
Require Snarky.R1CS_constraint_system.
Require Snarky.constraint.
Require Snarky.provider.
Require Snarky.typ_monads.
Require Snarky.backend.
Export Snarky.types.Checked.
Export Snarky.types.Types.

Set Universe Polymorphism.

Section Instances.

  Variables F V sys S : Type.
  Variable req : Type -> Type.

  Global Instance checked_return : Return (t F V sys req S) := {
    ret A x := Pure x
  }.

  Global Instance checked_map : Map (t F V sys req S) := {
    map :=
      (fix map' S A B (t : checked F V sys req S A) : (A -> B) -> checked F V sys req S B :=
        match t in checked _ _ _ _ s a return (a -> B) -> checked F V sys req s B with
        | Pure x => fun f => Pure (f x)
        | With_label s t k => fun f => With_label s t (fun b => map' _ _ _ (k b) f)
        | With_constraint_system c k => fun f => With_constraint_system c (map' _ _ _ k f)
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

  Global Instance checked_bind : Bind (t F V sys req S) := {
    bind :=
      (fix bind' S A B (t : checked F V sys req S A) : (A -> checked F V sys req S B) -> checked F V sys req S B :=
        match t in checked _ _ _ _ s a return (a -> checked F V sys req s B) -> checked F V sys req s B with
        | Pure x => fun f => f x
        | With_label s t k => fun f => With_label s t (fun b => bind' _ _ _ (k b) f)
        | With_constraint_system c k => fun f => With_constraint_system c (bind' _ _ _ k f)
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

Definition as_prover {F V sys req S} x : t F V sys req S unit :=
  As_prover x ((ret : _ -> t _ _ _ _ _ _) tt).

Module Run.
  Section Run_util.
    Set Implicit Arguments.
    Record run_state (F sys' : Type) (req : Type -> Type) (S : Type) :=
    { num_inputs : nat
    ; input : list F
    ; next_auxiliary : nat
    ; aux : list F
    ; system : option (backend.R1CS_constraint_system.t sys')
    ; eval_constraints : bool
    ; handler : request.Handler.t req
    ; state : option S
    ; stack : list string }.
    Unset Implicit Arguments.

    Context {F sys' : Type} {req : Type -> Type} `{field.Field F}.

    Definition sys := backend.R1CS_constraint_system.t sys'.

    Definition var := types.Cvar.t F nat.

(*Definition run {F sys S A} `{field.Field F} (num_inputs : nat) (input : list F) (aux : list F) (system : option sys)
  (eval_constraints : bool) (t0 : t F nat sys S A) (s0 : option s) :=
  let next_auxiliary := 1 + num_inputs in
  let eval_constraints := match s0 with
    | Some _ => eval_constraints
    | None => false
    end in
  let get_value := get_value num_inputs input in
  let run_as_prover := run_as_prover num_inputs input in
  let system := match system with
    | Some system => Some (R1CS_constraint_system.set_primary_input_size system num_inputs)
    | None => None in*)

    Definition set_next_auxiliary {S : Type} (next_auxiliary : nat) (run_state : run_state F sys' req S) :=
    {| num_inputs := num_inputs run_state
    ; input := input run_state
    ; next_auxiliary := next_auxiliary
    ; aux := aux run_state
    ; system := system run_state
    ; eval_constraints := eval_constraints run_state
    ; handler := handler run_state
    ; state := state run_state
    ; stack := stack run_state |}.

    Definition set_aux {S : Type} (aux : list F) (run_state : run_state F sys' req S) :=
    {| num_inputs := num_inputs run_state
    ; input := input run_state
    ; next_auxiliary := next_auxiliary run_state
    ; aux := aux
    ; system := system run_state
    ; eval_constraints := eval_constraints run_state
    ; handler := handler run_state
    ; state := state run_state
    ; stack := stack run_state |}.

    Definition set_system {S : Type} (system : option sys) (run_state : run_state F sys' req S)
      : Run_util.run_state F sys' req S :=
    {| num_inputs := num_inputs run_state
    ; input := input run_state
    ; next_auxiliary := next_auxiliary run_state
    ; aux := aux run_state
    ; system := system
    ; eval_constraints := eval_constraints run_state
    ; handler := handler run_state
    ; state := state run_state
    ; stack := stack run_state |}.

    Definition set_handler {S : Type} (handler : request.Handler.t req)
      (run_state : run_state F sys' req S) :=
    {| num_inputs := num_inputs run_state
    ; input := input run_state
    ; next_auxiliary := next_auxiliary run_state
    ; aux := aux run_state
    ; system := system run_state
    ; eval_constraints := eval_constraints run_state
    ; handler := handler
    ; state := state run_state
    ; stack := stack run_state |}.

    Definition set_state {S1 S2 : Type} (state : option S2) (run_state : run_state F sys' req S1) :=
    {| num_inputs := num_inputs run_state
    ; input := input run_state
    ; next_auxiliary := next_auxiliary run_state
    ; aux := aux run_state
    ; system := system run_state
    ; eval_constraints := eval_constraints run_state
    ; handler := handler run_state
    ; state := state
    ; stack := stack run_state |}.

    Definition set_stack {S : Type} (stack : list string)
      (run_state : run_state F sys' req S) :=
    {| num_inputs := num_inputs run_state
    ; input := input run_state
    ; next_auxiliary := next_auxiliary run_state
    ; aux := aux run_state
    ; system := system run_state
    ; eval_constraints := eval_constraints run_state
    ; handler := handler run_state
    ; state := state run_state
    ; stack := stack |}.

    Definition get_value {S : Type} (rstate : run_state F sys' req S) : var -> F :=
      let num_inputs := num_inputs rstate in
      let get_one i :=
        if Compare_dec.le_lt_dec i num_inputs then
          List.nth (i - 1) (input rstate) field.zero
        else
          List.nth (i - num_inputs - 1) (aux rstate) field.zero in
      cvar.eval get_one.

    Definition store_field_elt {S : Type} (rstate : run_state F sys' req S) x
      : run_state F sys' req S * var :=
     let v := next_auxiliary rstate in
     let rstate := set_aux (aux rstate ++ [x]) rstate in
     let rstate := set_next_auxiliary (v + 1) rstate in
     (rstate, cvar.Unsafe.of_var v).

    Definition alloc_var {S : Type} (rstate : run_state F sys' req S) :
      run_state F sys' req S * var :=
      let v := next_auxiliary rstate in
      let rstate := set_next_auxiliary (v + 1) rstate in
      (rstate, cvar.Unsafe.of_var v).

    Definition run_as_prover {S S' A : Type} (rstate : run_state F sys' req S')
      (x : option (as_prover.t F var S A)) (s : option S) : option S * option A :=
      match x, s with
      | Some x, Some s =>
        let (s', y) := as_prover.run x (get_value rstate) s in
        (Some s', Some y)
      | _, _ => (None, None)
      end.

  Context {linear_combination constraint : Type}.
  Context `{backend.R1CS_constraint_system F constraint sys'}.
  Context `{backend.R1CS_constraint constraint linear_combination}.
  Context `{backend.R1CS_constraint}.
  Context `{backend.Linear_combination linear_combination F (cvar.t F nat)}.

  Definition add_constraint {S : Type} (c : types.Constraint.t var)
    (rstate : run_state F sys' req S) : run_state F sys' req S :=
    match system rstate with
    | Some system =>
      let stack := stack rstate in
      set_system (Some (constraint.add stack c system)) rstate
    | None => rstate
    end.

  Definition store_or_alloc {Var Value S} (typ : Typ.t F var sys req Var Value)
    (value : option Value) (rstate : run_state F sys' req S) :=
    match value with
    | Some value =>
      typ_monads.Store.run (Typ.store typ value) store_field_elt rstate
    | None => typ_monads.Alloc.run (Typ.alloc typ) alloc_var rstate
    end.
  End Run_util.

  Section Run_aux.
    Context {F sys' : Type} {req : Type -> Type} `{field.Field F}.

    Context {linear_combination constraint : Type}.
    Context `{backend.R1CS_constraint_system F constraint sys'}.
    Context `{backend.R1CS_constraint constraint linear_combination}.
    Context `{backend.R1CS_constraint}.
    Context `{backend.Linear_combination linear_combination F (cvar.t F nat)}.

    Fixpoint run_aux {S A : Type} (rstate : run_state F sys' req S) (t : t F var sys req S A)
      {struct t} : (run_state F sys' req S * A) + _ :=
      match t in types.Types.checked _ _ _ _ s a
      return run_state F sys' req s -> (run_state F sys' req s * a) + _ with
      | Pure x => fun rstate => inl (rstate, x)
      | With_constraint_system f k => fun rstate =>
        let system := match system rstate with
          | Some system => Some (f system)
          | None => None
          end in
        run_aux (set_system system rstate) k
      | With_label lab t k => fun rstate =>
        let stack := stack rstate in
        match run_aux (set_stack (lab :: stack) rstate) t with
        | inl (rstate, y) => run_aux (set_stack stack rstate) (k y)
        | inr err => inr err
        end
      | As_prover x k => fun rstate =>
        let (s, _) := run_as_prover rstate (Some x) (state rstate) in
        run_aux (set_state s rstate) k
      | Add_constraint c t1 => fun rstate =>
        (** TODO: currently ignoring this. *)
        let failed := andb (eval_constraints rstate) (constraint.eval c (get_value rstate)) in
        if failed then
          inr (String.concat "\n" ["Constraint unsatisfied:"; constraint.annotation c;
            constraint.stack_to_string (stack rstate)]%string)
        else run_aux (add_constraint c rstate) t1
      | With_state p and_then t_sub k => fun rstate =>
        let s := state rstate in
        let (s1, s_sub) := run_as_prover rstate (Some p) s in
        match run_aux (set_state s_sub rstate) t_sub with
        | inl (rstate, y) =>
          let p1 := match state rstate return forall T, (_ -> T) -> option T with
            | Some s_sub => fun T and_then => Some (and_then s_sub)
            | None => fun _ _ => None
            end _ and_then in
          let rstate := set_state s rstate in
          let (s, _) := run_as_prover rstate p1 s1 in
          run_aux rstate (k y)
        | inr err => inr err
        end
      | With_handler h t k => fun rstate =>
        let handler := handler rstate in
        let rstate := set_handler (request.Handler.push handler h) rstate in
        match run_aux rstate t with
        | inl (rstate, y) => run_aux (set_handler handler rstate) (k y)
        | inr err => inr err
        end
      | Clear_handler t k => fun rstate =>
        let handler := handler rstate in
        let rstate := set_handler (List.nil) rstate in
        match run_aux rstate t with
        | inl (rstate, y) => run_aux (set_handler handler rstate) (k y)
        | inr err => inr err
        end
      | Next_auxiliary k => fun rstate =>
        run_aux rstate (k (next_auxiliary rstate))
      | Exists typ p k => fun rstate =>
        match state rstate with
        | Some s =>
          let (s', value) := provider.run p (get_value rstate) s (handler rstate) in
          match value with
          | Some value =>
            let (rstate, var) := typ_monads.Store.run (Typ.store typ value) store_field_elt rstate in
            match run_aux (set_state (Some tt) rstate) (Typ.check typ var) with
            | inl (rstate, _) =>
              run_aux (set_state (Some s') rstate)
                (k {| Handle.var := var; Handle.value := Some value |})
            | inr err => inr err
            end
          | None => inr "Unhandled request"%string
          end
        | None => 
          let (rstate, var) := typ_monads.Alloc.run (Typ.alloc typ) alloc_var rstate in
          match run_aux (set_state None rstate) (Typ.check typ var) with
          | inl (rstate, _) =>
            run_aux (set_state None rstate) (k {| Handle.var := var; Handle.value := None |})
          | inr err => inr err
          end
        end
      end rstate.

  Definition run {A S} (input : list F) (system : option sys) (eval_constraints : bool)
    (t : t F var sys req S A) (s : option S) :=
    let num_inputs := List.length input in
    let rstate := {|
      num_inputs := num_inputs
    ; input := input
    ; next_auxiliary := num_inputs + 1
    ; aux := []
    ; system := system
    ; eval_constraints := eval_constraints
    ; handler := request.Handler.fail
    ; state := s
    ; stack := []
    |} in
    run_aux rstate t.
End Run_aux.
End Run.
