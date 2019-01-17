Require Import List.
Import List.ListNotations.
Require Snarky.types.
Require Snarky.field.

Include Snarky.types.Cvar.

Module Unsafe.
  Definition of_var {F V} (v : V) : t F V := Var v.
End Unsafe.

Section Cvar.
  Context {F V : Type} `{field.Field F}.

  Definition eval (context : V -> F) (t0 : t F V) :=
    let fix go scale t0 :=
      match t0 with
      | Constant c => field.mul scale c
      | Var v => field.mul scale (context v)
      | Scale s t => go (field.mul s scale) t
      | Add t1 t2 => field.add (go scale t1) (go scale t2)
      end in
    go field.one t0.

  Definition constant (x : F) : t F V := Constant x.

  Definition to_constant_and_terms (t : t F V) :=
    let fix go scale constant terms t :=
      match t with
      | Constant c => (field.add constant (field.mul scale c), terms)
      | Var v => (constant, (scale, v) :: terms)
      | Scale s t => go (field.mul s scale) constant terms t
      | Add x1 x2 =>
        let (c, ts) := go scale constant terms x1 in
        go scale c terms x2
      end in
    let (c, ts) := go field.one field.zero [] t in
    (Some c, ts).

  Definition add (x y : t F V) :=
    match x, y with
    | Constant x, Constant y => Constant (field.add x y)
    | _, _ => Add x y
    end.

  Definition scale (x : t F V) (s : F) :=
    match x with
    | Constant x => Constant (field.mul x s)
    | Scale sx x => Scale (field.mul sx s) x
    | _ => Scale s x
    end.

  Definition neg_one := field.sub field.zero field.one.

  Definition sub (t1 t2 : t F V) :=
    add t1 (scale t2 neg_one).

  Definition linear_combination (terms : list (F * t F V)) :=
    List.fold_left (fun acc (term : F * t F V) =>
      let (c, t) := term in
      add acc (scale t c)) terms (constant field.zero).

  Definition sum (vs : list (t F V)) :=
    linear_combination (List.map (fun v => (field.one, v)) vs).

End Cvar.