Require Import String.
Require Snarky.Util.
Require Import List.
Import List.ListNotations.
Require Snarky.types.
Require Import Snarky.backend.
Require Snarky.cvar.
Import Snarky.types.Constraint.Basic.
Import Snarky.types.Constraint.

Section T.
  Context {V : Type}.

  Definition create_basic label basic : basic_with_annotation V :=
  {| basic := basic; annot := label |}.

  Definition override_label x label_opt : basic_with_annotation V :=
  {|
  basic := basic x;
  annot := match label_opt with
           | Some x => Some x
           | None => annot x
           end |}.

  Definition equal label x y := [create_basic label (Equal x y)].

  Definition boolean label x := [create_basic label (Boolean x)].

  Definition r1cs label a b c := [create_basic label (R1CS a b c)].

  Definition square label a b := [create_basic label (Square a b)].

  Definition annotation (t : t V) :=
  String.concat "; " (Util.List.filter_map annot t).

End T.

Section Snarky.

  Context {field var constraint linear_combination sys A : Type}
 `{F : Field field A}
 `{LC : Linear_combination linear_combination field (cvar.t field var)}
 `{R1C : R1CS_constraint constraint linear_combination}
 `{R1CS' : R1CS_constraint_system field constraint sys}.

  Definition basic_to_r1cs_constraint (basic : Basic.t (cvar.t field var)) :
  constraint.
    (destruct LC, R1C).
    (refine
  (let of_var := Linear_combination.of_var in
   match basic with
   | Boolean v =>
       let lc := of_var v in
       let constr := R1CS_constraint.create lc lc lc in
       R1CS_constraint.set_is_square constr true
   | Equal v1 v2 =>
       let constr :=
         R1CS_constraint.create Linear_combination.zero
           Linear_combination.zero (of_var (cvar.sub v1 v2)) in
       R1CS_constraint.set_is_square constr true
   | Square a c =>
       let a := of_var a in
       let constr := R1CS_constraint.create a a (of_var c) in
       R1CS_constraint.set_is_square constr true
   | R1CS a b c =>
       let constr := R1CS_constraint.create (of_var a) (of_var b) (of_var c)
         in
       R1CS_constraint.set_is_square constr false
   end); assumption).
  Defined.

  Definition stack_to_string := String.concat "\n".

  Definition add (stack : list string) (t : t (cvar.t field var))
  (system : R1CS_constraint_system.t sys) :=
  fold_left
    (fun system t =>
     let label :=
       match annot t with
       | Some label => label
       | None => "unknown"%string : string
       end in
     let c := basic_to_r1cs_constraint (basic t) in
     R1CS_constraint_system.add_constraint_with_annotation system c
       (stack_to_string (label :: stack))) t system.

  Definition eval_basic (t : Basic.t (cvar.t field var)) get_value :=
  match t with
  | Boolean v =>
      let x := get_value v in
      (field.equal x field.zero || field.equal x field.one)%bool
  | Equal v1 v2 => field.equal (get_value v1) (get_value v2)
  | R1CS v1 v2 v3 =>
      field.equal (field.mul (get_value v1) (get_value v2)) (get_value v3)
  | Square a c => field.equal (field.square (get_value a)) (get_value c)
  end.

  Definition eval (t : t (cvar.t field var)) get_value :=
  forallb (fun t => eval_basic (basic t) get_value) t.

End Snarky.