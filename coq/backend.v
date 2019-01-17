Require Snarky.field.
Require Snarky.bigint.
Require Snarky.R1CS_constraint_system.
Require Snarky.cvar.

Module Field := Snarky.field.
Module Bigint := Snarky.bigint.

Class Field_size (bigint : Type) := { field_size : bigint }.

Module Var.
  Class Index (t : Type) := { index : t -> nat }.
  Arguments index {t}.

  Class Create (t : Type) := { create : nat -> t }.
  Arguments create {t}.

  Class Var (t : Type) :=
  { var_index :> Index t
  ; var_create :> Create t
  }.

  Module Class.
    Instance nat_var : Var nat :=
    {| var_index := {| index := fun n => n |}
    ; var_create := {| create := fun n => n |}
    |}.
  End Class.
  Include Class.
End Var.

Module Linear_combination.
  Class Create (t : Type) := { create : unit -> t }.
  Arguments create {t Create}.

  Class Of_var (t var : Type) := { of_var : var -> t }.
  Arguments of_var {t var Of_var}.

  Class Of_field (t field : Type) := { of_field : field -> t }.
  Arguments of_field {t field Of_field}.

  Class Add_term (t field var : Type) := { add_term : t -> field -> var -> t }.
  Arguments add_term {t field var Add_term}.

  Definition of_constant {t field : Type} `{Create t} `{Of_field t field} c :=
    match c with
    | None => create tt
    | Some c => of_field c
    end.

  Definition zero {t field A} `{field.Field field A} `{Of_field t field} := of_field field.zero.

  Module Class.
    Class Linear_combination (t field var : Type) :=
    { linear_combination_create : Create t
    ; linear_combination_of_var : Of_var t var
    ; linear_combination_of_field : Of_field t field
    ; linear_combination_add_term : Add_term t field var
    }.
  End Class.
  Include Class.
End Linear_combination.

Module R1CS_constraint. 
  Class Create (t linear_combination : Type) :=
    { create : linear_combination -> linear_combination -> linear_combination -> t }.
  Arguments create {t linear_combination Create}.

  Class Set_is_square (t : Type) := { set_is_square : t -> bool -> t }.
  Arguments set_is_square {t Set_is_square}.

  Module Class.
    Class R1CS_constraint (t linear_combination : Type) :=
    { r1cs_constraint_create :> Create t linear_combination
    ; r1cs_constraint_set_is_square :> Set_is_square t
    }.
  End Class.
  Include Class.
End R1CS_constraint.

Module R1CS_constraint_system := R1CS_constraint_system.

Include Field.Class.
Include Bigint.Class.
Include Var.Class.
Include Linear_combination.Class.
Include R1CS_constraint.Class.
Include R1CS_constraint_system.Class.