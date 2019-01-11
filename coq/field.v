Section Field.

  Variable F : Type.

  (** Left general in case we want to use nat and Z, etc. *)
  Class Of_int (A : Type) := { of_int : A -> F }.

  Class One := { one : F }.

  Class Zero := { zero : F }.

  Class Add := { add : F -> F -> F }.

  Class Sub := { sub : F -> F -> F }.

  Class Mul := { mul : F -> F -> F }.

  Class Inv := { inv : F -> F }.

  Class Square := { square : F -> F }.

  Class Sqrt := { sqrt : F -> F }.

  Class Is_Square := { is_square : F -> bool }.

  Class Equal := { equal : F -> F -> bool }.

  Class Size_in_bits (F : Type) (A : Type) := { size_in_bits : A }.

  Definition negate `{Sub} `{Zero} x := sub zero x.

End Field.

Arguments of_int {F A Of_int} _.
Arguments one {F One}.
Arguments zero {F Zero}.
Arguments add {F Add} _ _.
Arguments sub {F Sub} _ _.
Arguments mul {F Mul} _ _.
Arguments inv {F Inv} _.
Arguments square {F Square} _.
Arguments sqrt {F Sqrt} _.
Arguments is_square {F Is_Square} _.
Arguments equal {F Equal} _ _.
Arguments size_in_bits {F A Size_in_bits}.
Arguments negate {F Sub Zero} _ : rename.

Module FieldNotation.

  Infix "+" := plus.
  Infix "*" := mul.
  Infix "-" := sub.
  Infix "/" := (fun x y => mul x (inv y)).

End FieldNotation.