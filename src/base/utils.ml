open Core_kernel

(** Alias [Cvar] to avoid shadowing. *)
module Cvar0 = Cvar

module Make
    (Backend : Backend_extended.S)
    (Checked : Checked_intf.Extended with type field := Backend.Field.t)
    (As_prover : As_prover.Extended
                   with module Types := Checked.Types
                    and type field := Backend.Field.t) =
struct
  open struct
    module Cvar = Backend.Cvar
    module Field = Backend.Field
    module Constraint = Backend.Constraint
  end

  let assert_equal ?label x y =
    match (x, y) with
    | Cvar0.Constant x, Cvar0.Constant y ->
        if Field.equal x y then Checked.return ()
        else
          failwithf !"assert_equal: %{sexp: Field.t} != %{sexp: Field.t}" x y ()
    | _ ->
        Checked.assert_equal ?label x y

  (* [equal_constraints z z_inv r] asserts that
     if z = 0 then r = 1, or
     if z <> 0 then r = 0 and z * z_inv = 1
  *)
  let equal_constraints (z : Cvar.t) (z_inv : Cvar.t) (r : Cvar.t) =
    Checked.assert_all
      [ Constraint.r1cs ~label:"equals_1" z_inv z Cvar.(constant Field.one - r)
      ; Constraint.r1cs ~label:"equals_2" r z (Cvar.constant Field.zero)
      ]

  (* [equal_vars z] computes [(r, z_inv)] that satisfy the constraints in
     [equal_constraints z z_inv r].

     In particular, [r] is [1] if [z = 0] and [0] otherwise.
  *)
  let equal_vars (z : Cvar.t) : (Field.t * Field.t) As_prover.t =
    let open As_prover in
    let%map z = As_prover.read_var z in
    if Field.equal z Field.zero then (Field.one, Field.zero)
    else (Field.zero, Field.inv z)
end
