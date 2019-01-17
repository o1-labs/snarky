Require List.

Module Response.
  Inductive t (req : Type -> Type) (A : Type) :=
  | Provide : A -> t req A
  | Delegate : req A -> t req A
  | Unhandled : t req A.

  Arguments Provide {req} {A}.
  Arguments Delegate {req} {A}.
  Arguments Unhandled {req} {A}.
End Response.

Module Handler.
  Import Response.

  Definition single (req : Type -> Type) := forall A, req A -> Response.t req A.

  Definition t req := list (single req).

  Definition fail {req} : t req := nil.

  Definition push {req} (t : t req) (single : single req) := (single :: t)%list.

  Unknown constructor: Provide.

  Definition create_single {req}
  (handler : forall {A}, req A -> Response.t req A) : single req := handler.
End Handler.