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

  Fixpoint run {req A} (stack : t req) (request : req A) : option A :=
  match stack with
  | nil => None
  | (handle :: stack)%list =>
      match handle _ request with
      | Response.Provide a => Some a
      | Response.Delegate request => run stack request
      | Response.Unhandled => run stack request
      end
  end.

  Definition create_single {req}
  (handler : forall {A}, req A -> Response.t req A) : single req := @handler.
End Handler.
