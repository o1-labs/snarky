Require List.

Module Response.
  Inductive t (req : Type -> Type) (A : Type) :=
  | Provide : A -> t req A
  | Delegate : req A -> t req A
  | Unhandled : t req A.

  Arguments Provide {req A}.
  Arguments Delegate {req A}.
  Arguments Unhandled {req A}.
End Response.

Module Handler.
  Import Response.

  Definition single (req : Type -> Type) := forall A, req A -> Response.t req A.

  Definition t req := list (single req).

  Definition fail {req} : t req := List.nil.

  Definition push {req} (t : t req) (single : single req) := cons single t.

  Fixpoint run {req A} (stack : t req) (request : req A) : option A :=
    match stack with
    | nil => None
    | cons handle stack =>
      match handle _ request with
      | Provide a => Some a
      | Delegate request => run stack request
      | Unhandled => run stack request
      end
    end.

  Definition create_single {req} (handler : forall {A}, req A -> Response.t req A)
    : single req :=
    @handler.
End Handler.