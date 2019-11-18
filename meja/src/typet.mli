type error =
  | Unbound_type_var of Parsetypes.type_expr
  | Wrong_number_args of Path.t * int * int
  | Expected_type_var of Parsetypes.type_expr
  | Constraints_not_satisfied of Parsetypes.type_expr * Parsetypes.type_decl
  | GADT_in_nonrec_type

module Type : sig
  val import :
       ?must_find:bool
    -> Parsetypes.type_expr
    -> Envi.t
    -> Typedast.type_expr * Envi.t

  val fold :
       init:'a
    -> f:('a -> Parsetypes.type_expr -> 'a)
    -> Parsetypes.type_expr
    -> 'a

  val iter : f:(Parsetypes.type_expr -> unit) -> Parsetypes.type_expr -> unit

  val map :
       loc:Location.t
    -> f:(Parsetypes.type_expr -> Parsetypes.type_expr)
    -> Parsetypes.type_expr
    -> Parsetypes.type_expr
end

module TypeDecl : sig
  val generalise :
    Parsetypes.type_decl -> Parsetypes.type_decl * Parsetypes.type_decl

  val predeclare : Envi.t -> Parsetypes.type_decl -> Envi.t
  (** Add the type to the environment as an abstract type.

      This can be used to set up the environment for recursive or co-recursive
      types, which need to find their own identifier in scope.
  *)

  val import :
       ?other_name:Path.t
    -> ?newtype:bool
    -> Parsetypes.type_decl
    -> Envi.t
    -> Typedast.type_decl * Envi.t
  (** Import a type declaration.

      If [other_name] is specified, then the type declaration will be stitched
      to the type with that name; otherwise, the type is stitched to a type of
      its own name in the other mode.
  *)

  val import_rec :
    Parsetypes.type_decl list -> Envi.t -> Typedast.type_decl list * Envi.t
  (** Import the given type declarations recursively. *)
end
