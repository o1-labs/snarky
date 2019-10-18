type error =
  | Unbound_type_var of Parsetypes.type_expr
  | Wrong_number_args of Path.t * int * int
  | Expected_type_var of Parsetypes.type_expr
  | Constraints_not_satisfied of Parsetypes.type_expr * Parsetypes.type_decl
  | Opaque_type_in_prover_mode of Parsetypes.type_expr

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

  val import :
       ?name:Ident.t
    -> ?other_name:Path.t
    -> ?tri_name:Path.t
    -> Parsetypes.type_decl
    -> Envi.t
    -> Typedast.type_decl * Envi.t
  (** Import a type declaration.

      The [name] parameter can be used to specify an existing identifier for
      the type, instead of generating a fresh one.

      If [other_name] is specified, then the type declaration will be stitched
      to the type with that name; otherwise, the type is stitched to a type of
      its own name in the other mode.

      If [tri_name] is specified in addition to [other_name], then the type
      will be tri-stitched as [name -> other_name <-> tri_name].
  *)
end
