type error =
  | Unbound_type_var of string
  | Wrong_number_args of Path.t * int * int
  | Expected_type_var of Parsetypes.type_expr
  | Constraints_not_satisfied of Parsetypes.type_expr * Parsetypes.type_decl
  | Opaque_type_in_prover_mode of Parsetypes.type_expr
  | Convertible_arities_differ of string * int * string * int
  | GADT_in_nonrec_type

val unify :
  (loc:Location.t -> Envi.t -> Type0.type_expr -> Type0.type_expr -> unit) ref
(** Internal: Import from [Typechecker]. *)

module Type : sig
  val mk_poly :
       loc:Location.t
    -> mode:Ast_types.mode
    -> Typedast.type_expr list
    -> Typedast.type_expr
    -> Envi.t
    -> Typedast.type_expr

  val mk_tuple :
       loc:Location.t
    -> mode:Ast_types.mode
    -> Typedast.type_expr list
    -> Envi.t
    -> Typedast.type_expr

  val mk_arrow :
       loc:Location.t
    -> mode:Ast_types.mode
    -> ?explicit:Ast_types.explicitness
    -> ?label:Ast_types.arg_label
    -> Typedast.type_expr
    -> Typedast.type_expr
    -> Envi.t
    -> Typedast.type_expr

  val mk_prover :
       loc:Location.t
    -> mode:Ast_types.mode
    -> Typedast.type_expr
    -> Typedast.type_expr

  val mk_conv :
       loc:Location.t
    -> mode:Ast_types.mode
    -> Typedast.type_expr
    -> Typedast.type_expr
    -> Envi.t
    -> Typedast.type_expr

  val mk_opaque :
    loc:Location.t -> Typedast.type_expr -> Envi.t -> Typedast.type_expr

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
       ?name:Ident.t
    -> ?other_name:Path.t
    -> ?tri_stitched:(Envi.t -> Type0.type_expr list -> Type0.type_expr)
    -> Parsetypes.type_decl
    -> Envi.t
    -> Typedast.type_decl * Envi.t
  (** Import a type declaration.

      The [name] parameter can be used to specify an existing identifier for
      the type, instead of generating a fresh one.

      If [other_name] is specified, then the type declaration will be stitched
      to the type with that name; otherwise, the type is stitched to a type of
      its own name in the other mode.

      Alternatively, if [tri_stitched] is specified, then it will be evaluated
      with the environment containing the type parameters of the declaration
      and the type parameters of the type declaration.  The declaration's type
      will be tri-stitched to its return value as tri-stitched as
      [name -> tri_stitched@{P} <-> tri_stitched@{C}].
      This argument may only be used in checked mode, and cannot be used in
      combination with [other_name].
  *)

  val import_convertible :
       Parsetypes.type_decl
    -> Parsetypes.conv_type
    -> Envi.t
    -> Typedast.type_decl * Typedast.conv_type * Envi.t
  (** Import a type declaration, stitching it to the type described by the
      [conv_type] argument.
  *)

  val import_rec :
    Parsetypes.type_decl list -> Envi.t -> Typedast.type_decl list * Envi.t
  (** Import the given type declarations recursively. *)
end
