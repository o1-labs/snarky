type error =
  | Unbound_type_var of Parsetypes.type_expr
  | Wrong_number_args of Longident.t * int * int
  | Expected_type_var of Parsetypes.type_expr
  | Constraints_not_satisfied of Parsetypes.type_expr * Parsetypes.type_decl

module Type : sig
  val import :
       ?must_find:bool
    -> Parsetypes.type_expr
    -> Envi.t
    -> Type0.type_expr * Envi.t
end

module TypeDecl : sig
  val import : Parsetypes.type_decl -> Envi.t -> Type0.type_decl * Envi.t
end
