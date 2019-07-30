open Core_kernel
open Typedast
open Ast_types

type iterator =
  { type_expr: iterator -> type_expr -> unit
  ; type_desc: iterator -> type_desc -> unit
  ; variant: iterator -> variant -> unit
  ; ptype_expr: iterator -> Parsetypes.type_expr -> unit
  ; ptype_desc: iterator -> Parsetypes.type_desc -> unit
  ; pvariant: iterator -> Parsetypes.variant -> unit
  ; field_decl: iterator -> Parsetypes.field_decl -> unit
  ; ctor_args: iterator -> Parsetypes.ctor_args -> unit
  ; ctor_decl: iterator -> Parsetypes.ctor_decl -> unit
  ; type_decl: iterator -> Parsetypes.type_decl -> unit
  ; type_decl_desc: iterator -> Parsetypes.type_decl_desc -> unit
  ; literal: iterator -> literal -> unit
  ; pattern: iterator -> pattern -> unit
  ; pattern_desc: iterator -> pattern_desc -> unit
  ; expression: iterator -> expression -> unit
  ; expression_desc: iterator -> expression_desc -> unit
  ; signature_item: iterator -> signature_item -> unit
  ; signature: iterator -> signature -> unit
  ; signature_desc: iterator -> signature_desc -> unit
  ; module_sig: iterator -> module_sig -> unit
  ; module_sig_desc: iterator -> module_sig_desc -> unit
  ; statement: iterator -> statement -> unit
  ; statements: iterator -> statements -> unit
  ; statement_desc: iterator -> statement_desc -> unit
  ; module_expr: iterator -> module_expr -> unit
  ; module_desc: iterator -> module_desc -> unit
  ; location: iterator -> Location.t -> unit
  ; longident: iterator -> Longident.t -> unit
  ; ident: iterator -> Ident.t -> unit
  ; path: iterator -> Path.t -> unit
  ; type0_expr: iterator -> Type0.type_expr -> unit
  ; type0_decl: iterator -> Type0.type_decl -> unit }

let lid iter {Location.txt; loc} =
  iter.longident iter txt ; iter.location iter loc

let str iter ({Location.txt= _; loc} : str) = iter.location iter loc

let ident iter ({Location.txt; loc} : Ident.t Location.loc) =
  iter.ident iter txt ; iter.location iter loc

let path iter ({Location.txt; loc} : Path.t Location.loc) =
  iter.location iter loc ; iter.path iter txt

let type_expr iter {type_desc; type_loc; type_type} =
  iter.location iter type_loc ;
  iter.type0_expr iter type_type ;
  iter.type_desc iter type_desc

let type_desc iter = function
  | Ttyp_var (name, _) ->
      Option.iter ~f:(str iter) name
  | Ttyp_tuple typs ->
      List.iter ~f:(iter.type_expr iter) typs
  | Ttyp_arrow (typ1, typ2, _, _) ->
      iter.type_expr iter typ1 ; iter.type_expr iter typ2
  | Ttyp_ctor variant ->
      iter.variant iter variant
  | Ttyp_poly (vars, typ) ->
      List.iter ~f:(iter.type_expr iter) vars ;
      iter.type_expr iter typ

let variant iter {var_ident; var_params; var_implicit_params} =
  path iter var_ident ;
  List.iter ~f:(iter.type_expr iter) var_params ;
  List.iter ~f:(iter.type_expr iter) var_implicit_params

let ptype_expr iter Parsetypes.{type_desc; type_loc} =
  iter.location iter type_loc ;
  iter.ptype_desc iter type_desc

let ptype_desc iter = function
  | Parsetypes.Ptyp_var (name, _) ->
      Option.iter ~f:(fun name -> iter.location iter name.Location.loc) name
  | Ptyp_tuple typs ->
      List.iter ~f:(iter.ptype_expr iter) typs
  | Ptyp_arrow (typ1, typ2, _, _) ->
      iter.ptype_expr iter typ1 ; iter.ptype_expr iter typ2
  | Ptyp_ctor variant ->
      iter.pvariant iter variant
  | Ptyp_poly (vars, typ) ->
      List.iter ~f:(iter.ptype_expr iter) vars ;
      iter.ptype_expr iter typ

let pvariant iter Parsetypes.{var_ident; var_params; var_implicit_params} =
  iter.location iter var_ident.loc ;
  iter.longident iter var_ident.txt ;
  List.iter ~f:(iter.ptype_expr iter) var_params ;
  List.iter ~f:(iter.ptype_expr iter) var_implicit_params

let field_decl iter Parsetypes.{fld_ident; fld_type; fld_loc} =
  iter.location iter fld_loc ;
  str iter fld_ident ;
  iter.ptype_expr iter fld_type

let ctor_args iter = function
  | Parsetypes.Ctor_tuple typs ->
      List.iter ~f:(iter.ptype_expr iter) typs
  | Ctor_record fields ->
      List.iter ~f:(iter.field_decl iter) fields

let ctor_decl iter Parsetypes.{ctor_ident; ctor_args; ctor_ret; ctor_loc} =
  iter.location iter ctor_loc ;
  str iter ctor_ident ;
  iter.ctor_args iter ctor_args ;
  Option.iter ~f:(iter.ptype_expr iter) ctor_ret

let type_decl iter
    Parsetypes.
      {tdec_ident; tdec_params; tdec_implicit_params; tdec_desc; tdec_loc} =
  iter.location iter tdec_loc ;
  str iter tdec_ident ;
  List.iter ~f:(iter.ptype_expr iter) tdec_params ;
  List.iter ~f:(iter.ptype_expr iter) tdec_implicit_params ;
  iter.type_decl_desc iter tdec_desc

let type_decl_desc iter = function
  | Parsetypes.Pdec_abstract ->
      ()
  | Pdec_alias typ ->
      iter.ptype_expr iter typ
  | Pdec_unfold typ ->
      iter.ptype_expr iter typ
  | Pdec_record fields ->
      List.iter ~f:(iter.field_decl iter) fields
  | Pdec_variant ctors ->
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Pdec_open ->
      ()
  | Pdec_extend (name, decl, ctors) ->
      path iter name ;
      iter.type0_decl iter decl ;
      List.iter ~f:(iter.ctor_decl iter) ctors

let literal (_iter : iterator) (_ : literal) = ()

let pattern iter {pat_desc; pat_loc; pat_type} =
  iter.location iter pat_loc ;
  iter.type0_expr iter pat_type ;
  iter.pattern_desc iter pat_desc

let pattern_desc iter = function
  | Tpat_any ->
      ()
  | Tpat_variable name ->
      ident iter name
  | Tpat_constraint (pat, typ) ->
      iter.type_expr iter typ ; iter.pattern iter pat
  | Tpat_tuple pats ->
      List.iter ~f:(iter.pattern iter) pats
  | Tpat_or (p1, p2) ->
      iter.pattern iter p1 ; iter.pattern iter p2
  | Tpat_int _ ->
      ()
  | Tpat_record fields ->
      List.iter fields ~f:(fun (name, pat) ->
          path iter name ; iter.pattern iter pat )
  | Tpat_ctor (name, arg) ->
      path iter name ;
      Option.iter ~f:(iter.pattern iter) arg

let expression iter {exp_desc; exp_loc; exp_type} =
  iter.location iter exp_loc ;
  iter.type0_expr iter exp_type ;
  iter.expression_desc iter exp_desc

let expression_desc iter = function
  | Texp_apply (e, args) ->
      iter.expression iter e ;
      List.iter args ~f:(fun (_label, e) -> iter.expression iter e)
  | Texp_variable name ->
      path iter name
  | Texp_literal l ->
      iter.literal iter l
  | Texp_fun (_label, p, e, _explicit) ->
      iter.pattern iter p ; iter.expression iter e
  | Texp_newtype (name, e) ->
      ident iter name ; iter.expression iter e
  | Texp_seq (e1, e2) ->
      iter.expression iter e1 ; iter.expression iter e2
  | Texp_let (p, e1, e2) ->
      iter.pattern iter p ; iter.expression iter e1 ; iter.expression iter e2
  | Texp_constraint (e, typ) ->
      iter.type_expr iter typ ; iter.expression iter e
  | Texp_tuple es ->
      List.iter ~f:(iter.expression iter) es
  | Texp_match (e, cases) ->
      iter.expression iter e ;
      List.iter cases ~f:(fun (p, e) ->
          iter.pattern iter p ; iter.expression iter e )
  | Texp_field (e, name) ->
      path iter name ; iter.expression iter e
  | Texp_record (bindings, default) ->
      Option.iter ~f:(iter.expression iter) default ;
      List.iter bindings ~f:(fun (name, e) ->
          path iter name ; iter.expression iter e )
  | Texp_ctor (name, arg) ->
      path iter name ;
      Option.iter ~f:(iter.expression iter) arg
  | Texp_unifiable {expression; name; id= _} ->
      ident iter name ;
      Option.iter ~f:(iter.expression iter) expression
  | Texp_if (e1, e2, e3) ->
      iter.expression iter e1 ;
      iter.expression iter e2 ;
      Option.iter ~f:(iter.expression iter) e3
  | Texp_prover e ->
      iter.expression iter e

let signature iter = List.iter ~f:(iter.signature_item iter)

let signature_item iter {sig_desc; sig_loc} =
  iter.location iter sig_loc ;
  iter.signature_desc iter sig_desc

let signature_desc iter = function
  | Tsig_value (name, typ) | Tsig_instance (name, typ) ->
      ident iter name ; iter.type_expr iter typ
  | Tsig_type decl ->
      iter.type_decl iter decl
  | Tsig_module (name, msig) | Tsig_modtype (name, msig) ->
      ident iter name ; iter.module_sig iter msig
  | Tsig_open name ->
      path iter name
  | Tsig_typeext (typ, ctors) ->
      iter.pvariant iter typ ;
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Tsig_request (typ, ctor) ->
      iter.ptype_expr iter typ ; iter.ctor_decl iter ctor
  | Tsig_multiple sigs ->
      iter.signature iter sigs
  | Tsig_prover sigs ->
      iter.signature iter sigs

let module_sig iter {msig_desc; msig_loc} =
  iter.location iter msig_loc ;
  iter.module_sig_desc iter msig_desc

let module_sig_desc iter = function
  | Tmty_sig sigs ->
      iter.signature iter sigs
  | Tmty_name name ->
      path iter name
  | Tmty_abstract ->
      ()
  | Tmty_functor (name, fsig, msig) ->
      str iter name ; iter.module_sig iter fsig ; iter.module_sig iter msig

let statements iter = List.iter ~f:(iter.statement iter)

let statement iter {stmt_desc; stmt_loc} =
  iter.location iter stmt_loc ;
  iter.statement_desc iter stmt_desc

let statement_desc iter = function
  | Tstmt_value (p, e) ->
      iter.pattern iter p ; iter.expression iter e
  | Tstmt_instance (name, e) ->
      ident iter name ; iter.expression iter e
  | Tstmt_type decl ->
      iter.type_decl iter decl
  | Tstmt_module (name, me) ->
      ident iter name ; iter.module_expr iter me
  | Tstmt_modtype (name, mty) ->
      ident iter name ; iter.module_sig iter mty
  | Tstmt_open name ->
      path iter name
  | Tstmt_typeext (typ, ctors) ->
      iter.pvariant iter typ ;
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Tstmt_request (typ, ctor, handler) ->
      iter.ptype_expr iter typ ;
      iter.ctor_decl iter ctor ;
      Option.iter handler ~f:(fun (p, e) ->
          Option.iter ~f:(iter.pattern iter) p ;
          iter.expression iter e )
  | Tstmt_multiple stmts ->
      iter.statements iter stmts
  | Tstmt_prover stmts ->
      iter.statements iter stmts

let module_expr iter {mod_desc; mod_loc} =
  iter.location iter mod_loc ;
  iter.module_desc iter mod_desc

let module_desc iter = function
  | Tmod_struct stmts ->
      iter.statements iter stmts
  | Tmod_name name ->
      path iter name
  | Tmod_functor (name, fsig, me) ->
      str iter name ; iter.module_sig iter fsig ; iter.module_expr iter me

let location (_iter : iterator) (_ : Location.t) = ()

let longident iter = function
  | Longident.Lident _ ->
      ()
  | Ldot (l, _) ->
      iter.longident iter l
  | Lapply (l1, l2) ->
      iter.longident iter l1 ; iter.longident iter l2

let path iter = function
  | Path.Pident ident ->
      iter.ident iter ident
  | Path.Pdot (path, _) ->
      iter.path iter path
  | Path.Papply (path1, path2) ->
      iter.path iter path1 ; iter.path iter path2

let ident (_iter : iterator) (_ : Ident.t) = ()

(** Stub. This isn't part of the typedast, so we don't do anything by default.
*)
let type0_decl (_iter : iterator) (_ : Type0.type_decl) = ()

(** Stub. This isn't part of the typedast, so we don't do anything by default.
*)
let type0_expr (_iter : iterator) (_ : Type0.type_expr) = ()

let default_iterator =
  { type_expr
  ; type_desc
  ; variant
  ; ptype_expr
  ; ptype_desc
  ; pvariant
  ; field_decl
  ; ctor_args
  ; ctor_decl
  ; type_decl
  ; type_decl_desc
  ; literal
  ; pattern
  ; pattern_desc
  ; expression
  ; expression_desc
  ; signature_item
  ; signature
  ; signature_desc
  ; module_sig
  ; module_sig_desc
  ; statement
  ; statements
  ; statement_desc
  ; module_expr
  ; module_desc
  ; location
  ; longident
  ; ident
  ; path
  ; type0_decl
  ; type0_expr }
