open Core_kernel
open Typedast
open Ast_types

type iterator =
  { type_expr: iterator -> type_expr -> unit
  ; type_desc: iterator -> type_desc -> unit
  ; variant: iterator -> variant -> unit
  ; field_decl: iterator -> field_decl -> unit
  ; ctor_args: iterator -> ctor_args -> unit
  ; ctor_decl: iterator -> ctor_decl -> unit
  ; type_decl: iterator -> type_decl -> unit
  ; type_decl_desc: iterator -> type_decl_desc -> unit
  ; literal: iterator -> literal -> unit
  ; pattern: iterator -> pattern -> unit
  ; pattern_desc: iterator -> pattern_desc -> unit
  ; expression: iterator -> expression -> unit
  ; expression_desc: iterator -> expression_desc -> unit
  ; convert_body: iterator -> convert_body -> unit
  ; convert_body_desc: iterator -> convert_body_desc -> unit
  ; convert: iterator -> convert -> unit
  ; convert_desc: iterator -> convert_desc -> unit
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
  | Ttyp_var name ->
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
  | Ttyp_prover typ ->
      iter.type_expr iter typ
  | Ttyp_conv (typ1, typ2) ->
      iter.type_expr iter typ1 ; iter.type_expr iter typ2
  | Ttyp_opaque typ ->
      iter.type_expr iter typ

let variant iter {var_ident; var_params} =
  path iter var_ident ;
  List.iter ~f:(iter.type_expr iter) var_params

let field_decl iter {fld_ident; fld_type; fld_loc; fld_fld} =
  iter.location iter fld_loc ;
  ident iter fld_ident ;
  iter.type_expr iter fld_type ;
  (* TODO: Type0_iterator *)
  ignore fld_fld

let ctor_args iter = function
  | Tctor_tuple typs ->
      List.iter ~f:(iter.type_expr iter) typs
  | Tctor_record fields ->
      List.iter ~f:(iter.field_decl iter) fields

let ctor_decl iter {ctor_ident; ctor_args; ctor_ret; ctor_loc; ctor_ctor} =
  iter.location iter ctor_loc ;
  ident iter ctor_ident ;
  iter.ctor_args iter ctor_args ;
  Option.iter ~f:(iter.type_expr iter) ctor_ret ;
  (* TODO: Type0_iterator *)
  ignore ctor_ctor

let type_decl iter {tdec_ident; tdec_params; tdec_desc; tdec_loc; tdec_tdec} =
  iter.location iter tdec_loc ;
  ident iter tdec_ident ;
  List.iter ~f:(iter.type_expr iter) tdec_params ;
  iter.type_decl_desc iter tdec_desc ;
  (* TODO: Type0_iterator *)
  ignore tdec_tdec

let type_decl_desc iter = function
  | Tdec_abstract ->
      ()
  | Tdec_alias typ ->
      iter.type_expr iter typ
  | Tdec_record fields ->
      List.iter ~f:(iter.field_decl iter) fields
  | Tdec_variant ctors ->
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Tdec_open ->
      ()
  | Tdec_extend (name, ctors) ->
      path iter name ;
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
      List.iter args ~f:(fun (_explicit, _label, e) -> iter.expression iter e)
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
  | Texp_instance (name, e1, e2) ->
      ident iter name ; iter.expression iter e1 ; iter.expression iter e2
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
  | Texp_read (conv, conv_args, e) ->
      iter.convert iter conv ;
      List.iter conv_args ~f:(fun (_lbl, e) -> iter.expression iter e) ;
      iter.expression iter e
  | Texp_prover (conv, conv_args, e) ->
      iter.convert iter conv ;
      List.iter conv_args ~f:(fun (_lbl, e) -> iter.expression iter e) ;
      iter.expression iter e
  | Texp_convert conv ->
      iter.convert iter conv

let convert_body iter {conv_body_desc; conv_body_loc; conv_body_type} =
  iter.location iter conv_body_loc ;
  iter.type0_expr iter conv_body_type ;
  iter.convert_body_desc iter conv_body_desc

let convert_body_desc iter = function
  | Tconv_record fields ->
      List.iter fields ~f:(fun (field, conv) ->
          path iter field ;
          iter.convert_body iter conv )
  | Tconv_ctor (name, args) ->
      path iter name ;
      List.iter args ~f:(fun (_label, conv) -> iter.convert_body iter conv)
  | Tconv_tuple convs ->
      List.iter ~f:(iter.convert_body iter) convs
  | Tconv_arrow (conv1, conv2) ->
      iter.convert_body iter conv1 ;
      iter.convert_body iter conv2
  | Tconv_identity | Tconv_opaque ->
      ()

let convert iter {conv_desc; conv_loc; conv_type} =
  iter.location iter conv_loc ;
  iter.type0_expr iter conv_type ;
  iter.convert_desc iter conv_desc

let convert_desc iter = function
  | Tconv_fun (name, body) ->
      ident iter name ; iter.convert iter body
  | Tconv_body body ->
      iter.convert_body iter body

let type_conv iter = function
  | Ttconv_with (_mode, decl) ->
      iter.type_decl iter decl
  | Ttconv_to typ ->
      iter.type_expr iter typ

let signature iter = List.iter ~f:(iter.signature_item iter)

let signature_item iter {sig_desc; sig_loc} =
  iter.location iter sig_loc ;
  iter.signature_desc iter sig_desc

let signature_desc iter = function
  | Tsig_value (name, typ) | Tsig_instance (name, typ) ->
      ident iter name ; iter.type_expr iter typ
  | Tsig_type decl ->
      iter.type_decl iter decl
  | Tsig_convtype (decl, tconv, convname, typ) ->
      iter.type_decl iter decl ;
      type_conv iter tconv ;
      ident iter convname ;
      iter.type_expr iter typ
  | Tsig_rectype decls ->
      List.iter ~f:(iter.type_decl iter) decls
  | Tsig_module (name, msig) | Tsig_modtype (name, msig) ->
      ident iter name ; iter.module_sig iter msig
  | Tsig_open name ->
      path iter name
  | Tsig_typeext (typ, ctors) ->
      iter.variant iter typ ;
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Tsig_request (typ, ctor) ->
      iter.type_expr iter typ ; iter.ctor_decl iter ctor
  | Tsig_multiple sigs ->
      iter.signature iter sigs
  | Tsig_prover sigs ->
      iter.signature iter sigs
  | Tsig_convert (name, typ) ->
      ident iter name ; iter.type_expr iter typ

let module_sig iter {msig_desc; msig_loc} =
  iter.location iter msig_loc ;
  iter.module_sig_desc iter msig_desc

let module_sig_desc iter = function
  | Tmty_sig sigs ->
      iter.signature iter sigs
  | Tmty_name name ->
      path iter name
  | Tmty_alias name ->
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
  | Tstmt_convtype (decl, tconv, convname, conv) ->
      iter.type_decl iter decl ;
      type_conv iter tconv ;
      ident iter convname ;
      iter.convert iter conv
  | Tstmt_rectype decls ->
      List.iter ~f:(iter.type_decl iter) decls
  | Tstmt_module (name, me) ->
      ident iter name ; iter.module_expr iter me
  | Tstmt_modtype (name, mty) ->
      ident iter name ; iter.module_sig iter mty
  | Tstmt_open name ->
      path iter name
  | Tstmt_open_instance name ->
      path iter name
  | Tstmt_typeext (typ, ctors) ->
      iter.variant iter typ ;
      List.iter ~f:(iter.ctor_decl iter) ctors
  | Tstmt_request (typ, ctor, handler) ->
      iter.type_expr iter typ ;
      iter.ctor_decl iter ctor ;
      Option.iter handler ~f:(fun (p, e) ->
          Option.iter ~f:(iter.pattern iter) p ;
          iter.expression iter e )
  | Tstmt_multiple stmts ->
      iter.statements iter stmts
  | Tstmt_prover stmts ->
      iter.statements iter stmts
  | Tstmt_convert (name, typ, conv) ->
      ident iter name ; iter.type_expr iter typ ; iter.convert iter conv

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
  | Path.Pdot (path, _, _) ->
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
  ; convert_body
  ; convert_body_desc
  ; convert
  ; convert_desc
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
