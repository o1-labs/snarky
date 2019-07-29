open Core_kernel
open Typedast
open Ast_types

type mapper =
  { type_expr: mapper -> Parsetypes.type_expr -> Parsetypes.type_expr
  ; type_desc: mapper -> Parsetypes.type_desc -> Parsetypes.type_desc
  ; variant: mapper -> Parsetypes.variant -> Parsetypes.variant
  ; field_decl: mapper -> Parsetypes.field_decl -> Parsetypes.field_decl
  ; ctor_args: mapper -> Parsetypes.ctor_args -> Parsetypes.ctor_args
  ; ctor_decl: mapper -> Parsetypes.ctor_decl -> Parsetypes.ctor_decl
  ; type_decl: mapper -> Parsetypes.type_decl -> Parsetypes.type_decl
  ; type_decl_desc:
      mapper -> Parsetypes.type_decl_desc -> Parsetypes.type_decl_desc
  ; literal: mapper -> literal -> literal
  ; pattern: mapper -> pattern -> pattern
  ; pattern_desc: mapper -> pattern_desc -> pattern_desc
  ; expression: mapper -> expression -> expression
  ; expression_desc: mapper -> expression_desc -> expression_desc
  ; signature_item: mapper -> signature_item -> signature_item
  ; signature: mapper -> signature -> signature
  ; signature_desc: mapper -> signature_desc -> signature_desc
  ; module_sig: mapper -> module_sig -> module_sig
  ; module_sig_desc: mapper -> module_sig_desc -> module_sig_desc
  ; statement: mapper -> statement -> statement
  ; statements: mapper -> statements -> statements
  ; statement_desc: mapper -> statement_desc -> statement_desc
  ; module_expr: mapper -> module_expr -> module_expr
  ; module_desc: mapper -> module_desc -> module_desc
  ; location: mapper -> Location.t -> Location.t
  ; longident: mapper -> Longident.t -> Longident.t
  ; ident: mapper -> Ident.t -> Ident.t
  ; type0: Type0_map.mapper }

let lid mapper {Location.txt; loc} =
  {Location.txt= mapper.longident mapper txt; loc= mapper.location mapper loc}

let str mapper ({Location.txt; loc} : str) =
  {Location.txt; loc= mapper.location mapper loc}

let ident mapper ({Location.txt; loc} : Ident.t Location.loc) =
  {Location.txt= mapper.ident mapper txt; loc= mapper.location mapper loc}

let type_expr mapper Parsetypes.{type_desc; type_loc} =
  let type_loc = mapper.location mapper type_loc in
  let type_desc = mapper.type_desc mapper type_desc in
  {Parsetypes.type_desc; type_loc}

let type_desc mapper typ =
  match typ with
  | Parsetypes.Ptyp_var (name, explicit) ->
      Parsetypes.Ptyp_var (Option.map ~f:(str mapper) name, explicit)
  | Ptyp_tuple typs ->
      Ptyp_tuple (List.map ~f:(mapper.type_expr mapper) typs)
  | Ptyp_arrow (typ1, typ2, explicit, label) ->
      Ptyp_arrow
        ( mapper.type_expr mapper typ1
        , mapper.type_expr mapper typ2
        , explicit
        , label )
  | Ptyp_ctor variant ->
      Ptyp_ctor (mapper.variant mapper variant)
  | Ptyp_poly (vars, typ) ->
      Ptyp_poly
        ( List.map ~f:(mapper.type_expr mapper) vars
        , mapper.type_expr mapper typ )

let variant mapper Parsetypes.{var_ident; var_params; var_implicit_params} =
  { Parsetypes.var_ident= lid mapper var_ident
  ; var_params= List.map ~f:(mapper.type_expr mapper) var_params
  ; var_implicit_params=
      List.map ~f:(mapper.type_expr mapper) var_implicit_params }

let field_decl mapper Parsetypes.{fld_ident; fld_type; fld_loc} =
  { Parsetypes.fld_loc= mapper.location mapper fld_loc
  ; fld_ident= str mapper fld_ident
  ; fld_type= mapper.type_expr mapper fld_type }

let ctor_args mapper = function
  | Parsetypes.Ctor_tuple typs ->
      Parsetypes.Ctor_tuple (List.map ~f:(mapper.type_expr mapper) typs)
  | Ctor_record fields ->
      Ctor_record (List.map ~f:(mapper.field_decl mapper) fields)

let ctor_decl mapper Parsetypes.{ctor_ident; ctor_args; ctor_ret; ctor_loc} =
  { Parsetypes.ctor_loc= mapper.location mapper ctor_loc
  ; ctor_ident= str mapper ctor_ident
  ; ctor_args= mapper.ctor_args mapper ctor_args
  ; ctor_ret= Option.map ~f:(mapper.type_expr mapper) ctor_ret }

let type_decl mapper
    Parsetypes.
      {tdec_ident; tdec_params; tdec_implicit_params; tdec_desc; tdec_loc} =
  { Parsetypes.tdec_loc= mapper.location mapper tdec_loc
  ; tdec_ident= str mapper tdec_ident
  ; tdec_params= List.map ~f:(mapper.type_expr mapper) tdec_params
  ; tdec_implicit_params=
      List.map ~f:(mapper.type_expr mapper) tdec_implicit_params
  ; tdec_desc= mapper.type_decl_desc mapper tdec_desc }

let type_decl_desc mapper = function
  | Parsetypes.TAbstract ->
      Parsetypes.TAbstract
  | TAlias typ ->
      TAlias (mapper.type_expr mapper typ)
  | TUnfold typ ->
      TUnfold (mapper.type_expr mapper typ)
  | TRecord fields ->
      TRecord (List.map ~f:(mapper.field_decl mapper) fields)
  | TVariant ctors ->
      TVariant (List.map ~f:(mapper.ctor_decl mapper) ctors)
  | TOpen ->
      TOpen
  | TExtend (name, decl, ctors) ->
      TExtend
        ( lid mapper name
        , mapper.type0.type_decl mapper.type0 decl
        , List.map ~f:(mapper.ctor_decl mapper) ctors )
  | TForward i ->
      TForward i

let literal (_iter : mapper) (l : literal) = l

let pattern mapper {pat_desc; pat_loc; pat_type} =
  { pat_loc= mapper.location mapper pat_loc
  ; pat_desc= mapper.pattern_desc mapper pat_desc
  ; pat_type= mapper.type0.type_expr mapper.type0 pat_type }

let pattern_desc mapper = function
  | Tpat_any ->
      Tpat_any
  | Tpat_variable name ->
      Tpat_variable (str mapper name)
  | Tpat_constraint (pat, typ) ->
      Tpat_constraint (mapper.pattern mapper pat, mapper.type_expr mapper typ)
  | Tpat_tuple pats ->
      Tpat_tuple (List.map ~f:(mapper.pattern mapper) pats)
  | Tpat_or (p1, p2) ->
      Tpat_or (mapper.pattern mapper p1, mapper.pattern mapper p2)
  | Tpat_int i ->
      Tpat_int i
  | Tpat_record fields ->
      Tpat_record
        (List.map fields ~f:(fun (name, pat) ->
             (lid mapper name, mapper.pattern mapper pat) ))
  | Tpat_ctor (name, arg) ->
      Tpat_ctor (lid mapper name, Option.map ~f:(mapper.pattern mapper) arg)

let expression mapper {exp_desc; exp_loc; exp_type} =
  { exp_loc= mapper.location mapper exp_loc
  ; exp_desc= mapper.expression_desc mapper exp_desc
  ; exp_type= mapper.type0.type_expr mapper.type0 exp_type }

let expression_desc mapper = function
  | Texp_apply (e, args) ->
      Texp_apply
        ( mapper.expression mapper e
        , List.map args ~f:(fun (label, e) ->
              (label, mapper.expression mapper e) ) )
  | Texp_variable name ->
      Texp_variable (lid mapper name)
  | Texp_literal l ->
      Texp_literal (mapper.literal mapper l)
  | Texp_fun (label, p, e, explicit) ->
      Texp_fun
        (label, mapper.pattern mapper p, mapper.expression mapper e, explicit)
  | Texp_newtype (name, e) ->
      Texp_newtype (str mapper name, mapper.expression mapper e)
  | Texp_seq (e1, e2) ->
      Texp_seq (mapper.expression mapper e1, mapper.expression mapper e2)
  | Texp_let (p, e1, e2) ->
      Texp_let
        ( mapper.pattern mapper p
        , mapper.expression mapper e1
        , mapper.expression mapper e2 )
  | Texp_constraint (e, typ) ->
      Texp_constraint (mapper.expression mapper e, mapper.type_expr mapper typ)
  | Texp_tuple es ->
      Texp_tuple (List.map ~f:(mapper.expression mapper) es)
  | Texp_match (e, cases) ->
      Texp_match
        ( mapper.expression mapper e
        , List.map cases ~f:(fun (p, e) ->
              (mapper.pattern mapper p, mapper.expression mapper e) ) )
  | Texp_field (e, name) ->
      Texp_field (mapper.expression mapper e, lid mapper name)
  | Texp_record (bindings, default) ->
      Texp_record
        ( List.map bindings ~f:(fun (name, e) ->
              (lid mapper name, mapper.expression mapper e) )
        , Option.map ~f:(mapper.expression mapper) default )
  | Texp_ctor (name, arg) ->
      Texp_ctor (lid mapper name, Option.map ~f:(mapper.expression mapper) arg)
  | Texp_unifiable {expression; name; id} ->
      Texp_unifiable
        { id
        ; name= str mapper name
        ; expression= Option.map ~f:(mapper.expression mapper) expression }
  | Texp_if (e1, e2, e3) ->
      Texp_if
        ( mapper.expression mapper e1
        , mapper.expression mapper e2
        , Option.map ~f:(mapper.expression mapper) e3 )

let signature mapper = List.map ~f:(mapper.signature_item mapper)

let signature_item mapper {sig_desc; sig_loc} =
  { sig_loc= mapper.location mapper sig_loc
  ; sig_desc= mapper.signature_desc mapper sig_desc }

let signature_desc mapper = function
  | Tsig_value (name, typ) ->
      Tsig_value (str mapper name, mapper.type_expr mapper typ)
  | Tsig_instance (name, typ) ->
      Tsig_instance (str mapper name, mapper.type_expr mapper typ)
  | Tsig_type decl ->
      Tsig_type (mapper.type_decl mapper decl)
  | Tsig_module (name, msig) ->
      Tsig_module (ident mapper name, mapper.module_sig mapper msig)
  | Tsig_modtype (name, msig) ->
      Tsig_modtype (ident mapper name, mapper.module_sig mapper msig)
  | Tsig_open name ->
      Tsig_open (lid mapper name)
  | Tsig_typeext (typ, ctors) ->
      Tsig_typeext
        (mapper.variant mapper typ, List.map ~f:(mapper.ctor_decl mapper) ctors)
  | Tsig_request (typ, ctor) ->
      Tsig_request (mapper.type_expr mapper typ, mapper.ctor_decl mapper ctor)
  | Tsig_multiple sigs ->
      Tsig_multiple (mapper.signature mapper sigs)

let module_sig mapper {msig_desc; msig_loc} =
  { msig_loc= mapper.location mapper msig_loc
  ; msig_desc= mapper.module_sig_desc mapper msig_desc }

let module_sig_desc mapper = function
  | Tmty_sig sigs ->
      Tmty_sig (mapper.signature mapper sigs)
  | Tmty_name name ->
      Tmty_name (lid mapper name)
  | Tmty_abstract ->
      Tmty_abstract
  | Tmty_functor (name, fsig, msig) ->
      Tmty_functor
        ( str mapper name
        , mapper.module_sig mapper fsig
        , mapper.module_sig mapper msig )

let statements mapper = List.map ~f:(mapper.statement mapper)

let statement mapper {stmt_desc; stmt_loc} =
  { stmt_loc= mapper.location mapper stmt_loc
  ; stmt_desc= mapper.statement_desc mapper stmt_desc }

let statement_desc mapper = function
  | Tstmt_value (p, e) ->
      Tstmt_value (mapper.pattern mapper p, mapper.expression mapper e)
  | Tstmt_instance (name, e) ->
      Tstmt_instance (str mapper name, mapper.expression mapper e)
  | Tstmt_type decl ->
      Tstmt_type (mapper.type_decl mapper decl)
  | Tstmt_module (name, me) ->
      Tstmt_module (ident mapper name, mapper.module_expr mapper me)
  | Tstmt_modtype (name, mty) ->
      Tstmt_modtype (ident mapper name, mapper.module_sig mapper mty)
  | Tstmt_open name ->
      Tstmt_open (lid mapper name)
  | Tstmt_typeext (typ, ctors) ->
      Tstmt_typeext
        (mapper.variant mapper typ, List.map ~f:(mapper.ctor_decl mapper) ctors)
  | Tstmt_request (typ, ctor, handler) ->
      Tstmt_request
        ( mapper.type_expr mapper typ
        , mapper.ctor_decl mapper ctor
        , Option.map handler ~f:(fun (p, e) ->
              ( Option.map ~f:(mapper.pattern mapper) p
              , mapper.expression mapper e ) ) )
  | Tstmt_multiple stmts ->
      Tstmt_multiple (mapper.statements mapper stmts)

let module_expr mapper {mod_desc; mod_loc} =
  { mod_loc= mapper.location mapper mod_loc
  ; mod_desc= mapper.module_desc mapper mod_desc }

let module_desc mapper = function
  | Tmod_struct stmts ->
      Tmod_struct (mapper.statements mapper stmts)
  | Tmod_name name ->
      Tmod_name (lid mapper name)
  | Tmod_functor (name, fsig, me) ->
      Tmod_functor
        ( str mapper name
        , mapper.module_sig mapper fsig
        , mapper.module_expr mapper me )

let location (_mapper : mapper) (loc : Location.t) = loc

let longident mapper = function
  | Longident.Lident str ->
      Longident.Lident str
  | Ldot (l, str) ->
      Ldot (mapper.longident mapper l, str)
  | Lapply (l1, l2) ->
      Lapply (mapper.longident mapper l1, mapper.longident mapper l2)

let ident (_mapper : mapper) (ident : Ident.t) = ident

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
  ; type0= Type0_map.default_mapper }
