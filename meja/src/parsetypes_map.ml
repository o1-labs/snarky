open Core_kernel
open Parsetypes
open Ast_types

type mapper =
  { type_expr: mapper -> type_expr -> type_expr
  ; type_desc: mapper -> type_desc -> type_desc
  ; variant: mapper -> variant -> variant
  ; field_decl: mapper -> field_decl -> field_decl
  ; ctor_args: mapper -> ctor_args -> ctor_args
  ; ctor_decl: mapper -> ctor_decl -> ctor_decl
  ; type_decl: mapper -> type_decl -> type_decl
  ; type_decl_desc: mapper -> type_decl_desc -> type_decl_desc
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
  ; type0: Type0_map.mapper }

let lid mapper {Location.txt; loc} =
  {Location.txt= mapper.longident mapper txt; loc= mapper.location mapper loc}

let str mapper ({Location.txt; loc} : str) =
  {Location.txt; loc= mapper.location mapper loc}

let path mapper ({Location.txt; loc} : Path.t Location.loc) =
  { Location.txt= mapper.type0.path mapper.type0 txt
  ; loc= mapper.location mapper loc }

let type_expr mapper {type_desc; type_loc} =
  let type_loc = mapper.location mapper type_loc in
  let type_desc = mapper.type_desc mapper type_desc in
  {type_desc; type_loc}

let type_desc mapper typ =
  match typ with
  | Ptyp_var (name, explicit) ->
      Ptyp_var (Option.map ~f:(str mapper) name, explicit)
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

let variant mapper {var_ident; var_params} =
  { var_ident= lid mapper var_ident
  ; var_params= List.map ~f:(mapper.type_expr mapper) var_params }

let field_decl mapper {fld_ident; fld_type; fld_loc} =
  { fld_loc= mapper.location mapper fld_loc
  ; fld_ident= str mapper fld_ident
  ; fld_type= mapper.type_expr mapper fld_type }

let ctor_args mapper = function
  | Ctor_tuple typs ->
      Ctor_tuple (List.map ~f:(mapper.type_expr mapper) typs)
  | Ctor_record fields ->
      Ctor_record (List.map ~f:(mapper.field_decl mapper) fields)

let ctor_decl mapper {ctor_ident; ctor_args; ctor_ret; ctor_loc} =
  { ctor_loc= mapper.location mapper ctor_loc
  ; ctor_ident= str mapper ctor_ident
  ; ctor_args= mapper.ctor_args mapper ctor_args
  ; ctor_ret= Option.map ~f:(mapper.type_expr mapper) ctor_ret }

let type_decl mapper {tdec_ident; tdec_params; tdec_desc; tdec_loc} =
  { tdec_loc= mapper.location mapper tdec_loc
  ; tdec_ident= str mapper tdec_ident
  ; tdec_params= List.map ~f:(mapper.type_expr mapper) tdec_params
  ; tdec_desc= mapper.type_decl_desc mapper tdec_desc }

let type_decl_desc mapper = function
  | Pdec_abstract ->
      Pdec_abstract
  | Pdec_alias typ ->
      Pdec_alias (mapper.type_expr mapper typ)
  | Pdec_record fields ->
      Pdec_record (List.map ~f:(mapper.field_decl mapper) fields)
  | Pdec_variant ctors ->
      Pdec_variant (List.map ~f:(mapper.ctor_decl mapper) ctors)
  | Pdec_open ->
      Pdec_open
  | Pdec_extend (name, decl, ctors) ->
      Pdec_extend
        ( path mapper name
        , mapper.type0.type_decl mapper.type0 decl
        , List.map ~f:(mapper.ctor_decl mapper) ctors )

let literal (_iter : mapper) (l : literal) = l

let pattern mapper {pat_desc; pat_loc} =
  { pat_loc= mapper.location mapper pat_loc
  ; pat_desc= mapper.pattern_desc mapper pat_desc }

let pattern_desc mapper = function
  | Ppat_any ->
      Ppat_any
  | Ppat_variable name ->
      Ppat_variable (str mapper name)
  | Ppat_constraint (pat, typ) ->
      Ppat_constraint (mapper.pattern mapper pat, mapper.type_expr mapper typ)
  | Ppat_tuple pats ->
      Ppat_tuple (List.map ~f:(mapper.pattern mapper) pats)
  | Ppat_or (p1, p2) ->
      Ppat_or (mapper.pattern mapper p1, mapper.pattern mapper p2)
  | Ppat_int i ->
      Ppat_int i
  | Ppat_record fields ->
      Ppat_record
        (List.map fields ~f:(fun (name, pat) ->
             (lid mapper name, mapper.pattern mapper pat) ))
  | Ppat_ctor (name, arg) ->
      Ppat_ctor (lid mapper name, Option.map ~f:(mapper.pattern mapper) arg)

let expression mapper {exp_desc; exp_loc} =
  { exp_loc= mapper.location mapper exp_loc
  ; exp_desc= mapper.expression_desc mapper exp_desc }

let expression_desc mapper = function
  | Pexp_apply (e, args) ->
      Pexp_apply
        ( mapper.expression mapper e
        , List.map args ~f:(fun (label, e) ->
              (label, mapper.expression mapper e) ) )
  | Pexp_variable name ->
      Pexp_variable (lid mapper name)
  | Pexp_literal l ->
      Pexp_literal (mapper.literal mapper l)
  | Pexp_fun (label, p, e, explicit) ->
      Pexp_fun
        (label, mapper.pattern mapper p, mapper.expression mapper e, explicit)
  | Pexp_newtype (name, e) ->
      Pexp_newtype (str mapper name, mapper.expression mapper e)
  | Pexp_seq (e1, e2) ->
      Pexp_seq (mapper.expression mapper e1, mapper.expression mapper e2)
  | Pexp_let (p, e1, e2) ->
      Pexp_let
        ( mapper.pattern mapper p
        , mapper.expression mapper e1
        , mapper.expression mapper e2 )
  | Pexp_constraint (e, typ) ->
      Pexp_constraint (mapper.expression mapper e, mapper.type_expr mapper typ)
  | Pexp_tuple es ->
      Pexp_tuple (List.map ~f:(mapper.expression mapper) es)
  | Pexp_match (e, cases) ->
      Pexp_match
        ( mapper.expression mapper e
        , List.map cases ~f:(fun (p, e) ->
              (mapper.pattern mapper p, mapper.expression mapper e) ) )
  | Pexp_field (e, name) ->
      Pexp_field (mapper.expression mapper e, lid mapper name)
  | Pexp_record (bindings, default) ->
      Pexp_record
        ( List.map bindings ~f:(fun (name, e) ->
              (lid mapper name, mapper.expression mapper e) )
        , Option.map ~f:(mapper.expression mapper) default )
  | Pexp_ctor (name, arg) ->
      Pexp_ctor (lid mapper name, Option.map ~f:(mapper.expression mapper) arg)
  | Pexp_unifiable {expression; name; id} ->
      Pexp_unifiable
        { id
        ; name= str mapper name
        ; expression= Option.map ~f:(mapper.expression mapper) expression }
  | Pexp_if (e1, e2, e3) ->
      Pexp_if
        ( mapper.expression mapper e1
        , mapper.expression mapper e2
        , Option.map ~f:(mapper.expression mapper) e3 )
  | Pexp_prover e ->
      Pexp_prover (mapper.expression mapper e)

let signature mapper = List.map ~f:(mapper.signature_item mapper)

let signature_item mapper {sig_desc; sig_loc} =
  { sig_loc= mapper.location mapper sig_loc
  ; sig_desc= mapper.signature_desc mapper sig_desc }

let signature_desc mapper = function
  | Psig_value (name, typ) ->
      Psig_value (str mapper name, mapper.type_expr mapper typ)
  | Psig_instance (name, typ) ->
      Psig_instance (str mapper name, mapper.type_expr mapper typ)
  | Psig_type decl ->
      Psig_type (mapper.type_decl mapper decl)
  | Psig_module (name, msig) ->
      Psig_module (str mapper name, mapper.module_sig mapper msig)
  | Psig_modtype (name, msig) ->
      Psig_modtype (str mapper name, mapper.module_sig mapper msig)
  | Psig_open name ->
      Psig_open (lid mapper name)
  | Psig_typeext (typ, ctors) ->
      Psig_typeext
        (mapper.variant mapper typ, List.map ~f:(mapper.ctor_decl mapper) ctors)
  | Psig_request (typ, ctor) ->
      Psig_request (mapper.type_expr mapper typ, mapper.ctor_decl mapper ctor)
  | Psig_multiple sigs ->
      Psig_multiple (mapper.signature mapper sigs)
  | Psig_prover sigs ->
      Psig_prover (mapper.signature mapper sigs)

let module_sig mapper {msig_desc; msig_loc} =
  { msig_loc= mapper.location mapper msig_loc
  ; msig_desc= mapper.module_sig_desc mapper msig_desc }

let module_sig_desc mapper = function
  | Pmty_sig sigs ->
      Pmty_sig (mapper.signature mapper sigs)
  | Pmty_name name ->
      Pmty_name (lid mapper name)
  | Pmty_abstract ->
      Pmty_abstract
  | Pmty_functor (name, fsig, msig) ->
      Pmty_functor
        ( str mapper name
        , mapper.module_sig mapper fsig
        , mapper.module_sig mapper msig )

let statements mapper = List.map ~f:(mapper.statement mapper)

let statement mapper {stmt_desc; stmt_loc} =
  { stmt_loc= mapper.location mapper stmt_loc
  ; stmt_desc= mapper.statement_desc mapper stmt_desc }

let statement_desc mapper = function
  | Pstmt_value (p, e) ->
      Pstmt_value (mapper.pattern mapper p, mapper.expression mapper e)
  | Pstmt_instance (name, e) ->
      Pstmt_instance (str mapper name, mapper.expression mapper e)
  | Pstmt_type decl ->
      Pstmt_type (mapper.type_decl mapper decl)
  | Pstmt_module (name, me) ->
      Pstmt_module (str mapper name, mapper.module_expr mapper me)
  | Pstmt_modtype (name, mty) ->
      Pstmt_modtype (str mapper name, mapper.module_sig mapper mty)
  | Pstmt_open name ->
      Pstmt_open (lid mapper name)
  | Pstmt_typeext (typ, ctors) ->
      Pstmt_typeext
        (mapper.variant mapper typ, List.map ~f:(mapper.ctor_decl mapper) ctors)
  | Pstmt_request (typ, ctor, handler) ->
      Pstmt_request
        ( mapper.type_expr mapper typ
        , mapper.ctor_decl mapper ctor
        , Option.map handler ~f:(fun (p, e) ->
              ( Option.map ~f:(mapper.pattern mapper) p
              , mapper.expression mapper e ) ) )
  | Pstmt_multiple stmts ->
      Pstmt_multiple (mapper.statements mapper stmts)
  | Pstmt_prover stmts ->
      Pstmt_prover (mapper.statements mapper stmts)

let module_expr mapper {mod_desc; mod_loc} =
  { mod_loc= mapper.location mapper mod_loc
  ; mod_desc= mapper.module_desc mapper mod_desc }

let module_desc mapper = function
  | Pmod_struct stmts ->
      Pmod_struct (mapper.statements mapper stmts)
  | Pmod_name name ->
      Pmod_name (lid mapper name)
  | Pmod_functor (name, fsig, me) ->
      Pmod_functor
        ( str mapper name
        , mapper.module_sig mapper fsig
        , mapper.module_expr mapper me )

let location (_iter : mapper) (loc : Location.t) = loc

let longident mapper = function
  | Longident.Lident str ->
      Longident.Lident str
  | Ldot (l, str) ->
      Ldot (mapper.longident mapper l, str)
  | Lapply (l1, l2) ->
      Lapply (mapper.longident mapper l1, mapper.longident mapper l2)

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
  ; type0= Type0_map.default_mapper }
