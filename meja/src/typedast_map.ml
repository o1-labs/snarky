open Core_kernel
open Typedast
open Ast_types

type mapper =
  { type_expr: mapper -> type_expr -> type_expr
  ; type_desc: mapper -> type_desc -> type_desc
  ; variant: mapper -> variant -> variant
  ; row_tag: mapper -> row_tag -> row_tag
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
  ; convert: mapper -> convert -> convert
  ; convert_desc: mapper -> convert_desc -> convert_desc
  ; convert_body: mapper -> convert_body -> convert_body
  ; convert_body_desc: mapper -> convert_body_desc -> convert_body_desc
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
  ; path: mapper -> Path.t -> Path.t
  ; type0: Type0_map.mapper }

let with_backtrack_replace f =
  let snap = Type1.Snapshot.create () in
  let ret = f () in
  Type1.backtrack_replace snap ;
  ret

let lid mapper {Location.txt; loc} =
  {Location.txt= mapper.longident mapper txt; loc= mapper.location mapper loc}

let str mapper ({Location.txt; loc} : str) =
  {Location.txt; loc= mapper.location mapper loc}

let ident mapper ({Location.txt; loc} : Ident.t Location.loc) =
  {Location.txt= mapper.ident mapper txt; loc= mapper.location mapper loc}

let path mapper ({Location.txt; loc} : Path.t Location.loc) =
  {Location.txt= mapper.path mapper txt; loc= mapper.location mapper loc}

let type_expr mapper {type_desc; type_loc; type_type} =
  let type_loc = mapper.location mapper type_loc in
  let type_desc = mapper.type_desc mapper type_desc in
  let type_type =
    with_backtrack_replace (fun () ->
        mapper.type0.type_expr mapper.type0 type_type )
  in
  {type_desc; type_loc; type_type}

let type_desc mapper typ =
  match typ with
  | Ttyp_var name ->
      Ttyp_var (Option.map ~f:(str mapper) name)
  | Ttyp_tuple typs ->
      Ttyp_tuple (List.map ~f:(mapper.type_expr mapper) typs)
  | Ttyp_arrow (typ1, typ2, explicit, label) ->
      Ttyp_arrow
        ( mapper.type_expr mapper typ1
        , mapper.type_expr mapper typ2
        , explicit
        , label )
  | Ttyp_ctor variant ->
      Ttyp_ctor (mapper.variant mapper variant)
  | Ttyp_poly (vars, typ) ->
      Ttyp_poly
        ( List.map ~f:(mapper.type_expr mapper) vars
        , mapper.type_expr mapper typ )
  | Ttyp_prover typ ->
      Ttyp_prover (mapper.type_expr mapper typ)
  | Ttyp_conv (typ1, typ2) ->
      Ttyp_conv (mapper.type_expr mapper typ1, mapper.type_expr mapper typ2)
  | Ttyp_opaque typ ->
      Ttyp_opaque (mapper.type_expr mapper typ)
  | Ttyp_row (tags, closed, min_tags) ->
      Ttyp_row
        ( List.map ~f:(mapper.row_tag mapper) tags
        , closed
        , Option.map ~f:(List.map ~f:(ident mapper)) min_tags )

let variant mapper {var_ident; var_params} =
  { var_ident= path mapper var_ident
  ; var_params= List.map ~f:(mapper.type_expr mapper) var_params }

let row_tag mapper {rtag_ident; rtag_arg; rtag_loc} =
  { rtag_ident= ident mapper rtag_ident
  ; rtag_arg= List.map ~f:(mapper.type_expr mapper) rtag_arg
  ; rtag_loc= mapper.location mapper rtag_loc }

let field_decl mapper {fld_ident; fld_type; fld_loc; fld_fld} =
  { fld_loc= mapper.location mapper fld_loc
  ; fld_ident= ident mapper fld_ident
  ; fld_type= mapper.type_expr mapper fld_type
  ; fld_fld=
      with_backtrack_replace (fun () ->
          mapper.type0.field_decl mapper.type0 fld_fld ) }

let ctor_args mapper = function
  | Tctor_tuple typs ->
      Tctor_tuple (List.map ~f:(mapper.type_expr mapper) typs)
  | Tctor_record fields ->
      Tctor_record (List.map ~f:(mapper.field_decl mapper) fields)

let ctor_decl mapper {ctor_ident; ctor_args; ctor_ret; ctor_loc; ctor_ctor} =
  { ctor_loc= mapper.location mapper ctor_loc
  ; ctor_ident= ident mapper ctor_ident
  ; ctor_args= mapper.ctor_args mapper ctor_args
  ; ctor_ret= Option.map ~f:(mapper.type_expr mapper) ctor_ret
  ; ctor_ctor=
      with_backtrack_replace (fun () ->
          mapper.type0.ctor_decl mapper.type0 ctor_ctor ) }

let type_decl mapper {tdec_ident; tdec_params; tdec_desc; tdec_loc; tdec_tdec}
    =
  { tdec_loc= mapper.location mapper tdec_loc
  ; tdec_ident= ident mapper tdec_ident
  ; tdec_params= List.map ~f:(mapper.type_expr mapper) tdec_params
  ; tdec_desc= mapper.type_decl_desc mapper tdec_desc
  ; tdec_tdec=
      with_backtrack_replace (fun () ->
          mapper.type0.type_decl mapper.type0 tdec_tdec ) }

let type_decl_desc mapper = function
  | Tdec_abstract ->
      Tdec_abstract
  | Tdec_alias typ ->
      Tdec_alias (mapper.type_expr mapper typ)
  | Tdec_record fields ->
      Tdec_record (List.map ~f:(mapper.field_decl mapper) fields)
  | Tdec_variant ctors ->
      Tdec_variant (List.map ~f:(mapper.ctor_decl mapper) ctors)
  | Tdec_open ->
      Tdec_open
  | Tdec_extend (name, ctors) ->
      Tdec_extend
        (path mapper name, List.map ~f:(mapper.ctor_decl mapper) ctors)

let literal (_iter : mapper) (l : literal) = l

let pattern mapper {pat_desc; pat_loc; pat_type} =
  { pat_loc= mapper.location mapper pat_loc
  ; pat_desc= mapper.pattern_desc mapper pat_desc
  ; pat_type=
      with_backtrack_replace (fun () ->
          mapper.type0.type_expr mapper.type0 pat_type ) }

let pattern_desc mapper = function
  | Tpat_any ->
      Tpat_any
  | Tpat_variable name ->
      Tpat_variable (ident mapper name)
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
             (path mapper name, mapper.pattern mapper pat) ))
  | Tpat_ctor (name, arg) ->
      Tpat_ctor (path mapper name, Option.map ~f:(mapper.pattern mapper) arg)
  | Tpat_row_ctor (name, args) ->
      Tpat_row_ctor
        (ident mapper name, List.map ~f:(mapper.pattern mapper) args)

let convert_body mapper {conv_body_desc; conv_body_loc; conv_body_type} =
  { conv_body_loc= mapper.location mapper conv_body_loc
  ; conv_body_desc= mapper.convert_body_desc mapper conv_body_desc
  ; conv_body_type= mapper.type0.type_expr mapper.type0 conv_body_type }

let convert_body_desc mapper = function
  | Tconv_record fields ->
      Tconv_record
        (List.map fields ~f:(fun (name, conv) ->
             (path mapper name, mapper.convert_body mapper conv) ))
  | Tconv_ctor (name, args) ->
      Tconv_ctor
        ( path mapper name
        , List.map args ~f:(fun (label, conv) ->
              (label, mapper.convert_body mapper conv) ) )
  | Tconv_tuple convs ->
      Tconv_tuple (List.map ~f:(mapper.convert_body mapper) convs)
  | Tconv_arrow (conv1, conv2) ->
      Tconv_arrow
        (mapper.convert_body mapper conv1, mapper.convert_body mapper conv2)
  | Tconv_identity ->
      Tconv_identity
  | Tconv_opaque ->
      Tconv_opaque

let convert mapper {conv_desc; conv_loc; conv_type} =
  { conv_loc= mapper.location mapper conv_loc
  ; conv_desc= mapper.convert_desc mapper conv_desc
  ; conv_type= mapper.type0.type_expr mapper.type0 conv_type }

let convert_desc mapper = function
  | Tconv_fun (name, conv) ->
      Tconv_fun (ident mapper name, mapper.convert mapper conv)
  | Tconv_body conv ->
      Tconv_body (mapper.convert_body mapper conv)

let expression mapper {exp_desc; exp_loc; exp_type} =
  { exp_loc= mapper.location mapper exp_loc
  ; exp_desc= mapper.expression_desc mapper exp_desc
  ; exp_type=
      with_backtrack_replace (fun () ->
          mapper.type0.type_expr mapper.type0 exp_type ) }

let expression_desc mapper = function
  | Texp_apply (e, args) ->
      Texp_apply
        ( mapper.expression mapper e
        , List.map args ~f:(fun (explicit, label, e) ->
              (explicit, label, mapper.expression mapper e) ) )
  | Texp_variable name ->
      Texp_variable (path mapper name)
  | Texp_literal l ->
      Texp_literal (mapper.literal mapper l)
  | Texp_fun (label, p, e, explicit) ->
      Texp_fun
        (label, mapper.pattern mapper p, mapper.expression mapper e, explicit)
  | Texp_newtype (name, e) ->
      Texp_newtype (ident mapper name, mapper.expression mapper e)
  | Texp_seq (e1, e2) ->
      Texp_seq (mapper.expression mapper e1, mapper.expression mapper e2)
  | Texp_let (p, e1, e2) ->
      Texp_let
        ( mapper.pattern mapper p
        , mapper.expression mapper e1
        , mapper.expression mapper e2 )
  | Texp_instance (name, e1, e2) ->
      Texp_instance
        ( ident mapper name
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
      Texp_field (mapper.expression mapper e, path mapper name)
  | Texp_record (bindings, default) ->
      Texp_record
        ( List.map bindings ~f:(fun (name, e) ->
              (path mapper name, mapper.expression mapper e) )
        , Option.map ~f:(mapper.expression mapper) default )
  | Texp_ctor (name, arg) ->
      Texp_ctor (path mapper name, Option.map ~f:(mapper.expression mapper) arg)
  | Texp_row_ctor (name, args) ->
      Texp_row_ctor
        (ident mapper name, List.map ~f:(mapper.expression mapper) args)
  | Texp_unifiable {expression; name; id} ->
      Texp_unifiable
        { id
        ; name= ident mapper name
        ; expression= Option.map ~f:(mapper.expression mapper) expression }
  | Texp_if (e1, e2, e3) ->
      Texp_if
        ( mapper.expression mapper e1
        , mapper.expression mapper e2
        , Option.map ~f:(mapper.expression mapper) e3 )
  | Texp_read (conv, conv_args, e) ->
      Texp_read
        ( mapper.convert mapper conv
        , List.map conv_args ~f:(fun (label, e) ->
              (label, mapper.expression mapper e) )
        , mapper.expression mapper e )
  | Texp_prover (conv, conv_args, e) ->
      Texp_prover
        ( mapper.convert mapper conv
        , List.map conv_args ~f:(fun (label, e) ->
              (label, mapper.expression mapper e) )
        , mapper.expression mapper e )
  | Texp_convert conv ->
      Texp_convert (mapper.convert mapper conv)

let type_conv mapper = function
  | Ttconv_with (mode, decl) ->
      Ttconv_with (mode, mapper.type_decl mapper decl)
  | Ttconv_to typ ->
      Ttconv_to (mapper.type_expr mapper typ)

let signature mapper = List.map ~f:(mapper.signature_item mapper)

let signature_item mapper {sig_desc; sig_loc} =
  { sig_loc= mapper.location mapper sig_loc
  ; sig_desc= mapper.signature_desc mapper sig_desc }

let signature_desc mapper = function
  | Tsig_value (name, typ) ->
      Tsig_value (ident mapper name, mapper.type_expr mapper typ)
  | Tsig_instance (name, typ) ->
      Tsig_instance (ident mapper name, mapper.type_expr mapper typ)
  | Tsig_type decl ->
      Tsig_type (mapper.type_decl mapper decl)
  | Tsig_convtype (decl, tconv, convname, typ) ->
      Tsig_convtype
        ( mapper.type_decl mapper decl
        , type_conv mapper tconv
        , ident mapper convname
        , mapper.type_expr mapper typ )
  | Tsig_rectype decls ->
      Tsig_rectype (List.map ~f:(mapper.type_decl mapper) decls)
  | Tsig_module (name, msig) ->
      Tsig_module (ident mapper name, mapper.module_sig mapper msig)
  | Tsig_modtype (name, msig) ->
      Tsig_modtype (ident mapper name, mapper.module_sig mapper msig)
  | Tsig_open name ->
      Tsig_open (path mapper name)
  | Tsig_typeext (typ, ctors) ->
      Tsig_typeext
        (mapper.variant mapper typ, List.map ~f:(mapper.ctor_decl mapper) ctors)
  | Tsig_request (typ, ctor) ->
      Tsig_request (mapper.type_expr mapper typ, mapper.ctor_decl mapper ctor)
  | Tsig_multiple sigs ->
      Tsig_multiple (mapper.signature mapper sigs)
  | Tsig_prover sigs ->
      Tsig_prover (mapper.signature mapper sigs)
  | Tsig_convert (name, typ) ->
      Tsig_convert (ident mapper name, mapper.type_expr mapper typ)

let module_sig mapper {msig_desc; msig_loc} =
  { msig_loc= mapper.location mapper msig_loc
  ; msig_desc= mapper.module_sig_desc mapper msig_desc }

let module_sig_desc mapper = function
  | Tmty_sig sigs ->
      Tmty_sig (mapper.signature mapper sigs)
  | Tmty_name name ->
      Tmty_name (path mapper name)
  | Tmty_alias name ->
      Tmty_alias (path mapper name)
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
      Tstmt_instance (ident mapper name, mapper.expression mapper e)
  | Tstmt_type decl ->
      Tstmt_type (mapper.type_decl mapper decl)
  | Tstmt_convtype (decl, tconv, convname, conv) ->
      Tstmt_convtype
        ( mapper.type_decl mapper decl
        , type_conv mapper tconv
        , ident mapper convname
        , mapper.convert mapper conv )
  | Tstmt_rectype decls ->
      Tstmt_rectype (List.map ~f:(mapper.type_decl mapper) decls)
  | Tstmt_module (name, me) ->
      Tstmt_module (ident mapper name, mapper.module_expr mapper me)
  | Tstmt_modtype (name, mty) ->
      Tstmt_modtype (ident mapper name, mapper.module_sig mapper mty)
  | Tstmt_open name ->
      Tstmt_open (path mapper name)
  | Tstmt_open_instance name ->
      Tstmt_open_instance (path mapper name)
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
  | Tstmt_prover stmts ->
      Tstmt_prover (mapper.statements mapper stmts)
  | Tstmt_convert (name, typ, conv) ->
      Tstmt_convert
        ( ident mapper name
        , mapper.type_expr mapper typ
        , mapper.convert mapper conv )

let module_expr mapper {mod_desc; mod_loc} =
  { mod_loc= mapper.location mapper mod_loc
  ; mod_desc= mapper.module_desc mapper mod_desc }

let module_desc mapper = function
  | Tmod_struct stmts ->
      Tmod_struct (mapper.statements mapper stmts)
  | Tmod_name name ->
      Tmod_name (path mapper name)
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

let path mapper = function
  | Path.Pident ident ->
      Path.Pident (mapper.ident mapper ident)
  | Path.Pdot (path, mode, str) ->
      Path.Pdot (mapper.path mapper path, mode, str)
  | Path.Papply (path1, path2) ->
      Path.Papply (mapper.path mapper path1, mapper.path mapper path2)

let ident (_mapper : mapper) (ident : Ident.t) = ident

let default_iterator =
  { type_expr
  ; type_desc
  ; variant
  ; row_tag
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
  ; type0= Type0_map.default_mapper }
