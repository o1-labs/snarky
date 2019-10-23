open Core_kernel
open Asttypes
open Meja_lib.Ast_types
open Ast_helper
open Meja_lib.Typedast

let of_ident = Meja_lib.Ident.name

let of_ident_loc = map_loc ~f:of_ident

let of_path = Meja_lib.Path.to_longident

let of_path_loc = map_loc ~f:of_path

let mk_loc ?loc x =
  match loc with
  | Some loc ->
      Location.mkloc x loc
  | None ->
      Location.mknoloc x

let rec of_type_desc ?loc typ =
  match typ with
  | Ttyp_var None ->
      Typ.any ?loc ()
  | Ttyp_var (Some name) ->
      Typ.var ?loc name.txt
  | Ttyp_poly (_, typ) ->
      of_type_expr typ
  | Ttyp_arrow (typ1, typ2, _, label) ->
      Typ.arrow ?loc label (of_type_expr typ1) (of_type_expr typ2)
  | Ttyp_ctor {var_ident= name; var_params= params; _} ->
      Typ.constr ?loc (of_path_loc name) (List.map ~f:of_type_expr params)
  | Ttyp_tuple typs ->
      Typ.tuple ?loc (List.map ~f:of_type_expr typs)
  | Ttyp_prover typ ->
      of_type_expr typ
  | Ttyp_conv (typ1, typ2) ->
      mk_typ_t ?loc typ1 typ2
  | Ttyp_opaque typ ->
      Typ.constr ?loc
        (Location.mkloc
           (Option.value_exn
              (Longident.unflatten ["Snarky"; "As_prover"; "Ref"; "t"]))
           (Option.value ~default:Location.none loc))
        [Typ.any ?loc (); of_type_expr typ]

and of_type_expr typ = of_type_desc ~loc:typ.type_loc typ.type_desc

and mk_typ_t ?(loc = Location.none) typ1 typ2 =
  let typ1 = of_type_expr typ1 in
  let typ2 = of_type_expr typ2 in
  let typ_t =
    Option.value_exn (Longident.unflatten ["Snarky"; "Types"; "Typ"; "t"])
  in
  let typ_t = Location.mkloc typ_t loc in
  (* Arguments are [var, value, field, checked]. *)
  Typ.constr ~loc typ_t [typ1; typ2; Typ.any ~loc (); Typ.any ~loc ()]

let of_field_decl {fld_ident= name; fld_type= typ; fld_loc= loc; _} =
  Type.field ~loc (of_ident_loc name) (of_type_expr typ)

let of_ctor_args = function
  | Tctor_tuple args ->
      Parsetree.Pcstr_tuple (List.map ~f:of_type_expr args)
  | Tctor_record fields ->
      Parsetree.Pcstr_record (List.map ~f:of_field_decl fields)

let of_ctor_decl
    { ctor_ident= name
    ; ctor_args= args
    ; ctor_ret= ret
    ; ctor_loc= loc
    ; ctor_ctor= _ } =
  Type.constructor (of_ident_loc name) ~loc ~args:(of_ctor_args args)
    ?res:(Option.map ~f:of_type_expr ret)

let of_ctor_decl_ext
    { ctor_ident= name
    ; ctor_args= args
    ; ctor_ret= ret
    ; ctor_loc= loc
    ; ctor_ctor= _ } =
  Te.decl ~loc ~args:(of_ctor_args args) (of_ident_loc name)
    ?res:(Option.map ~f:of_type_expr ret)

let of_type_decl decl =
  let loc = decl.tdec_loc in
  let name = of_ident_loc decl.tdec_ident in
  let params =
    List.map ~f:(fun t -> (of_type_expr t, Invariant)) decl.tdec_params
  in
  match decl.tdec_desc with
  | Tdec_abstract ->
      Type.mk name ~loc ~params
  | Tdec_alias typ ->
      Type.mk name ~loc ~params ~manifest:(of_type_expr typ)
  | Tdec_record fields ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_record (List.map ~f:of_field_decl fields))
  | Tdec_variant ctors ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_variant (List.map ~f:of_ctor_decl ctors))
  | Tdec_open ->
      Type.mk name ~loc ~params ~kind:Parsetree.Ptype_open
  | Tdec_extend _ ->
      failwith "Cannot convert TExtend to OCaml"

let rec of_pattern_desc ?loc = function
  | Tpat_any ->
      Pat.any ?loc ()
  | Tpat_variable str ->
      Pat.var ?loc (of_ident_loc str)
  | Tpat_constraint (p, typ) ->
      Pat.constraint_ ?loc (of_pattern p) (of_type_expr typ)
  | Tpat_tuple ps ->
      Pat.tuple ?loc (List.map ~f:of_pattern ps)
  | Tpat_or (p1, p2) ->
      Pat.or_ ?loc (of_pattern p1) (of_pattern p2)
  | Tpat_int i ->
      Pat.constant ?loc (Const.int i)
  | Tpat_record fields ->
      Pat.record ?loc
        (List.map fields ~f:(fun (f, p) -> (of_path_loc f, of_pattern p)))
        Open
  | Tpat_ctor (name, arg) ->
      Pat.construct ?loc (of_path_loc name) (Option.map ~f:of_pattern arg)

and of_pattern pat = of_pattern_desc ~loc:pat.pat_loc pat.pat_desc

let of_literal ?loc = function
  | Bool _ ->
      failwith "Unhandled boolean literal"
  | Int i ->
      Exp.constant ?loc (Const.int i)
  | Field _f ->
      failwith "Unhandled field literal"
  | String s ->
      Exp.constant ?loc (Const.string s)

(** Code generation for [Typ.t] store/read fields from [convert_body_desc]s. *)
let rec mapper_of_convert_body_desc ~field ~bind ~return ?loc desc =
  let mapper_of_convert_body = mapper_of_convert_body ~field ~bind ~return in
  match desc with
  | Tconv_record fields ->
      let bind = Exp.ident ?loc bind in
      let return = Exp.ident ?loc return in
      let fields =
        List.map fields ~f:(fun (name, conv) ->
            let name = of_path_loc name in
            let short_name = map_loc ~f:Longident.last name in
            let pat = Pat.var ~loc:name.loc short_name in
            let exp = Exp.ident ~loc:name.loc (mk_lid short_name) in
            let conv = mapper_of_convert_body conv in
            (name, pat, exp, conv) )
      in
      let record_exp =
        let fields =
          List.map fields ~f:(fun (name, _, exp, _) -> (name, exp))
        in
        (* Return the re-constructed record. *)
        Exp.apply ?loc return [(Nolabel, Exp.record ?loc fields None)]
      in
      let record_pat =
        let fields =
          List.map fields ~f:(fun (name, pat, _, _) -> (name, pat))
        in
        Pat.record ?loc fields Closed
      in
      let binds =
        List.fold ~init:record_exp fields ~f:(fun exp (_, pat, var, conv) ->
            let apply_conv = Exp.apply ?loc conv [(Nolabel, var)] in
            let rest = Exp.fun_ ?loc Nolabel None pat exp in
            Exp.apply ?loc bind [(Nolabel, apply_conv); (Labelled "f", rest)]
        )
      in
      Exp.fun_ ?loc Nolabel None record_pat binds
  | Tconv_ctor (name, []) ->
      Exp.field ?loc (Exp.ident ~loc:name.loc (of_path_loc name)) field
  | Tconv_ctor (name, args) ->
      (* TODO: Lift this case as a let in [convert], just apply the name. *)
      let args =
        List.map args ~f:(fun arg ->
            (Nolabel, of_convert_desc ~loc:arg.conv_body_loc (Tconv_body arg))
        )
      in
      let name = of_path_loc name in
      Exp.field ?loc (Exp.apply ?loc (Exp.ident ~loc:name.loc name) args) field
  | Tconv_tuple convs ->
      let bind = Exp.ident ?loc bind in
      let return = Exp.ident ?loc return in
      let convs =
        List.mapi convs ~f:(fun i conv ->
            let loc = conv.conv_body_loc in
            let name = mk_loc ~loc (sprintf "x%i" i) in
            let exp = Exp.ident ~loc (mk_lid name) in
            let pat = Pat.var ~loc name in
            let conv = mapper_of_convert_body conv in
            (pat, exp, conv) )
      in
      let tuple_exp =
        (* Return the re-constructed tuple. *)
        Exp.apply ?loc return
          [(Nolabel, Exp.tuple ?loc (List.map convs ~f:(fun (_, e, _) -> e)))]
      in
      let tuple_pat =
        Pat.tuple ?loc (List.map convs ~f:(fun (p, _, _) -> p))
      in
      let binds =
        List.fold ~init:tuple_exp convs ~f:(fun exp (pat, var, conv) ->
            let apply_conv = Exp.apply ?loc conv [(Nolabel, var)] in
            let rest = Exp.fun_ ?loc Nolabel None pat exp in
            Exp.apply ?loc bind [(Nolabel, apply_conv); (Labelled "f", rest)]
        )
      in
      Exp.fun_ ?loc Nolabel None tuple_pat binds

(** Code generation for [Typ.t] store/read fields from [convert_body]s. *)
and mapper_of_convert_body ~field ~bind ~return
    {conv_body_desc; conv_body_loc= loc; conv_body_type= _} =
  mapper_of_convert_body_desc ~field ~bind ~return ~loc conv_body_desc

(** Code generation for [Typ.t] alloc fields from [convert_body_desc]s. *)
and alloc_of_convert_body_desc ?loc desc =
  let lid x = mk_loc ?loc (Option.value_exn (Longident.unflatten x)) in
  let return = lid ["Snarky"; "Typ_monads"; "Alloc"; "return"] in
  let bind = lid ["Snarky"; "Typ_mondas"; "Alloc"; "bind"] in
  let field = lid ["Snarky"; "Types"; "Typ"; "alloc"] in
  match desc with
  | Tconv_record fields ->
      let bind = Exp.ident ?loc bind in
      let return = Exp.ident ?loc return in
      let fields =
        List.map fields ~f:(fun (name, conv) ->
            let name = of_path_loc name in
            let short_name = map_loc ~f:Longident.last name in
            let pat = Pat.var ~loc:name.loc short_name in
            let exp = Exp.ident ~loc:name.loc (mk_lid short_name) in
            let conv = alloc_of_convert_body conv in
            (name, pat, exp, conv) )
      in
      let record_exp =
        let fields =
          List.map fields ~f:(fun (name, _, exp, _) -> (name, exp))
        in
        (* Return the re-constructed record. *)
        Exp.apply ?loc return [(Nolabel, Exp.record ?loc fields None)]
      in
      let binds =
        List.fold ~init:record_exp fields ~f:(fun exp (_, pat, _, conv) ->
            let rest = Exp.fun_ ?loc Nolabel None pat exp in
            Exp.apply ?loc bind [(Nolabel, conv); (Labelled "f", rest)] )
      in
      binds
  | Tconv_ctor (name, []) ->
      Exp.field ?loc (Exp.ident ~loc:name.loc (of_path_loc name)) field
  | Tconv_ctor (name, args) ->
      (* TODO: Lift this case as a let in [convert], just apply the name. *)
      let args =
        List.map args ~f:(fun arg ->
            (Nolabel, of_convert_desc ~loc:arg.conv_body_loc (Tconv_body arg))
        )
      in
      let name = of_path_loc name in
      Exp.field ?loc (Exp.apply ?loc (Exp.ident ~loc:name.loc name) args) field
  | Tconv_tuple convs ->
      let bind = Exp.ident ?loc bind in
      let return = Exp.ident ?loc return in
      let convs =
        List.mapi convs ~f:(fun i conv ->
            let loc = conv.conv_body_loc in
            let name = mk_loc ~loc (sprintf "x%i" i) in
            let exp = Exp.ident ~loc (mk_lid name) in
            let pat = Pat.var ~loc name in
            let conv = alloc_of_convert_body conv in
            (pat, exp, conv) )
      in
      let tuple_exp =
        (* Return the re-constructed tuple. *)
        Exp.apply ?loc return
          [(Nolabel, Exp.tuple ?loc (List.map convs ~f:(fun (_, e, _) -> e)))]
      in
      let binds =
        List.fold ~init:tuple_exp convs ~f:(fun exp (pat, _, conv) ->
            let rest = Exp.fun_ ?loc Nolabel None pat exp in
            Exp.apply ?loc bind [(Nolabel, conv); (Labelled "f", rest)] )
      in
      binds

(** Code generation for [Typ.t] alloc fields from [convert_body]s. *)
and alloc_of_convert_body
    {conv_body_desc; conv_body_loc= loc; conv_body_type= _} =
  alloc_of_convert_body_desc ~loc conv_body_desc

(** Code generation for [Typ.t] check fields from [convert_body_desc]s. *)
and check_of_convert_body_desc ?loc desc =
  let lid x = mk_loc ?loc (Option.value_exn (Longident.unflatten x)) in
  let return = lid ["Snarky"; "Checked"; "return"] in
  let bind = lid ["Snarky"; "Checked"; "bind"] in
  let field = lid ["Snarky"; "Types"; "Typ"; "check"] in
  match desc with
  | Tconv_record fields ->
      let bind = Exp.ident ?loc bind in
      let return = Exp.ident ?loc return in
      let fields =
        List.map fields ~f:(fun (name, conv) ->
            let name = of_path_loc name in
            let short_name = map_loc ~f:Longident.last name in
            let pat = Pat.var ~loc:name.loc short_name in
            let exp = Exp.ident ~loc:name.loc (mk_lid short_name) in
            let conv = check_of_convert_body conv in
            (name, pat, exp, conv) )
      in
      let unit_exp =
        (* Return unit. *)
        Exp.apply ?loc return [(Nolabel, Exp.construct ?loc (lid ["()"]) None)]
      in
      let unit_pat = Pat.construct ?loc (lid ["()"]) None in
      let record_pat =
        let fields =
          List.map fields ~f:(fun (name, pat, _, _) -> (name, pat))
        in
        Pat.record ?loc fields Closed
      in
      let binds =
        List.fold ~init:unit_exp fields ~f:(fun exp (_, _, var, conv) ->
            let apply_conv = Exp.apply ?loc conv [(Nolabel, var)] in
            let rest = Exp.fun_ ?loc Nolabel None unit_pat exp in
            Exp.apply ?loc bind [(Nolabel, apply_conv); (Labelled "f", rest)]
        )
      in
      Exp.fun_ ?loc Nolabel None record_pat binds
  | Tconv_ctor (name, []) ->
      Exp.field ?loc (Exp.ident ~loc:name.loc (of_path_loc name)) field
  | Tconv_ctor (name, args) ->
      (* TODO: Lift this case as a let in [convert], just apply the name. *)
      let args =
        List.map args ~f:(fun arg ->
            (Nolabel, of_convert_desc ~loc:arg.conv_body_loc (Tconv_body arg))
        )
      in
      let name = of_path_loc name in
      Exp.field ?loc (Exp.apply ?loc (Exp.ident ~loc:name.loc name) args) field
  | Tconv_tuple convs ->
      let bind = Exp.ident ?loc bind in
      let return = Exp.ident ?loc return in
      let convs =
        List.mapi convs ~f:(fun i conv ->
            let loc = conv.conv_body_loc in
            let name = mk_loc ~loc (sprintf "x%i" i) in
            let exp = Exp.ident ~loc (mk_lid name) in
            let pat = Pat.var ~loc name in
            let conv = check_of_convert_body conv in
            (pat, exp, conv) )
      in
      let unit_exp =
        (* Return unit. *)
        Exp.apply ?loc return [(Nolabel, Exp.construct ?loc (lid ["()"]) None)]
      in
      let unit_pat = Pat.construct ?loc (lid ["()"]) None in
      let tuple_pat =
        Pat.tuple ?loc (List.map convs ~f:(fun (p, _, _) -> p))
      in
      let binds =
        List.fold ~init:unit_exp convs ~f:(fun exp (_, var, conv) ->
            let apply_conv = Exp.apply ?loc conv [(Nolabel, var)] in
            let rest = Exp.fun_ ?loc Nolabel None unit_pat exp in
            Exp.apply ?loc bind [(Nolabel, apply_conv); (Labelled "f", rest)]
        )
      in
      Exp.fun_ ?loc Nolabel None tuple_pat binds

(** Code generation for [Typ.t] store/read fields from [convert_body]s. *)
and check_of_convert_body
    {conv_body_desc; conv_body_loc= loc; conv_body_type= _} =
  check_of_convert_body_desc ~loc conv_body_desc

(** Code generation for [Typ.t]s from [convert_desc]s. *)
and of_convert_desc ?loc = function
  | Tconv_fun (name, body) ->
      Exp.fun_ ?loc Nolabel None
        (Pat.var ~loc:name.loc (of_ident_loc name))
        (of_convert body)
  | Tconv_body {conv_body_desc= Tconv_ctor (name, []); conv_body_loc= loc; _}
    ->
      Exp.ident ~loc (of_path_loc name)
  | Tconv_body {conv_body_desc= Tconv_ctor (name, args); conv_body_loc= loc; _}
    ->
      let args =
        List.map args ~f:(fun arg ->
            (Nolabel, of_convert_desc ~loc:arg.conv_body_loc (Tconv_body arg))
        )
      in
      Exp.apply ~loc (Exp.ident ~loc:name.loc (of_path_loc name)) args
  | Tconv_body body ->
      let mk_lid x = mk_loc ?loc (Option.value_exn (Longident.unflatten x)) in
      let store_bind = mk_lid ["Snarky"; "Typ_monads"; "Store"; "bind"] in
      let store_return = mk_lid ["Snarky"; "Typ_monads"; "Store"; "return"] in
      let store_field = mk_lid ["Snarky"; "Types"; "Typ"; "store"] in
      let read_bind = mk_lid ["Snarky"; "Typ_monads"; "Read"; "bind"] in
      let read_return = mk_lid ["Snarky"; "Typ_monads"; "Read"; "return"] in
      let read_field = mk_lid ["Snarky"; "Types"; "Typ"; "read"] in
      Exp.record ?loc
        [ ( mk_lid ["Snarky"; "Types"; "Typ"; "store"]
          , mapper_of_convert_body ~field:store_field ~bind:store_bind
              ~return:store_return body )
        ; ( mk_lid ["Snarky"; "Types"; "Typ"; "read"]
          , mapper_of_convert_body ~field:read_field ~bind:read_bind
              ~return:read_return body )
        ; ( mk_lid ["Snarky"; "Types"; "Typ"; "alloc"]
          , alloc_of_convert_body body )
        ; ( mk_lid ["Snarky"; "Types"; "Typ"; "check"]
          , check_of_convert_body body ) ]
        None

(** Code generation for [Typ.t]s from [convert]s. *)
and of_convert {conv_desc; conv_loc= loc; conv_type= _} =
  of_convert_desc ~loc conv_desc

let rec of_expression_desc ?loc = function
  | Texp_apply (f, es) ->
      Exp.apply ?loc (of_expression f)
        (List.map ~f:(fun (label, x) -> (label, of_expression x)) es)
  | Texp_variable name ->
      Exp.ident ?loc (of_path_loc name)
  | Texp_literal l ->
      of_literal ?loc l
  | Texp_fun (label, p, body, _) ->
      Exp.fun_ ?loc label None (of_pattern p) (of_expression body)
  | Texp_newtype (name, body) ->
      Exp.newtype ?loc (of_ident_loc name) (of_expression body)
  | Texp_constraint (e, typ) ->
      Exp.constraint_ ?loc (of_expression e) (of_type_expr typ)
  | Texp_seq (e1, e2) ->
      Exp.sequence ?loc (of_expression e1) (of_expression e2)
  | Texp_let (p, e_rhs, e) ->
      Exp.let_ ?loc Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)
  | Texp_tuple es ->
      Exp.tuple ?loc (List.map ~f:of_expression es)
  | Texp_match (e, cases) ->
      Exp.match_ ?loc (of_expression e)
        (List.map cases ~f:(fun (p, e) ->
             Exp.case (of_pattern p) (of_expression e) ))
  | Texp_field (e, field) ->
      Exp.field ?loc (of_expression e) (of_path_loc field)
  | Texp_record (fields, ext) ->
      Exp.record ?loc
        (List.map fields ~f:(fun (f, e) -> (of_path_loc f, of_expression e)))
        (Option.map ~f:of_expression ext)
  | Texp_ctor (name, arg) ->
      Exp.construct ?loc (of_path_loc name) (Option.map ~f:of_expression arg)
  | Texp_unifiable {expression= Some e; _} ->
      of_expression e
  | Texp_unifiable {name; _} ->
      Exp.ident ?loc (mk_lid @@ of_ident_loc name)
  | Texp_if (e1, e2, e3) ->
      Exp.ifthenelse ?loc (of_expression e1) (of_expression e2)
        (Option.map ~f:of_expression e3)
  | Texp_prover e ->
      of_expression e

and of_handler ?(loc = Location.none) ?ctor_ident (args, body) =
  Parsetree.(
    [%expr
      function
      | With
          { request=
              [%p
                match ctor_ident with
                | Some ctor_ident ->
                    Pat.construct ~loc (mk_lid ctor_ident)
                      (Option.map ~f:of_pattern args)
                | None -> (
                  match args with
                  | Some args ->
                      of_pattern args
                  | None ->
                      Pat.any () )]
          ; respond } ->
          let unhandled = Snarky.Request.unhandled in
          [%e of_expression body]
      | _ ->
          Snarky.Request.unhandled])

and of_expression exp = of_expression_desc ~loc:exp.exp_loc exp.exp_desc

let rec of_signature_desc ?loc = function
  | Tsig_value (name, typ) | Tsig_instance (name, typ) ->
      Sig.value ?loc (Val.mk ?loc (of_ident_loc name) (of_type_expr typ))
  | Tsig_type decl ->
      Sig.type_ ?loc Recursive [of_type_decl decl]
  | Tsig_convtype (decl, tconv, convname, typ) ->
      let decls =
        match tconv with
        | Ttconv_with (_, conv_decl) ->
            [of_type_decl decl; of_type_decl conv_decl]
        | Ttconv_to _ ->
            [of_type_decl decl]
      in
      let sigs =
        [ Sig.type_ ?loc Nonrecursive decls
        ; Sig.value ?loc
            (Val.mk ?loc (of_ident_loc convname) (of_type_expr typ)) ]
      in
      Sig.include_ ?loc
        { pincl_mod= Mty.signature ?loc sigs
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }
  | Tsig_module (name, msig) ->
      let msig =
        match of_module_sig msig with
        | Some msig ->
            msig
        | None ->
            failwith
              "Cannot generate OCaml for a module with an abstract signature"
      in
      Sig.module_ ?loc (Md.mk ?loc (of_ident_loc name) msig)
  | Tsig_modtype (name, msig) ->
      Sig.modtype ?loc
        (Mtd.mk ?loc ?typ:(of_module_sig msig) (of_ident_loc name))
  | Tsig_open name ->
      Sig.open_ ?loc (Opn.mk ?loc (of_path_loc name))
  | Tsig_typeext (variant, ctors) ->
      let params =
        List.map variant.var_params ~f:(fun typ -> (of_type_expr typ, Invariant)
        )
      in
      let ctors = List.map ~f:of_ctor_decl_ext ctors in
      Sig.type_extension ?loc
        (Te.mk ~params (of_path_loc variant.var_ident) ctors)
  | Tsig_request (_, ctor) ->
      let params = [(Typ.any ?loc (), Invariant)] in
      let ident =
        mk_loc ?loc Longident.(Ldot (Ldot (Lident "Snarky", "Request"), "t"))
      in
      Sig.type_extension ?loc (Te.mk ~params ident [of_ctor_decl_ext ctor])
  | Tsig_multiple sigs | Tsig_prover sigs ->
      Sig.include_ ?loc
        { pincl_mod= Mty.signature ?loc (of_signature sigs)
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }
  | Tsig_convert (name, typ) ->
      Sig.value ?loc (Val.mk ?loc (of_ident_loc name) (of_type_expr typ))

and of_signature_item sigi = of_signature_desc ~loc:sigi.sig_loc sigi.sig_desc

and of_signature sig_ = List.map ~f:of_signature_item sig_

and of_module_sig_desc ?loc = function
  | Tmty_sig signature ->
      Some (Mty.signature ?loc (of_signature signature))
  | Tmty_name name ->
      Some (Mty.ident ?loc (of_path_loc name))
  | Tmty_alias name ->
      Some (Mty.alias ?loc (of_path_loc name))
  | Tmty_abstract ->
      None
  | Tmty_functor (name, f, msig) ->
      let msig =
        match of_module_sig msig with
        | Some msig ->
            msig
        | None ->
            failwith
              "Cannot generate OCaml for a functor signature with an abstract \
               signature"
      in
      Some (Mty.functor_ ?loc name (of_module_sig f) msig)

and of_module_sig msig = of_module_sig_desc ~loc:msig.msig_loc msig.msig_desc

let rec of_statement_desc ?loc = function
  | Tstmt_value (p, e) ->
      Str.value ?loc Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]
  | Tstmt_instance (name, e) ->
      Str.value ?loc Nonrecursive
        [Vb.mk (Pat.var ?loc (of_ident_loc name)) (of_expression e)]
  | Tstmt_type decl ->
      Str.type_ ?loc Recursive [of_type_decl decl]
  | Tstmt_convtype (decl, tconv, convname, conv) ->
      let decls =
        match tconv with
        | Ttconv_with (_, conv_decl) ->
            [of_type_decl decl; of_type_decl conv_decl]
        | Ttconv_to _ ->
            [of_type_decl decl]
      in
      let strs =
        [ Str.type_ ?loc Nonrecursive decls
        ; Str.value ?loc Nonrecursive
            [Vb.mk (Pat.var ?loc (of_ident_loc convname)) (of_convert conv)] ]
      in
      Str.include_ ?loc
        { pincl_mod= Mod.structure ?loc strs
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }
  | Tstmt_module (name, m) ->
      Str.module_ ?loc (Mb.mk ?loc (of_ident_loc name) (of_module_expr m))
  | Tstmt_modtype (name, msig) ->
      Str.modtype ?loc
        (Mtd.mk ?loc ?typ:(of_module_sig msig) (of_ident_loc name))
  | Tstmt_open name ->
      Str.open_ ?loc (Opn.mk ?loc (Of_ocaml.open_of_name (of_path_loc name)))
  | Tstmt_typeext (variant, ctors) ->
      let params =
        List.map variant.var_params ~f:(fun typ -> (of_type_expr typ, Invariant)
        )
      in
      let ctors = List.map ~f:of_ctor_decl_ext ctors in
      Str.type_extension ?loc
        (Te.mk ~params (of_path_loc variant.var_ident) ctors)
  | Tstmt_request (_, ctor, handler) ->
      let params = [(Typ.any ?loc (), Invariant)] in
      let ident =
        mk_loc ?loc Longident.(Ldot (Ldot (Lident "Snarky", "Request"), "t"))
      in
      let typ_ext =
        Str.type_extension ?loc (Te.mk ~params ident [of_ctor_decl_ext ctor])
      in
      let handler =
        Option.map handler
          ~f:
            Parsetree.(
              fun (args, body) ->
                let {txt= name; loc} = ctor.ctor_ident in
                [%stri
                  let [%p
                        Pat.var ~loc (mk_loc ~loc ("handle_" ^ of_ident name))]
                      = function
                    | With
                        { request=
                            [%p
                              Pat.construct ~loc
                                (mk_lid (of_ident_loc ctor.ctor_ident))
                                (Option.map ~f:of_pattern args)]
                        ; respond } ->
                        let unhandled = Snarky.Request.unhandled in
                        [%e of_expression body]
                    | _ ->
                        Snarky.Request.unhandled])
      in
      Str.include_ ?loc
        { pincl_mod= Mod.structure ?loc (typ_ext :: Option.to_list handler)
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }
  | Tstmt_multiple stmts | Tstmt_prover stmts ->
      Str.include_ ?loc
        { pincl_mod= Mod.structure ?loc (List.map ~f:of_statement stmts)
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }
  | Tstmt_convert (name, typ, conv) ->
      Str.value ?loc Nonrecursive
        [ Vb.mk
            (Pat.constraint_ ~loc:typ.type_loc
               (Pat.var ~loc:name.loc (of_ident_loc name))
               (of_type_expr typ))
            (of_convert conv) ]

and of_statement stmt = of_statement_desc ~loc:stmt.stmt_loc stmt.stmt_desc

and of_module_expr m = of_module_desc ~loc:m.mod_loc m.mod_desc

and of_module_desc ?loc = function
  | Tmod_struct stmts ->
      Mod.structure ?loc (List.map ~f:of_statement stmts)
  | Tmod_name name ->
      Mod.ident ?loc (of_path_loc name)
  | Tmod_functor (name, f, m) ->
      Mod.functor_ ?loc name (of_module_sig f) (of_module_expr m)

let of_file = List.map ~f:of_statement
