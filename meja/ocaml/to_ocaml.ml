open Core_kernel
open Asttypes
open Meja_lib.Ast_types
open Ast_helper
open Meja_lib.Parsetypes

let rec of_type_desc ?loc typ =
  match typ with
  | Ptyp_var None ->
      Typ.any ?loc ()
  | Ptyp_var (Some name) ->
      Typ.var ?loc name.txt
  | Ptyp_poly (_, typ) ->
      of_type_expr typ
  | Ptyp_arrow (typ1, typ2, _, label) ->
      Typ.arrow ?loc label (of_type_expr typ1) (of_type_expr typ2)
  | Ptyp_ctor {var_ident= name; var_params= params; _} ->
      Typ.constr ?loc name (List.map ~f:of_type_expr params)
  | Ptyp_tuple typs ->
      Typ.tuple ?loc (List.map ~f:of_type_expr typs)
  | Ptyp_prover typ ->
      of_type_expr typ

and of_type_expr typ = of_type_desc ~loc:typ.type_loc typ.type_desc

let of_field_decl {fld_ident= name; fld_type= typ; fld_loc= loc; _} =
  Type.field ~loc name (of_type_expr typ)

let of_ctor_args = function
  | Ctor_tuple args ->
      Parsetree.Pcstr_tuple (List.map ~f:of_type_expr args)
  | Ctor_record fields ->
      Parsetree.Pcstr_record (List.map ~f:of_field_decl fields)

let of_ctor_decl
    {ctor_ident= name; ctor_args= args; ctor_ret= ret; ctor_loc= loc} =
  Type.constructor name ~loc ~args:(of_ctor_args args)
    ?res:(Option.map ~f:of_type_expr ret)

let of_ctor_decl_ext
    {ctor_ident= name; ctor_args= args; ctor_ret= ret; ctor_loc= loc} =
  Te.decl ~loc ~args:(of_ctor_args args) name
    ?res:(Option.map ~f:of_type_expr ret)

let of_type_decl decl =
  let loc = decl.tdec_loc in
  let name = decl.tdec_ident in
  let params =
    List.map ~f:(fun t -> (of_type_expr t, Invariant)) decl.tdec_params
  in
  match decl.tdec_desc with
  | Pdec_abstract ->
      Type.mk name ~loc ~params
  | Pdec_alias typ ->
      Type.mk name ~loc ~params ~manifest:(of_type_expr typ)
  | Pdec_record fields ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_record (List.map ~f:of_field_decl fields))
  | Pdec_variant ctors ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_variant (List.map ~f:of_ctor_decl ctors))
  | Pdec_open ->
      Type.mk name ~loc ~params ~kind:Parsetree.Ptype_open
  | Pdec_extend _ ->
      failwith "Cannot convert TExtend to OCaml"

let rec of_pattern_desc ?loc = function
  | Ppat_any ->
      Pat.any ?loc ()
  | Ppat_variable str ->
      Pat.var ?loc str
  | Ppat_constraint (p, typ) ->
      Pat.constraint_ ?loc (of_pattern p) (of_type_expr typ)
  | Ppat_tuple ps ->
      Pat.tuple ?loc (List.map ~f:of_pattern ps)
  | Ppat_or (p1, p2) ->
      Pat.or_ ?loc (of_pattern p1) (of_pattern p2)
  | Ppat_int i ->
      Pat.constant ?loc (Const.int i)
  | Ppat_record fields ->
      Pat.record ?loc
        (List.map fields ~f:(fun (f, p) -> (f, of_pattern p)))
        Open
  | Ppat_ctor (name, arg) ->
      Pat.construct ?loc name (Option.map ~f:of_pattern arg)

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

let rec of_expression_desc ?loc = function
  | Pexp_apply (f, es) ->
      Exp.apply ?loc (of_expression f)
        (List.map ~f:(fun (label, x) -> (label, of_expression x)) es)
  | Pexp_variable name ->
      Exp.ident ?loc name
  | Pexp_literal l ->
      of_literal ?loc l
  | Pexp_fun (label, p, body, _) ->
      Exp.fun_ ?loc label None (of_pattern p) (of_expression body)
  | Pexp_newtype (name, body) ->
      Exp.newtype ?loc name (of_expression body)
  | Pexp_constraint (e, typ) ->
      Exp.constraint_ ?loc (of_expression e) (of_type_expr typ)
  | Pexp_seq (e1, e2) ->
      Exp.sequence ?loc (of_expression e1) (of_expression e2)
  | Pexp_let (p, e_rhs, e) ->
      Exp.let_ ?loc Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)
  | Pexp_tuple es ->
      Exp.tuple ?loc (List.map ~f:of_expression es)
  | Pexp_match (e, cases) ->
      Exp.match_ ?loc (of_expression e)
        (List.map cases ~f:(fun (p, e) ->
             Exp.case (of_pattern p) (of_expression e) ))
  | Pexp_field (e, field) ->
      Exp.field ?loc (of_expression e) field
  | Pexp_record (fields, ext) ->
      Exp.record ?loc
        (List.map fields ~f:(fun (f, e) -> (f, of_expression e)))
        (Option.map ~f:of_expression ext)
  | Pexp_ctor (name, arg) ->
      Exp.construct ?loc name (Option.map ~f:of_expression arg)
  | Pexp_unifiable {expression= Some e; _} ->
      of_expression e
  | Pexp_unifiable {name; _} ->
      Exp.ident ?loc (mk_lid name)
  | Pexp_if (e1, e2, e3) ->
      Exp.ifthenelse ?loc (of_expression e1) (of_expression e2)
        (Option.map ~f:of_expression e3)
  | Pexp_prover e ->
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
  | Psig_value (name, typ) | Psig_instance (name, typ) ->
      Sig.value ?loc (Val.mk ?loc name (of_type_expr typ))
  | Psig_type decl ->
      Sig.type_ ?loc Recursive [of_type_decl decl]
  | Psig_rectype decls ->
      Sig.type_ ?loc Recursive (List.map ~f:of_type_decl decls)
  | Psig_module (name, msig) ->
      let msig =
        match of_module_sig msig with
        | Some msig ->
            msig
        | None ->
            failwith
              "Cannot generate OCaml for a module with an abstract signature"
      in
      Sig.module_ ?loc (Md.mk ?loc name msig)
  | Psig_modtype (name, msig) ->
      Sig.modtype ?loc (Mtd.mk ?loc ?typ:(of_module_sig msig) name)
  | Psig_open name ->
      Sig.open_ ?loc (Opn.mk ?loc name)
  | Psig_typeext (variant, ctors) ->
      let params =
        List.map variant.var_params ~f:(fun typ -> (of_type_expr typ, Invariant)
        )
      in
      let ctors = List.map ~f:of_ctor_decl_ext ctors in
      Sig.type_extension ?loc (Te.mk ~params variant.var_ident ctors)
  | Psig_request (_, ctor) ->
      let params = [(Typ.any ?loc (), Invariant)] in
      let ident =
        Location.mkloc
          Longident.(Ldot (Ldot (Lident "Snarky", "Request"), "t"))
          (Option.value ~default:Location.none loc)
      in
      Sig.type_extension ?loc (Te.mk ~params ident [of_ctor_decl_ext ctor])
  | Psig_multiple sigs | Psig_prover sigs ->
      Sig.include_ ?loc
        { pincl_mod= Mty.signature ?loc (of_signature sigs)
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }

and of_signature_item sigi = of_signature_desc ~loc:sigi.sig_loc sigi.sig_desc

and of_signature sig_ = List.map ~f:of_signature_item sig_

and of_module_sig_desc ?loc = function
  | Pmty_sig signature ->
      Some (Mty.signature ?loc (of_signature signature))
  | Pmty_name name ->
      Some (Mty.ident ?loc name)
  | Pmty_alias name ->
      Some (Mty.alias ?loc name)
  | Pmty_abstract ->
      None
  | Pmty_functor (name, f, msig) ->
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
  | Pstmt_value (p, e) ->
      Str.value ?loc Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]
  | Pstmt_instance (name, e) ->
      Str.value ?loc Nonrecursive [Vb.mk (Pat.var ?loc name) (of_expression e)]
  | Pstmt_type decl ->
      Str.type_ ?loc Recursive [of_type_decl decl]
  | Pstmt_module (name, m) ->
      Str.module_ ?loc (Mb.mk ?loc name (of_module_expr m))
  | Pstmt_modtype (name, msig) ->
      Str.modtype ?loc (Mtd.mk ?loc ?typ:(of_module_sig msig) name)
  | Pstmt_open name ->
      Str.open_ ?loc (Opn.mk ?loc (Of_ocaml.open_of_name name))
  | Pstmt_typeext (variant, ctors) ->
      let params =
        List.map variant.var_params ~f:(fun typ -> (of_type_expr typ, Invariant)
        )
      in
      let ctors = List.map ~f:of_ctor_decl_ext ctors in
      Str.type_extension ?loc (Te.mk ~params variant.var_ident ctors)
  | Pstmt_request (_, ctor, handler) ->
      let params = [(Typ.any ?loc (), Invariant)] in
      let ident =
        Location.mkloc
          Longident.(Ldot (Ldot (Lident "Snarky", "Request"), "t"))
          (Option.value ~default:Location.none loc)
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
                  let [%p Pat.var ~loc (Location.mkloc ("handle_" ^ name) loc)]
                      = function
                    | With
                        { request=
                            [%p
                              Pat.construct ~loc (mk_lid ctor.ctor_ident)
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
  | Pstmt_multiple stmts | Pstmt_prover stmts ->
      Str.include_ ?loc
        { pincl_mod= Mod.structure ?loc (List.map ~f:of_statement stmts)
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }

and of_statement stmt = of_statement_desc ~loc:stmt.stmt_loc stmt.stmt_desc

and of_module_expr m = of_module_desc ~loc:m.mod_loc m.mod_desc

and of_module_desc ?loc = function
  | Pmod_struct stmts ->
      Mod.structure ?loc (List.map ~f:of_statement stmts)
  | Pmod_name name ->
      Mod.ident ?loc name
  | Pmod_functor (name, f, m) ->
      Mod.functor_ ?loc name (of_module_sig f) (of_module_expr m)

let of_file = List.map ~f:of_statement
