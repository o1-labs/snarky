open Core_kernel
open Asttypes
open Ast_types
open Ast_helper
open Parsetypes

let rec of_type_desc ?loc typ =
  match typ with
  | Tvar (None, _) ->
      Typ.any ?loc ()
  | Tvar (Some name, _) ->
      Typ.var ?loc name.txt
  | Tpoly (_, typ) ->
      of_type_expr typ
  | Tarrow (typ1, typ2, _, label) ->
      Typ.arrow ?loc label (of_type_expr typ1) (of_type_expr typ2)
  | Tctor
      {var_ident= name; var_params= params; var_implicit_params= implicits; _}
    ->
      Typ.constr ?loc name (List.map ~f:of_type_expr (params @ implicits))
  | Ttuple typs ->
      Typ.tuple ?loc (List.map ~f:of_type_expr typs)

and of_type_expr typ = of_type_desc ~loc:typ.type_loc typ.type_desc

let of_field_decl {fld_ident= name; fld_type= typ; fld_loc= loc; _} =
  Type.field ~loc name (of_type_expr typ)

let of_ctor_args = function
  | Ctor_tuple args ->
      Parsetree.Pcstr_tuple (List.map ~f:of_type_expr args)
  | Ctor_record (_, fields) ->
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
    List.map
      ~f:(fun t -> (of_type_expr t, Invariant))
      (decl.tdec_params @ decl.tdec_implicit_params)
  in
  match decl.tdec_desc with
  | TAbstract ->
      Type.mk name ~loc ~params
  | TAlias typ ->
      Type.mk name ~loc ~params ~manifest:(of_type_expr typ)
  | TRecord fields ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_record (List.map ~f:of_field_decl fields))
  | TVariant ctors ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_variant (List.map ~f:of_ctor_decl ctors))
  | TOpen ->
      Type.mk name ~loc ~params ~kind:Parsetree.Ptype_open
  | TExtend _ ->
      failwith "Cannot convert TExtend to OCaml"
  | TUnfold _ ->
      failwith "Cannot convert TUnfold to OCaml"
  | TForward _ ->
      failwith "Cannot convert TForward to OCaml"

let rec of_pattern_desc ?loc = function
  | PAny ->
      Pat.any ?loc ()
  | PVariable str ->
      Pat.var ?loc str
  | PConstraint (p, typ) ->
      Pat.constraint_ ?loc (of_pattern p) (of_type_expr typ)
  | PTuple ps ->
      Pat.tuple ?loc (List.map ~f:of_pattern ps)
  | POr (p1, p2) ->
      Pat.or_ ?loc (of_pattern p1) (of_pattern p2)
  | PInt i ->
      Pat.constant ?loc (Const.int i)
  | PRecord fields ->
      Pat.record ?loc
        (List.map fields ~f:(fun (f, p) -> (f, of_pattern p)))
        Open
  | PCtor (name, arg) ->
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
  | Apply (f, es) ->
      Exp.apply ?loc (of_expression f)
        (List.map ~f:(fun (label, x) -> (label, of_expression x)) es)
  | Variable name ->
      Exp.ident ?loc name
  | Literal l ->
      of_literal ?loc l
  | Fun (label, p, body, _) ->
      Exp.fun_ ?loc label None (of_pattern p) (of_expression body)
  | Newtype (name, body) ->
      Exp.newtype ?loc name (of_expression body)
  | Constraint (e, typ) ->
      Exp.constraint_ ?loc (of_expression e) (of_type_expr typ)
  | Seq (e1, e2) ->
      Exp.sequence ?loc (of_expression e1) (of_expression e2)
  | Let (p, e_rhs, e) ->
      Exp.let_ ?loc Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)
  | Tuple es ->
      Exp.tuple ?loc (List.map ~f:of_expression es)
  | Match (e, cases) ->
      Exp.match_ ?loc (of_expression e)
        (List.map cases ~f:(fun (p, e) ->
             Exp.case (of_pattern p) (of_expression e) ))
  | Field (e, field) ->
      Exp.field ?loc (of_expression e) field
  | Record (fields, ext) ->
      Exp.record ?loc
        (List.map fields ~f:(fun (f, e) -> (f, of_expression e)))
        (Option.map ~f:of_expression ext)
  | Ctor (name, arg) ->
      Exp.construct ?loc name (Option.map ~f:of_expression arg)
  | Unifiable {expression= Some e; _} ->
      of_expression e
  | Unifiable {name; _} ->
      Exp.ident ?loc (mk_lid name)

and of_expression exp = of_expression_desc ~loc:exp.exp_loc exp.exp_desc

let rec of_signature_desc ?loc = function
  | SValue (name, typ) | SInstance (name, typ) ->
      Sig.value ?loc (Val.mk ?loc name (of_type_expr typ))
  | STypeDecl decl ->
      Sig.type_ ?loc Recursive [of_type_decl decl]
  | SModule (name, msig) ->
      let msig =
        match of_module_sig msig with
        | Some msig ->
            msig
        | None ->
            failwith
              "Cannot generate OCaml for a module with an abstract signature"
      in
      Sig.module_ ?loc (Md.mk ?loc name msig)
  | SModType (name, msig) ->
      Sig.modtype ?loc (Mtd.mk ?loc ?typ:(of_module_sig msig) name)
  | SOpen name ->
      Sig.open_ ?loc (Opn.mk ?loc name)
  | STypeExtension (variant, ctors) ->
      let params =
        List.map variant.var_params ~f:(fun typ -> (of_type_expr typ, Invariant)
        )
      in
      let ctors = List.map ~f:of_ctor_decl_ext ctors in
      Sig.type_extension ?loc (Te.mk ~params variant.var_ident ctors)
  | SRequest (_, ctor) ->
      let params = [(Typ.any ?loc (), Invariant)] in
      let ident =
        Location.mkloc
          Longident.(Ldot (Ldot (Lident "Snarky", "Request"), "t"))
          (Option.value ~default:Location.none loc)
      in
      Sig.type_extension ?loc (Te.mk ~params ident [of_ctor_decl_ext ctor])
  | SMultiple sigs ->
      Sig.include_ ?loc
        { pincl_mod= Mty.signature ?loc (of_signature sigs)
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }

and of_signature_item sigi = of_signature_desc ~loc:sigi.sig_loc sigi.sig_desc

and of_signature sig_ = List.map ~f:of_signature_item sig_

and of_module_sig_desc ?loc = function
  | Signature signature ->
      Some (Mty.signature ?loc (of_signature signature))
  | SigName name ->
      Some (Mty.alias ?loc name)
  | SigAbstract ->
      None
  | SigFunctor (name, f, msig) ->
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
  | Value (p, e) ->
      Str.value ?loc Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]
  | Instance (name, e) ->
      Str.value ?loc Nonrecursive [Vb.mk (Pat.var ?loc name) (of_expression e)]
  | TypeDecl decl ->
      Str.type_ ?loc Recursive [of_type_decl decl]
  | Module (name, m) ->
      Str.module_ ?loc (Mb.mk ?loc name (of_module_expr m))
  | ModType (name, msig) ->
      Str.modtype ?loc (Mtd.mk ?loc ?typ:(of_module_sig msig) name)
  | Open name ->
      Str.open_ ?loc (Opn.mk ?loc name)
  | TypeExtension (variant, ctors) ->
      let params =
        List.map variant.var_params ~f:(fun typ -> (of_type_expr typ, Invariant)
        )
      in
      let ctors = List.map ~f:of_ctor_decl_ext ctors in
      Str.type_extension ?loc (Te.mk ~params variant.var_ident ctors)
  | Request (_, ctor, handler) ->
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
  | Multiple stmts ->
      Str.include_ ?loc
        { pincl_mod= Mod.structure ?loc (List.map ~f:of_statement stmts)
        ; pincl_loc= Option.value ~default:Location.none loc
        ; pincl_attributes= [] }

and of_statement stmt = of_statement_desc ~loc:stmt.stmt_loc stmt.stmt_desc

and of_module_expr m = of_module_desc ~loc:m.mod_loc m.mod_desc

and of_module_desc ?loc = function
  | Structure stmts ->
      Mod.structure ?loc (List.map ~f:of_statement stmts)
  | ModName name ->
      Mod.ident ?loc name
  | Functor (name, f, m) ->
      Mod.functor_ ?loc name (of_module_sig f) (of_module_expr m)

let of_file = List.map ~f:of_statement
