open Core_kernel
open Ast_types
open Parsetypes
open Type0

type error =
  | Check_failed of type_expr * type_expr * error
  | Cannot_unify of type_expr * type_expr
  | Recursive_variable of type_expr
  | Unbound of string * lid
  | Unbound_value of str
  | Variable_on_one_side of string
  | Pattern_declaration of string * string
  | Empty_record
  | Wrong_record_field of Longident.t * type_expr
  | Repeated_field of string
  | Missing_fields of string list
  | Wrong_type_description of string * str
  | Unifiable_expr
  | No_unifiable_expr
  | No_instance of type_expr
  | Argument_expected of Longident.t
  | Not_extensible of Longident.t
  | Extension_different_arity of Longident.t

exception Error of Location.t * error

let bind_none x f = match x with Some x -> x | None -> f ()

let unpack_decls ~loc typ ctyp env =
  if Int.equal typ.type_id ctyp.type_id then Some (typ, ctyp)
  else
    let unfold_typ () =
      Option.map (Envi.TypeDecl.unfold_alias ~loc typ env) ~f:(fun typ ->
          (typ, ctyp) )
    in
    let unfold_ctyp () =
      Option.map (Envi.TypeDecl.unfold_alias ~loc ctyp env) ~f:(fun ctyp ->
          (typ, ctyp) )
    in
    match (typ.type_desc, ctyp.type_desc) with
    | Tctor variant, Tctor cvariant ->
        let decl_id, cdecl_id =
          (variant.var_decl.tdec_id, cvariant.var_decl.tdec_id)
        in
        (* Try to unfold the oldest type definition first. *)
        if decl_id < cdecl_id then bind_none (Some (unfold_ctyp ())) unfold_typ
        else bind_none (Some (unfold_typ ())) unfold_ctyp
    | Tctor _, _ ->
        unfold_typ ()
    | _, Tctor _ ->
        unfold_ctyp ()
    | _ ->
        None

let rec check_type_aux ~loc typ ctyp env =
  let check_type_aux = check_type_aux ~loc in
  let without_instance ~f (typ : type_expr) env =
    match Envi.Type.instance env typ with
    | Some typ' -> (
        Envi.Type.clear_instance typ env ;
        f typ' env ;
        match Envi.Type.instance env typ with
        | Some _ ->
            raise (Error (loc, Recursive_variable typ))
        | None ->
            Some (Envi.Type.add_instance typ typ' env) )
    | None ->
        None
  in
  Type0.unify_depths typ ctyp ;
  match (typ.type_desc, ctyp.type_desc) with
  | _, _ when Int.equal typ.type_id ctyp.type_id ->
      ()
  | Tpoly (_, typ), _ ->
      check_type_aux typ ctyp env
  | _, Tpoly (_, ctyp) ->
      check_type_aux typ ctyp env
  | Tvar _, Tvar _ ->
      bind_none
        (without_instance typ env ~f:(fun typ -> check_type_aux typ ctyp))
        (fun () ->
          bind_none
            (without_instance ctyp env ~f:(fun ctyp -> check_type_aux typ ctyp))
            (fun () ->
              (* Add the outermost (in terms of lexical scope) of the variables as
                 the instance for the other. We do this by chosing the type of
                 lowest ID, to ensure strict ordering and thus no cycles. *)
              if ctyp.type_id < typ.type_id then
                Envi.Type.add_instance typ ctyp env
              else Envi.Type.add_instance ctyp typ env ) )
  | Tvar _, _ ->
      bind_none
        (without_instance typ env ~f:(fun typ -> check_type_aux typ ctyp))
        (fun () -> Envi.Type.add_instance typ ctyp env)
  | _, Tvar _ ->
      bind_none
        (without_instance ctyp env ~f:(fun ctyp -> check_type_aux typ ctyp))
        (fun () -> Envi.Type.add_instance ctyp typ env)
  | Ttuple typs, Ttuple ctyps -> (
    match
      List.iter2 typs ctyps ~f:(fun typ ctyp -> check_type_aux typ ctyp env)
    with
    | Ok () ->
        ()
    | Unequal_lengths ->
        raise (Error (loc, Cannot_unify (typ, ctyp))) )
  | ( Tarrow (typ1, typ2, Explicit, label1)
    , Tarrow (ctyp1, ctyp2, Explicit, label2) )
  | ( Tarrow (typ1, typ2, Implicit, label1)
    , Tarrow (ctyp1, ctyp2, Implicit, label2) ) ->
      ( match (label1, label2) with
      | Nolabel, Nolabel ->
          ()
      | Labelled x, Labelled y when String.equal x y ->
          ()
      | Optional x, Optional y when String.equal x y ->
          ()
      | _ ->
          raise (Error (loc, Cannot_unify (typ, ctyp))) ) ;
      check_type_aux typ1 ctyp1 env ;
      check_type_aux typ2 ctyp2 env
  | Tctor variant, Tctor constr_variant -> (
    (* Always try to unfold first, so that type aliases with phantom
         parameters can unify, as in OCaml.
      *)
    match unpack_decls ~loc typ ctyp env with
    | Some (typ, ctyp) ->
        check_type_aux typ ctyp env
    | None ->
        if Int.equal variant.var_decl.tdec_id constr_variant.var_decl.tdec_id
        then
          match
            List.iter2 variant.var_params constr_variant.var_params
              ~f:(fun param constr_param ->
                check_type_aux param constr_param env )
          with
          | Ok env ->
              env
          | Unequal_lengths ->
              raise (Error (loc, Cannot_unify (typ, ctyp)))
        else raise (Error (loc, Cannot_unify (typ, ctyp))) )
  | Tctor _, _ | _, Tctor _ ->
      (* Unfold an alias and compare again *)
      let typ, ctyp =
        match unpack_decls ~loc typ ctyp env with
        | Some (typ, ctyp) ->
            (typ, ctyp)
        | None ->
            raise (Error (loc, Cannot_unify (typ, ctyp)))
      in
      check_type_aux typ ctyp env
  | _, _ ->
      raise (Error (loc, Cannot_unify (typ, ctyp)))

let check_type ~loc env typ constr_typ =
  match check_type_aux ~loc typ constr_typ env with
  | exception Error (_, err) ->
      let typ = Envi.Type.flatten typ env in
      let constr_typ = Envi.Type.flatten constr_typ env in
      raise (Error (loc, Check_failed (typ, constr_typ, err)))
  | () ->
      ()

(** [is_subtype ~loc env typ ~of_:ctyp] returns whether [typ] is a subtype of
    [ctyp], instantiating any variables in [ctyp] to those they match with in
    [typ].

    If this function returns [false], the [ctyp] value *must not* be used,
    since its variables may have been instantiated incorrectly. A type
    containing only fresh variables should be used.

    The type variables within [typ] will remain unchanged.
    *)
let rec is_subtype ~loc env typ ~of_:ctyp =
  let is_subtype = is_subtype ~loc env in
  let without_instance ~f (typ : type_expr) =
    match Envi.Type.instance env typ with
    | Some typ' -> (
        Envi.Type.clear_instance typ env ;
        let ret = f typ' in
        match Envi.Type.instance env typ with
        | Some _ ->
            Some false
        | None ->
            Envi.Type.add_instance typ typ' env ;
            Some ret )
    | None ->
        None
  in
  match (typ.type_desc, ctyp.type_desc) with
  | _, _ when Int.equal typ.type_id ctyp.type_id ->
      true
  | Tpoly (_, typ), _ ->
      is_subtype typ ~of_:ctyp
  | _, Tpoly (_, ctyp) ->
      is_subtype typ ~of_:ctyp
  | Tvar _, Tvar _ ->
      bind_none
        (without_instance typ ~f:(fun typ -> is_subtype typ ~of_:ctyp))
        (fun () ->
          bind_none
            (without_instance ctyp ~f:(fun ctyp -> is_subtype typ ~of_:ctyp))
            (fun () ->
              Envi.Type.add_instance ctyp typ env ;
              true ) )
  | Tvar _, _ ->
      (* [typ] is more general than [ctyp] *)
      bind_none
        (without_instance typ ~f:(fun typ -> is_subtype typ ~of_:ctyp))
        (fun () -> false)
  | _, Tvar _ ->
      (* [ctyp] is more general than [typ] *)
      bind_none
        (without_instance ctyp ~f:(fun ctyp -> is_subtype typ ~of_:ctyp))
        (fun () ->
          Envi.Type.add_instance ctyp typ env ;
          true )
  | Ttuple typs, Ttuple ctyps -> (
    match
      List.for_all2 typs ctyps ~f:(fun typ ctyp -> is_subtype typ ~of_:ctyp)
    with
    | Ok x ->
        x
    | Unequal_lengths ->
        false )
  | ( Tarrow (typ1, typ2, Explicit, label1)
    , Tarrow (ctyp1, ctyp2, Explicit, label2) )
  | ( Tarrow (typ1, typ2, Implicit, label1)
    , Tarrow (ctyp1, ctyp2, Implicit, label2) ) ->
      ( match (label1, label2) with
      | Nolabel, Nolabel ->
          true
      | Labelled x, Labelled y when String.equal x y ->
          true
      | Optional x, Optional y when String.equal x y ->
          true
      | _ ->
          false )
      && is_subtype typ1 ~of_:ctyp1 && is_subtype typ2 ~of_:ctyp2
  | Tctor variant, Tctor constr_variant -> (
    (* Always try to unfold first, so that type aliases with phantom
         parameters can unify, as in OCaml.
      *)
    match unpack_decls ~loc typ ctyp env with
    | Some (typ, ctyp) ->
        is_subtype typ ~of_:ctyp
    | None ->
        if Int.equal variant.var_decl.tdec_id constr_variant.var_decl.tdec_id
        then
          match
            List.for_all2 variant.var_params constr_variant.var_params
              ~f:(fun param constr_param -> is_subtype param ~of_:constr_param)
          with
          | Ok x ->
              x
          | Unequal_lengths ->
              false
        else false )
  | Tctor _, _ | _, Tctor _ -> (
    (* Unfold an alias and compare again *)
    match unpack_decls ~loc typ ctyp env with
    | Some (typ, ctyp) ->
        is_subtype typ ~of_:ctyp
    | None ->
        false )
  | _, _ ->
      false

let rec add_implicits ~loc implicits typ env =
  match implicits with
  | [] ->
      typ
  | typ' :: implicits ->
      let typ = add_implicits ~loc implicits typ env in
      Envi.Type.mk (Tarrow (typ', typ, Implicit, Nolabel)) env

let free_type_vars ?depth typ =
  let empty = Set.empty (module Envi.Type) in
  let rec free_type_vars set typ =
    match typ.type_desc with
    | Tpoly (vars, typ) ->
        let poly_vars =
          Set.union_list
            (module Envi.Type)
            (List.map ~f:(Envi.Type.type_vars ?depth) vars)
        in
        Set.union set (Set.diff (free_type_vars empty typ) poly_vars)
    | Tarrow (typ1, typ2, _, _) ->
        Set.union
          (Envi.Type.type_vars ?depth typ1)
          (Envi.Type.type_vars ?depth typ2)
    | _ ->
        fold ~init:set typ ~f:free_type_vars
  in
  free_type_vars empty typ

let polymorphise typ env =
  let typ_vars = Set.to_list (free_type_vars ~depth:env.Envi.depth typ) in
  match typ_vars with
  | [] ->
      typ
  | _ ->
      Envi.Type.mk (Tpoly (typ_vars, typ)) env

let add_polymorphised name typ env =
  let typ = Envi.Type.flatten typ env in
  let typ = polymorphise typ env in
  Envi.add_name name typ env

let get_field (field : lid) env =
  let loc = field.loc in
  match Envi.TypeDecl.find_of_field field env with
  | Some
      ( ({tdec_desc= TRecord field_decls; tdec_ident; tdec_params; _} as decl)
      , i ) ->
      let vars, bound_vars, _ =
        Envi.Type.refresh_vars ~loc tdec_params Int.Map.empty env
      in
      let name =
        Location.mkloc
          ( match field.txt with
          | Longident.Ldot (m, _) ->
              Longident.Ldot (m, tdec_ident.txt)
          | _ ->
              Longident.Lident tdec_ident.txt )
          tdec_ident.loc
      in
      let rcd_type = Envi.TypeDecl.mk_typ ~params:vars ~ident:name decl env in
      let {fld_type; _} = List.nth_exn field_decls i in
      let rcd_type = Envi.Type.copy ~loc rcd_type bound_vars env in
      let fld_type = Envi.Type.copy ~loc fld_type bound_vars env in
      (i, fld_type, rcd_type)
  | _ ->
      raise (Error (loc, Unbound ("record field", field)))

let get_field_of_decl typ bound_vars field_decls (field : lid) env =
  let loc = field.loc in
  match field with
  | {txt= Longident.Lident name; _} -> (
    match
      List.findi field_decls ~f:(fun _ {fld_ident; _} ->
          String.equal fld_ident.txt name )
    with
    | Some (i, {fld_type; _}) ->
        let typ = Envi.Type.copy ~loc typ bound_vars env in
        let fld_type = Envi.Type.copy ~loc fld_type bound_vars env in
        (i, fld_type, typ)
    | None ->
        get_field field env )
  | _ ->
      get_field field env

let get_ctor (name : lid) env =
  let loc = name.loc in
  match (Envi.TypeDecl.find_of_constructor name env, name) with
  | ( Some
        ( ( { tdec_desc= TVariant ctors
            ; tdec_ident
            ; tdec_params
            ; tdec_implicit_params
            ; _ } as decl )
        , i )
    , name )
  | ( Some
        ( { tdec_desc= TExtend (name, decl, ctors)
          ; tdec_ident
          ; tdec_params
          ; tdec_implicit_params
          ; _ }
        , i )
    , _ ) ->
      let ctor = List.nth_exn ctors i in
      let make_name (tdec_ident : str) =
        Location.mkloc
          ( match name.txt with
          | Longident.Ldot (m, _) ->
              Longident.Ldot (m, tdec_ident.txt)
          | _ ->
              Longident.Lident tdec_ident.txt )
          tdec_ident.loc
      in
      let typ, params =
        match ctor.ctor_ret with
        | Some ({type_desc= Tctor {var_params; _}; _} as typ) ->
            (typ, var_params)
        | _ ->
            ( Envi.TypeDecl.mk_typ ~params:tdec_params
                ~ident:(make_name tdec_ident) decl env
            , tdec_params )
      in
      let args_typ =
        match ctor.ctor_args with
        | Ctor_record decl ->
            Envi.Type.mk
              (Tctor
                 { var_ident= make_name ctor.ctor_ident
                 ; var_params= params
                 ; var_implicit_params= tdec_implicit_params
                 ; var_decl= decl })
              env
        | Ctor_tuple [typ] ->
            typ
        | Ctor_tuple typs ->
            Envi.Type.mk (Ttuple typs) env
      in
      let bound_vars =
        Set.to_list
          (Set.union (Envi.Type.type_vars typ) (Envi.Type.type_vars args_typ))
      in
      let _, bound_vars, _ =
        Envi.Type.refresh_vars ~loc bound_vars Int.Map.empty env
      in
      let args_typ = Envi.Type.copy ~loc args_typ bound_vars env in
      let typ = Envi.Type.copy ~loc typ bound_vars env in
      (typ, args_typ)
  | _ ->
      raise (Error (loc, Unbound ("constructor", name)))

let rec check_pattern ~add env typ pat =
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | Ppat_any ->
      ({Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_any}, env)
  | Ppat_variable str ->
      let env = add str typ env in
      ({Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_variable str}, env)
  | Ppat_constraint (p, constr_typ) ->
      let ctyp, env = Typet.Type.import constr_typ env in
      check_type ~loc env typ ctyp ;
      let p, env = check_pattern ~add env ctyp p in
      let normalised_ctyp = Envi.Type.normalise_constr_names env ctyp in
      let constr_typ =
        Untype_ast.type_expr ~loc:constr_typ.type_loc normalised_ctyp
      in
      ( { Typedast.pat_loc= loc
        ; pat_type= typ
        ; pat_desc= Tpat_constraint (p, constr_typ) }
      , env )
  | Ppat_tuple ps ->
      let vars = List.map ps ~f:(fun _ -> Envi.Type.mkvar None env) in
      let tuple_typ = Envi.Type.mk (Ttuple vars) env in
      check_type ~loc env typ tuple_typ ;
      let ps, env = check_patterns ~add env vars ps in
      ( {Typedast.pat_loc= loc; pat_type= tuple_typ; pat_desc= Tpat_tuple ps}
      , env )
  | Ppat_or (p1, p2) ->
      let env = Envi.open_expr_scope env in
      let p1, env = check_pattern ~add env typ p1 in
      let scope1, env = Envi.pop_expr_scope env in
      let env = Envi.open_expr_scope env in
      let p2, env = check_pattern ~add env typ p2 in
      let scope2, env = Envi.pop_expr_scope env in
      (* Check that the assignments in each scope match. *)
      let () =
        Envi.Scope.fold_over ~init:() scope1 scope2
          ~type_variables:(fun ~key:_ ~data () ->
            match data with
            | `Both (var1, var2) ->
                check_type ~loc env var1 var2
            | _ ->
                () )
          ~names:(fun ~key:name ~data () ->
            match data with
            | `Both (typ1, typ2) ->
                check_type ~loc env typ1 typ2
            | _ ->
                raise (Error (loc, Variable_on_one_side name)) )
          ~type_decls:(fun ~key:name ~data:_ _ ->
            raise (Error (loc, Pattern_declaration ("type", name))) )
          ~fields:(fun ~key:name ~data:_ _ ->
            raise (Error (loc, Pattern_declaration ("field", name))) )
          ~ctors:(fun ~key:name ~data:_ _ ->
            raise (Error (loc, Pattern_declaration ("constructor", name))) )
          ~modules:(fun ~key:name ~data:_ _ ->
            raise (Error (loc, Pattern_declaration ("module", name))) )
          ~module_types:(fun ~key:name ~data:_ _ ->
            raise (Error (loc, Pattern_declaration ("module type", name))) )
          ~instances:(fun ~key:_ ~data:_ () -> ())
      in
      let env = Envi.push_scope scope2 env in
      ({Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_or (p1, p2)}, env)
  | Ppat_int i ->
      check_type ~loc env typ Initial_env.Type.int ;
      ({Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_int i}, env)
  | Ppat_record [] ->
      raise (Error (loc, Empty_record))
  | Ppat_record ((field, _) :: _ as fields) ->
      let typ, field_decls, bound_vars, env =
        match Envi.TypeDecl.find_unaliased_of_type ~loc typ env with
        | Some ({tdec_desc= TRecord field_decls; _}, bound_vars, env) ->
            (typ, field_decls, bound_vars, env)
        | _ -> (
          match Envi.TypeDecl.find_of_field field env with
          | Some (({tdec_desc= TRecord field_decls; tdec_params; _} as decl), _)
            ->
              let vars, bound_vars, env =
                Envi.Type.refresh_vars ~loc tdec_params Int.Map.empty env
              in
              let ident =
                Longident.(
                  match field.txt with
                  | Lident _ ->
                      Location.mkloc (Lident decl.tdec_ident.txt) loc
                  | Ldot (path, _) ->
                      Location.mkloc (Ldot (path, decl.tdec_ident.txt)) loc
                  | _ ->
                      failwith "Unhandled Lapply in field name")
              in
              let decl_type =
                Envi.TypeDecl.mk_typ ~params:vars ~ident decl env
              in
              check_type ~loc env typ decl_type ;
              (decl_type, field_decls, bound_vars, env)
          | _ ->
              raise (Error (loc, Unbound ("record field", field))) )
      in
      let field_typs =
        List.map fields ~f:(fun (field, _p) ->
            let _, field_typ, record_typ =
              get_field_of_decl typ bound_vars field_decls field env
            in
            ( try check_type ~loc:field.loc env record_typ typ
              with Error (_, Check_failed (_, _, Cannot_unify (typ, _))) ->
                raise (Error (field.loc, Wrong_record_field (field.txt, typ)))
            ) ;
            field_typ )
      in
      let ps, env =
        check_patterns ~add env field_typs (List.map ~f:snd fields)
      in
      let fields =
        List.map2_exn fields ps ~f:(fun (field, _) p -> (field, p))
      in
      ( {Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_record fields}
      , env )
  | Ppat_ctor (name, arg) ->
      let typ', args_typ = get_ctor name env in
      check_type ~loc env typ typ' ;
      let arg, env =
        match arg with
        | Some arg ->
            let arg, env = check_pattern ~add env args_typ arg in
            (Some arg, env)
        | None ->
            let typ = Envi.Type.mk (Ttuple []) env in
            check_type ~loc env args_typ typ ;
            (None, env)
      in
      ( {Typedast.pat_loc= loc; pat_type= typ'; pat_desc= Tpat_ctor (name, arg)}
      , env )

and check_patterns ~add env typs pats =
  let rev_pats, env =
    List.fold2_exn ~init:([], env) typs pats ~f:(fun (rev_pats, env) typ pat ->
        let pat, env = check_pattern ~add env typ pat in
        (pat :: rev_pats, env) )
  in
  (List.rev rev_pats, env)

let rec get_expression env expected exp =
  let loc = exp.exp_loc in
  match exp.exp_desc with
  | Pexp_apply (f, es) ->
      let f_typ = Envi.Type.mkvar None env in
      let f, env = get_expression env f_typ f in
      let (typ, env), es =
        List.fold_map ~init:(f.Typedast.exp_type, env) es
          ~f:(fun (f_typ, env) (label, e) ->
            let f_typ = Envi.Type.bubble_label env label f_typ in
            let e_typ = Envi.Type.mkvar None env in
            let res_typ = Envi.Type.mkvar None env in
            let arrow =
              Envi.Type.mk (Tarrow (e_typ, res_typ, Explicit, label)) env
            in
            check_type ~loc:e.exp_loc env f_typ arrow ;
            let e_typ =
              match label with
              | Optional _ ->
                  Initial_env.Type.option e_typ
              | _ ->
                  e_typ
            in
            let e, env = get_expression env e_typ e in
            ((res_typ, env), (label, e)) )
      in
      let typ =
        Envi.Type.discard_optional_labels @@ Envi.Type.flatten typ env
      in
      (* Squash nested applies from implicit arguments. *)
      let f, es =
        match f.exp_desc with
        | Texp_apply (f', args) ->
            if
              List.for_all args ~f:(function
                | _, {exp_desc= Texp_unifiable _; _} ->
                    true
                | _ ->
                    false )
            then (f', args @ es)
            else (f, es)
        | _ ->
            (f, es)
      in
      check_type ~loc env expected typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_apply (f, es)}, env)
  | Pexp_variable name ->
      let typ = Envi.find_name ~loc name env in
      let implicits, result_typ = Envi.Type.get_implicits [] typ in
      check_type ~loc env expected result_typ ;
      let implicits =
        List.map implicits ~f:(fun (label, typ) ->
            (label, Envi.Type.new_implicit_var ~loc typ env) )
      in
      let e =
        {Typedast.exp_loc= loc; exp_type= typ; exp_desc= Texp_variable name}
      in
      let e =
        if List.is_empty implicits then e
        else
          { Typedast.exp_loc= loc
          ; exp_type= result_typ
          ; exp_desc= Texp_apply (e, implicits) }
      in
      (e, env)
  | Pexp_literal (Int i) ->
      let typ = Initial_env.Type.int in
      check_type ~loc env expected typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_literal (Int i)}, env)
  | Pexp_literal (Bool _b) ->
      failwith "Unhandled boolean literal"
  | Pexp_literal (Field _f) ->
      failwith "Unhandled field literal"
  | Pexp_literal (String s) ->
      let typ = Initial_env.Type.string in
      check_type ~loc env expected typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_literal (String s)}, env)
  | Pexp_fun (label, p, body, explicit) ->
      let env = Envi.open_expr_scope env in
      let p_typ = Envi.Type.mkvar None env in
      let body_typ = Envi.Type.mkvar None env in
      let typ = Envi.Type.mk (Tarrow (p_typ, body_typ, explicit, label)) env in
      check_type ~loc env expected typ ;
      let add_name =
        match label with
        | Optional _ ->
            fun name typ -> Envi.add_name name (Initial_env.Type.option typ)
        | _ ->
            Envi.add_name
      in
      (* In OCaml, function arguments can't be polymorphic, so each check refines
       them rather than instantiating the parameters. *)
      let p, env = check_pattern ~add:add_name env p_typ p in
      let body, env = get_expression env body_typ body in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env typ ;
      ( { exp_loc= loc
        ; exp_type= typ
        ; exp_desc= Texp_fun (label, p, body, explicit) }
      , env )
  | Pexp_newtype (name, body) ->
      let env = Envi.open_expr_scope env in
      let decl =
        { tdec_ident= name
        ; tdec_params= []
        ; tdec_implicit_params= []
        ; tdec_desc= TUnfold (Ast_build.Type.none ())
        ; tdec_loc= loc }
      in
      let decl, env = Typet.TypeDecl.import decl env in
      let typ =
        match decl.tdec_desc with TUnfold typ -> typ | _ -> assert false
      in
      (* Create a self-referencing type declaration. *)
      typ.type_desc
      <- Tctor
           { var_ident= mk_lid name
           ; var_params= []
           ; var_implicit_params= []
           ; var_decl= decl } ;
      let body, env = get_expression env expected body in
      (* Substitute the self-reference for a type variable. *)
      typ.type_desc <- Tvar (Some name, Explicit) ;
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env body.exp_type ;
      ( { exp_loc= loc
        ; exp_type= body.exp_type
        ; exp_desc= Texp_newtype (name, body) }
      , env )
  | Pexp_seq (e1, e2) ->
      let e1, env = get_expression env Initial_env.Type.unit e1 in
      let e2, env = get_expression env expected e2 in
      ({exp_loc= loc; exp_type= e2.exp_type; exp_desc= Texp_seq (e1, e2)}, env)
  | Pexp_let (p, e1, e2) ->
      let env = Envi.open_expr_scope env in
      let p, e1, env = check_binding env p e1 in
      let e2, env = get_expression env expected e2 in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env e2.exp_type ;
      ( {exp_loc= loc; exp_type= e2.exp_type; exp_desc= Texp_let (p, e1, e2)}
      , env )
  | Pexp_constraint (e, typ') ->
      let typ, env = Typet.Type.import typ' env in
      check_type ~loc env expected typ ;
      let e, env = get_expression env typ e in
      check_type ~loc env e.exp_type typ ;
      let typ' =
        Untype_ast.type_expr ~loc:typ'.type_loc
          (Envi.Type.normalise_constr_names env typ)
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_constraint (e, typ')}, env)
  | Pexp_tuple es ->
      let typs = List.map es ~f:(fun _ -> Envi.Type.mkvar None env) in
      let typ = Envi.Type.mk (Ttuple typs) env in
      check_type ~loc env expected typ ;
      let env = ref env in
      let es =
        List.map2_exn es typs ~f:(fun e expected ->
            let e, env' = get_expression !env expected e in
            env := env' ;
            e )
      in
      let typ =
        Envi.Type.mk (Ttuple (List.map es ~f:(fun {exp_type= t; _} -> t))) !env
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_tuple es}, !env)
  | Pexp_match (e, cases) ->
      let e_typ = Envi.Type.mkvar None env in
      let e, env = get_expression env e_typ e in
      let typ = e.exp_type in
      let env, cases =
        List.fold_map ~init:env cases ~f:(fun env (p, e) ->
            let env = Envi.open_expr_scope env in
            let p, env = check_pattern ~add:add_polymorphised env typ p in
            let e, env = get_expression env expected e in
            let env = Envi.close_expr_scope env in
            (env, (p, e)) )
      in
      Envi.Type.update_depths env expected ;
      ({exp_loc= loc; exp_type= expected; exp_desc= Texp_match (e, cases)}, env)
  | Pexp_field (e, field) ->
      let field_info =
        match field.txt with
        | Lident _ ->
            None
        | Ldot (path, _) -> (
          match Envi.TypeDecl.find_of_field field env with
          | Some (({tdec_desc= TRecord field_decls; tdec_params; _} as decl), i)
            ->
              let vars, bound_vars, env =
                Envi.Type.refresh_vars ~loc tdec_params Int.Map.empty env
              in
              let ident =
                Location.mkloc (Longident.Ldot (path, decl.tdec_ident.txt)) loc
              in
              let decl_type =
                Envi.TypeDecl.mk_typ ~params:vars ~ident decl env
              in
              let {fld_type; _} = List.nth_exn field_decls i in
              let fld_type = Envi.Type.copy ~loc fld_type bound_vars env in
              check_type ~loc env expected fld_type ;
              Some (fld_type, decl_type, env)
          | _ ->
              None )
        | Lapply _ ->
            failwith "Unhandled Lapply in field name"
      in
      let typ, decl_type, env, resolved =
        match field_info with
        | Some (fld_type, decl_type, env) ->
            (fld_type, decl_type, env, true)
        | None ->
            let fld_type = expected in
            let decl_type = Envi.Type.mkvar None env in
            (fld_type, decl_type, env, false)
      in
      let e, env = get_expression env decl_type e in
      let typ, env =
        if resolved then (typ, env)
        else
          match Envi.TypeDecl.find_unaliased_of_type ~loc e.exp_type env with
          | Some ({tdec_desc= TRecord field_decls; _}, bound_vars, env) -> (
            match
              List.find field_decls ~f:(fun {fld_ident; _} ->
                  match field.txt with
                  | Lident field ->
                      String.equal fld_ident.txt field
                  | _ ->
                      false
                  (* This case shouldn't happen! *) )
            with
            | Some {fld_type; _} ->
                let fld_type = Envi.Type.copy ~loc fld_type bound_vars env in
                check_type ~loc env typ fld_type ;
                (fld_type, env)
            | None ->
                raise (Error (loc, Wrong_record_field (field.txt, e.exp_type)))
            )
          | _ -> (
            match Envi.TypeDecl.find_of_field field env with
            | Some
                (({tdec_desc= TRecord field_decls; tdec_params; _} as decl), i)
              ->
                let vars, bound_vars, env =
                  Envi.Type.refresh_vars ~loc tdec_params Int.Map.empty env
                in
                let ident =
                  Longident.(
                    match field.txt with
                    | Lident _ ->
                        Location.mkloc (Lident decl.tdec_ident.txt) loc
                    | Ldot (path, _) ->
                        Location.mkloc (Ldot (path, decl.tdec_ident.txt)) loc
                    | _ ->
                        failwith "Unhandled Lapply in field name")
                in
                let e_typ =
                  Envi.TypeDecl.mk_typ ~params:vars ~ident decl env
                in
                check_type ~loc env e.exp_type e_typ ;
                let {fld_type; _} = List.nth_exn field_decls i in
                let fld_type = Envi.Type.copy ~loc fld_type bound_vars env in
                let fld_type = Envi.Type.copy ~loc fld_type bound_vars env in
                (fld_type, env)
            | _ ->
                raise (Error (loc, Unbound ("record field", field))) )
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_field (e, field)}, env)
  | Pexp_record ([], _) ->
      raise (Error (loc, Empty_record))
  | Pexp_record (((field, _) :: _ as fields), ext) ->
      let typ, ext, env =
        match ext with
        | Some ext ->
            let ext, env = get_expression env expected ext in
            (ext.exp_type, Some ext, env)
        | None ->
            (expected, None, env)
      in
      let typ, field_decls, bound_vars, env =
        match Envi.TypeDecl.find_unaliased_of_type ~loc typ env with
        | Some ({tdec_desc= TRecord field_decls; _}, bound_vars, env) ->
            (typ, field_decls, bound_vars, env)
        | _ -> (
          match Envi.TypeDecl.find_of_field field env with
          | Some (({tdec_desc= TRecord field_decls; tdec_params; _} as decl), _)
            ->
              let vars, bound_vars, env =
                Envi.Type.refresh_vars ~loc tdec_params Int.Map.empty env
              in
              let ident =
                Longident.(
                  match field.txt with
                  | Lident _ ->
                      Location.mkloc (Lident decl.tdec_ident.txt) loc
                  | Ldot (path, _) ->
                      Location.mkloc (Ldot (path, decl.tdec_ident.txt)) loc
                  | _ ->
                      failwith "Unhandled Lapply in field name")
              in
              let decl_type =
                Envi.TypeDecl.mk_typ ~params:vars ~ident decl env
              in
              check_type ~loc env typ decl_type ;
              (decl_type, field_decls, bound_vars, env)
          | _ ->
              raise (Error (loc, Unbound ("record field", field))) )
      in
      let env = ref env in
      let fields_filled = Array.create ~len:(List.length field_decls) false in
      let fields =
        List.map fields ~f:(fun (field, e) ->
            let i, field_typ, record_typ =
              get_field_of_decl typ bound_vars field_decls field !env
            in
            ( try check_type ~loc:field.loc !env record_typ typ
              with Error (_, Check_failed (_, _, Cannot_unify (typ, _))) ->
                raise (Error (field.loc, Wrong_record_field (field.txt, typ)))
            ) ;
            let e, env' = get_expression !env field_typ e in
            ( if fields_filled.(i) then
              let name = (List.nth_exn field_decls i).fld_ident.txt in
              raise (Error (field.loc, Repeated_field name)) ) ;
            fields_filled.(i) <- true ;
            env := env' ;
            (field, e) )
      in
      ( match ext with
      | Some _ ->
          () (* TODO: warn when all fields have been provided. *)
      | None ->
          let fields_filled = Array.to_list fields_filled in
          let names =
            List.fold2_exn ~init:[] fields_filled field_decls
              ~f:(fun names filled {fld_ident; _} ->
                if filled then names else fld_ident.txt :: names )
          in
          if not (List.is_empty names) then
            raise (Error (loc, Missing_fields names)) ) ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_record (fields, ext)}, !env)
  | Pexp_ctor (name, arg) ->
      let typ, arg_typ = get_ctor name env in
      check_type ~loc env expected typ ;
      let arg, env =
        match arg with
        | Some arg ->
            let arg, env = get_expression env arg_typ arg in
            (Some arg, env)
        | None ->
            let typ = Envi.Type.mk (Ttuple []) env in
            ( try check_type ~loc env arg_typ typ
              with _ -> raise (Error (loc, Argument_expected name.txt)) ) ;
            (None, env)
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_ctor (name, arg)}, env)
  | Pexp_unifiable _ ->
      raise (Error (loc, Unifiable_expr))
  | Pexp_if (e1, e2, None) ->
      check_type ~loc env Initial_env.Type.unit expected ;
      let e1, env = get_expression env Initial_env.Type.bool e1 in
      let e2, env = get_expression env Initial_env.Type.unit e2 in
      ( { exp_loc= loc
        ; exp_type= Initial_env.Type.unit
        ; exp_desc= Texp_if (e1, e2, None) }
      , env )
  | Pexp_if (e1, e2, Some e3) ->
      let e1, env = get_expression env Initial_env.Type.bool e1 in
      let e2, env = get_expression env expected e2 in
      let e3, env = get_expression env expected e3 in
      ( {exp_loc= loc; exp_type= expected; exp_desc= Texp_if (e1, e2, Some e3)}
      , env )

and check_binding ?(toplevel = false) (env : Envi.t) p e : 's =
  let loc = e.exp_loc in
  let typ = Envi.Type.mkvar None env in
  let env = Envi.open_expr_scope env in
  let e, env = get_expression env typ e in
  let env = Envi.close_expr_scope env in
  Envi.Type.update_depths env e.exp_type ;
  let exp_type = Envi.Type.flatten e.exp_type env in
  let e = {e with exp_type} in
  let typ_vars = free_type_vars ~depth:env.Envi.depth exp_type in
  let implicit_vars =
    Envi.Type.flattened_implicit_vars ~loc ~toplevel
      ~is_subtype:(is_subtype ~loc:e.exp_loc)
      typ_vars env
  in
  let e, env =
    List.fold ~init:(e, env) implicit_vars ~f:(fun (e, env) var ->
        match var.exp_desc with
        | Texp_unifiable {expression= None; name; _} ->
            let exp_type =
              Envi.Type.mk
                (Tarrow (var.exp_type, e.exp_type, Implicit, Nolabel))
                env
            in
            let p =
              { Typedast.pat_desc= Tpat_variable name
              ; pat_loc= loc
              ; pat_type= var.exp_type }
            in
            ( { Typedast.exp_desc= Texp_fun (Nolabel, p, e, Implicit)
              ; exp_type
              ; exp_loc= loc }
            , env )
        | _ ->
            raise (Error (var.exp_loc, No_unifiable_expr)) )
  in
  let loc = p.pat_loc in
  match (p.pat_desc, implicit_vars) with
  | Ppat_variable str, _ ->
      let typ =
        if Set.is_empty typ_vars then e.exp_type
        else Envi.Type.mk (Tpoly (Set.to_list typ_vars, e.exp_type)) env
      in
      let env = Envi.add_name str typ env in
      let p =
        {Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_variable str}
      in
      (p, e, env)
  | Ppat_constraint (({pat_desc= Ppat_variable str; _} as p'), typ), _ ->
      let ctyp, env = Typet.Type.import typ env in
      check_type ~loc env e.exp_type ctyp ;
      let ctyp =
        if Set.is_empty typ_vars then ctyp
        else Envi.Type.mk (Tpoly (Set.to_list typ_vars, ctyp)) env
      in
      let env = Envi.add_name str ctyp env in
      let p' =
        { Typedast.pat_loc= p'.pat_loc
        ; pat_type= ctyp
        ; pat_desc= Tpat_variable str }
      in
      let typ =
        Untype_ast.type_expr ~loc (Envi.Type.normalise_constr_names env ctyp)
      in
      let p =
        { Typedast.pat_loc= p.pat_loc
        ; pat_type= ctyp
        ; pat_desc= Tpat_constraint (p', typ) }
      in
      (p, e, env)
  | _, [] ->
      let p, env = check_pattern ~add:add_polymorphised env e.exp_type p in
      (p, e, env)
  | _, implicit :: _ ->
      raise (Error (loc, No_instance implicit.exp_type))

let type_extension ~loc variant ctors env =
  let {Parsetypes.var_ident; var_params; var_implicit_params= _} = variant in
  let ({tdec_ident; tdec_params; tdec_implicit_params; tdec_desc; _} as decl) =
    match Envi.raw_find_type_declaration var_ident env with
    | open_decl ->
        open_decl
    | exception _ ->
        raise (Error (loc, Unbound ("type constructor", var_ident)))
  in
  ( match tdec_desc with
  | TOpen ->
      ()
  | _ ->
      raise (Error (loc, Not_extensible var_ident.txt)) ) ;
  ( match List.iter2 tdec_params var_params ~f:(fun _ _ -> ()) with
  | Ok _ ->
      ()
  | Unequal_lengths ->
      raise (Error (loc, Extension_different_arity var_ident.txt)) ) ;
  let decl =
    { Parsetypes.tdec_ident
    ; tdec_params= var_params
    ; tdec_implicit_params=
        List.map ~f:(Untype_ast.type_expr ~loc) tdec_implicit_params
    ; tdec_desc= TExtend (var_ident, decl, ctors)
    ; tdec_loc= loc }
  in
  let decl, env = Typet.TypeDecl.import decl env in
  let ctors =
    match decl.tdec_desc with
    | TExtend (_, _, ctors) ->
        ctors
    | _ ->
        failwith "Expected a TExtend."
  in
  let variant =
    { var_ident
    ; var_implicit_params= decl.tdec_implicit_params
    ; var_decl= decl
    ; var_params= decl.tdec_params }
  in
  (env, variant, ctors)

let rec check_signature_item env item =
  let loc = item.sig_loc in
  match item.sig_desc with
  | Psig_value (name, typ) ->
      let env = Envi.open_expr_scope env in
      let typ', env = Typet.Type.import typ env in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env typ' ;
      let env = add_polymorphised name typ' env in
      (env, {Typedast.sig_desc= Tsig_value (name, typ); sig_loc= loc})
  | Psig_instance (name, typ) ->
      let env = Envi.open_expr_scope env in
      let typ', env = Typet.Type.import typ env in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env typ' ;
      let env = add_polymorphised name typ' env in
      let env = Envi.add_implicit_instance name.txt typ' env in
      (env, {Typedast.sig_desc= Tsig_instance (name, typ); sig_loc= loc})
  | Psig_type decl ->
      let _decl, env = Typet.TypeDecl.import decl env in
      (env, {Typedast.sig_desc= Tsig_type decl; sig_loc= loc})
  | Psig_module (name, msig) ->
      let msig, m, env =
        check_module_sig env (Envi.relative_path env name.txt) msig
      in
      let env =
        match m with
        | Envi.Scope.Immediate m ->
            Envi.add_module name m env
        | Envi.Scope.Deferred path ->
            Envi.add_deferred_module name path env
      in
      (env, {Typedast.sig_desc= Tsig_module (name, msig); sig_loc= loc})
  | Psig_modtype (name, signature) ->
      let env = Envi.open_module name.txt env in
      let signature, m_env, env =
        check_module_sig env (Envi.relative_path env name.txt) signature
      in
      let env = Envi.add_module_type name.txt m_env env in
      (env, {Typedast.sig_desc= Tsig_modtype (name, signature); sig_loc= loc})
  | Psig_open name ->
      let m = Envi.find_module ~loc name env in
      let env = Envi.open_namespace_scope m env in
      (env, {Typedast.sig_desc= Tsig_open name; sig_loc= loc})
  | Psig_typeext (variant, ctors) ->
      let env, _variant, _ctors = type_extension ~loc variant ctors env in
      (env, {Typedast.sig_desc= Tsig_typeext (variant, ctors); sig_loc= loc})
  | Psig_request (arg, ctor_decl) ->
      let open Ast_build in
      let variant =
        Type.variant ~loc ~params:[Type.none ~loc ()]
          (Lid.of_list ["Snarky__Request"; "t"])
      in
      let ctor_ret =
        Type.mk ~loc (Ptyp_ctor {variant with var_params= [arg]})
      in
      let ctor_decl = {ctor_decl with ctor_ret= Some ctor_ret} in
      let env, _variant, _ctors =
        type_extension ~loc variant [ctor_decl] env
      in
      (env, {Typedast.sig_desc= Tsig_request (arg, ctor_decl); sig_loc= loc})
  | Psig_multiple sigs ->
      let env, sigs = check_signature env sigs in
      (env, {Typedast.sig_desc= Tsig_multiple sigs; sig_loc= loc})

and check_signature env signature =
  List.fold_map ~init:env signature ~f:check_signature_item

and check_module_sig env path msig =
  let loc = msig.msig_loc in
  match msig.msig_desc with
  | Pmty_sig signature ->
      let env = Envi.open_absolute_module (Some path) env in
      let env, signature = check_signature env signature in
      let m, env = Envi.pop_module ~loc env in
      ( {Typedast.msig_desc= Tmty_sig signature; msig_loc= loc}
      , Envi.Scope.Immediate m
      , env )
  | Pmty_name lid ->
      let m =
        match Envi.find_module_deferred ~loc lid env with
        | Some m ->
            m
        | None ->
            Envi.Scope.Deferred lid.txt
      in
      ({Typedast.msig_desc= Tmty_name lid; msig_loc= loc}, m, env)
  | Pmty_abstract ->
      let env = Envi.open_absolute_module (Some path) env in
      let m, env = Envi.pop_module ~loc env in
      ( {Typedast.msig_desc= Tmty_abstract; msig_loc= loc}
      , Envi.Scope.Immediate m
      , env )
  | Pmty_functor (f_name, f, msig) ->
      let f, f_mty, env = check_module_sig env (Lident f_name.txt) f in
      let ftor path f_instance =
        (* We want the functored module to be accessible only in un-prefixed
           space.
        *)
        let env = Envi.open_absolute_module None env in
        let env =
          match f_instance with
          | Envi.Scope.Immediate f ->
              Envi.add_module f_name f env
          | Envi.Scope.Deferred path ->
              Envi.add_deferred_module f_name path env
        in
        (* TODO: check that f_instance matches f_mty *)
        let msig, m, _env = check_module_sig env path msig in
        match m with
        | Envi.Scope.Immediate m ->
            (m, msig)
        | Envi.Scope.Deferred path ->
            (Envi.find_module ~loc (Location.mkloc path loc) env, msig)
      in
      (* Check that f_mty builds the functor as expected. *)
      let _, msig = ftor (Lapply (path, Lident f_name.txt)) f_mty in
      let m = Envi.make_functor path (fun path f -> fst (ftor path f)) in
      ( {Typedast.msig_desc= Tmty_functor (f_name, f, msig); msig_loc= loc}
      , Envi.Scope.Immediate m
      , env )

let in_decl = ref false

let rec check_statement env stmt =
  let loc = stmt.stmt_loc in
  match stmt.stmt_desc with
  | Value (p, e) ->
      let env = Envi.open_expr_scope env in
      let p, e, env = check_binding ~toplevel:true env p e in
      let scope, env = Envi.pop_expr_scope env in
      (* Uplift the names from the expression scope, discarding the scope and
         its associated type variables etc. *)
      let env = Envi.join_expr_scope env scope in
      (env, {Typedast.stmt_loc= loc; stmt_desc= Tstmt_value (p, e)})
  | Instance (name, e) ->
      let env = Envi.open_expr_scope env in
      let p = {pat_desc= Ppat_variable name; pat_loc= name.loc} in
      let _, e, env = check_binding ~toplevel:true env p e in
      let scope, env = Envi.pop_expr_scope env in
      let env = Envi.join_expr_scope env scope in
      let env = Envi.add_implicit_instance name.txt e.exp_type env in
      (env, {Typedast.stmt_loc= loc; stmt_desc= Tstmt_instance (name, e)})
  | TypeDecl decl when !in_decl ->
      let decl, env = Typet.TypeDecl.import decl env in
      let stmt =
        { Typedast.stmt_loc= loc
        ; stmt_desc= Tstmt_type (Untype_ast.type_decl ~loc decl) }
      in
      (env, stmt)
  | TypeDecl decl ->
      in_decl := true ;
      let ret =
        let stmt =
          match Codegen.typ_of_decl ~loc decl with
          | Some typ_stmts ->
              {stmt with stmt_desc= Multiple typ_stmts}
          | None ->
              stmt
        in
        check_statement env stmt
      in
      in_decl := false ;
      ret
  | Module (name, m) ->
      let env = Envi.open_module name.txt env in
      let env, m = check_module_expr env m in
      let m_env, env = Envi.pop_module ~loc env in
      let env = Envi.add_module name m_env env in
      (env, {Typedast.stmt_loc= loc; stmt_desc= Tstmt_module (name, m)})
  | ModType (name, signature) ->
      let signature, m_env, env =
        check_module_sig env (Envi.relative_path env name.txt) signature
      in
      let env = Envi.add_module_type name.txt m_env env in
      ( env
      , {Typedast.stmt_loc= loc; stmt_desc= Tstmt_modtype (name, signature)} )
  | Open name ->
      let m = Envi.find_module ~loc name env in
      ( Envi.open_namespace_scope m env
      , {Typedast.stmt_loc= loc; stmt_desc= Tstmt_open name} )
  | TypeExtension (variant, ctors) ->
      let env, _variant, _ctors = type_extension ~loc variant ctors env in
      (env, {Typedast.stmt_loc= loc; stmt_desc= Tstmt_typeext (variant, ctors)})
  | Request (arg, ctor_decl, handler) ->
      let open Ast_build in
      let variant =
        Type.variant ~loc ~params:[Type.none ~loc ()]
          (Lid.of_list ["Snarky__Request"; "t"])
      in
      let ctor_ret =
        Type.mk ~loc (Ptyp_ctor {variant with var_params= [arg]})
      in
      let ctor_decl = {ctor_decl with ctor_ret= Some ctor_ret} in
      let env, _, ctors = type_extension ~loc variant [ctor_decl] env in
      let ctor_decl =
        match ctors with
        | [ctor] ->
            { (Untype_ast.ctor_decl ~loc ctor) with
              ctor_ret=
                Some
                  (Type.mk ~loc
                     (Ptyp_ctor
                        (Type.variant ~loc ~params:[arg]
                           (Lid.of_list ["Snarky"; "Request"; "t"])))) }
        | _ ->
            failwith "Wrong number of constructors returned for Request."
      in
      let name = ctor_decl.ctor_ident.txt in
      let handler, env =
        match handler with
        | Some (pat, body) ->
            let loc, pat_loc =
              Location.(
                match pat with
                | Some pat ->
                    ( {body.exp_loc with loc_start= pat.pat_loc.loc_start}
                    , pat.pat_loc )
                | None ->
                    (body.exp_loc, body.exp_loc))
            in
            let p = Pat.var ~loc ("handle_" ^ name) in
            let e =
              let request = Lid.of_name "request" in
              let respond = Lid.of_name "respond" in
              let body =
                Exp.let_ ~loc (Pat.var "unhandled")
                  (Exp.var (Lid.of_list ["Snarky__Request"; "unhandled"]))
                  (Exp.match_ ~loc:stmt.stmt_loc
                     (Exp.var ~loc (Lid.of_name "request"))
                     [ ( Pat.ctor ~loc:pat_loc (Lid.of_name name) ?args:pat
                       , body )
                     ; ( Pat.any ()
                       , Exp.var (Lid.of_list ["Snarky__Request"; "unhandled"])
                       ) ])
              in
              Exp.fun_
                (Pat.ctor
                   (Lid.of_list ["Snarky__Request"; "With"])
                   ~args:(Pat.record [Pat.field request; Pat.field respond]))
                body
            in
            let _p, e, env = check_binding ~toplevel:true env p e in
            let pat, body =
              match e with
              | { exp_desc=
                    Texp_fun
                      ( Nolabel
                      , _
                      , { exp_desc=
                            Texp_let
                              ( _
                              , _
                              , { exp_desc=
                                    Texp_match
                                      ( _
                                      , [ ( {pat_desc= Tpat_ctor (_, pat); _}
                                          , body )
                                        ; _ ] )
                                ; _ } )
                        ; _ }
                      , _ )
                ; _ } ->
                  (pat, body)
              | _ ->
                  failwith "Unexpected output of check_binding for Request"
            in
            (Some (pat, body), env)
        | None ->
            (None, env)
      in
      (env, {stmt_loc= loc; stmt_desc= Tstmt_request (arg, ctor_decl, handler)})
  | Multiple stmts ->
      let env, stmts = List.fold_map ~init:env stmts ~f:check_statement in
      (env, {stmt_loc= loc; stmt_desc= Tstmt_multiple stmts})

and check_module_expr env m =
  let loc = m.mod_loc in
  match m.mod_desc with
  | Structure stmts ->
      let env, stmts = List.fold_map ~f:check_statement ~init:env stmts in
      (env, {Typedast.mod_loc= loc; mod_desc= Tmod_struct stmts})
  | ModName name ->
      let path = Envi.current_path env in
      (* Remove the module placed on the stack by the caller. *)
      let _, env = Envi.pop_module ~loc env in
      let m' = Envi.find_module ~loc name env in
      let env = Envi.push_scope {m' with path} env in
      (env, {Typedast.mod_loc= loc; mod_desc= Tmod_name name})
  | Functor (f_name, f, m) ->
      let path = Option.value_exn (Envi.current_path env) in
      (* Remove the module placed on the stack by the caller. *)
      let _, env = Envi.pop_module ~loc env in
      let f, f', env = check_module_sig env (Lident f_name.txt) f in
      let ftor path f_instance =
        (* We want the functored module to be accessible only in un-prefixed
           space.
        *)
        let env = Envi.open_absolute_module None env in
        let env =
          match f_instance with
          | Envi.Scope.Immediate f ->
              Envi.add_module f_name f env
          | Envi.Scope.Deferred path ->
              Envi.add_deferred_module f_name path env
        in
        (* TODO: check that f_instance matches f' *)
        let env = Envi.open_absolute_module (Some path) env in
        let env, m' = check_module_expr env m in
        let m, _env = Envi.pop_module ~loc env in
        (m, m')
      in
      (* Check that f builds the functor as expected. *)
      let _, m = ftor (Lapply (path, Lident f_name.txt)) f' in
      let env =
        Envi.push_scope
          (Envi.make_functor path (fun path f -> fst (ftor path f)))
          env
      in
      (env, {m with mod_desc= Tmod_functor (f_name, f, m)})

let check_signature env signature =
  Envi.set_type_predeclaring env ;
  let ret = check_signature env signature in
  Envi.unset_type_predeclaring env ;
  ret

let check (ast : statement list) (env : Envi.t) =
  List.fold_map ast ~init:env ~f:check_statement

(* Error handling *)

open Format

let pp_typ = Typeprint.type_expr

let rec report_error ppf = function
  | Check_failed (typ, constr_typ, err) ->
      fprintf ppf
        "@[<v>@[<hov>Incompatable types@ @[<h>%a@] and@ @[<h>%a@]:@]@;%a@]"
        pp_typ typ pp_typ constr_typ report_error err
  | Cannot_unify (typ, constr_typ) ->
      fprintf ppf "@[<hov>Cannot unify@ @[<h>%a@] and@ @[<h>%a@]@]" pp_typ typ
        pp_typ constr_typ
  | Recursive_variable typ ->
      fprintf ppf
        "@[<hov>The variable@ @[<h>%a@](%d) would have an instance that \
         contains itself.@]"
        pp_typ typ typ.type_id
  | Unbound (kind, value) ->
      fprintf ppf "@[<hov>Unbound %s@ %a.@]" kind Longident.pp value.txt
  | Unbound_value value ->
      fprintf ppf "Unbound value %s." value.txt
  | Variable_on_one_side name ->
      fprintf ppf
        "@[<hov>Variable@ %s@ must@ occur@ on@ both@ sides@ of@ this@ '|'@ \
         pattern.@]"
        name
  | Pattern_declaration (kind, name) ->
      fprintf ppf "@[<hov>Unexpected %s declaration for %s within a pattern.@]"
        kind name
  | Empty_record ->
      fprintf ppf "Unexpected empty record."
  | Wrong_record_field (field, typ) ->
      fprintf ppf
        "@[<hov>This record expression is expected to have type@ \
         @[<h>%a@]@;The field %a does not belong to type@ @[<h>%a@].@]"
        pp_typ typ Longident.pp field pp_typ typ
  | Repeated_field field ->
      fprintf ppf "@[<hov>The record field %s is defined several times.@]"
        field
  | Missing_fields fields ->
      fprintf ppf "@[<hov>Some record fields are undefined:@ %a@]"
        (pp_print_list ~pp_sep:pp_print_space pp_print_string)
        fields
  | Wrong_type_description (kind, name) ->
      fprintf ppf
        "@[<hov>Internal error: Expected a type declaration of kind %s, but \
         instead got %s@]"
        kind name.txt
  | Unifiable_expr ->
      fprintf ppf "Internal error: Unexpected implicit variable."
  | No_unifiable_expr ->
      fprintf ppf "Internal error: Expected an unresolved implicit variable."
  | No_instance typ ->
      fprintf ppf
        "@[<hov>Could not find an instance for an implicit variable of type@ \
         @[<h>%a@].@]"
        pp_typ typ
  | Argument_expected lid ->
      fprintf ppf "@[<hov>The constructor %a expects an argument.@]"
        Longident.pp lid
  | Not_extensible lid ->
      fprintf ppf "@[<hov>Type definition %a is not extensible.@]" Longident.pp
        lid
  | Extension_different_arity lid ->
      fprintf ppf
        "@[<hov>This extension does not match the definition of type %a@;They \
         have different arities.@]"
        Longident.pp lid

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
    | _ ->
        None )
