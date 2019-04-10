open Core_kernel
open Parsetypes

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

let unpack_decls typ ctyp env =
  match (typ.type_desc, ctyp.type_desc) with
  | Tctor variant, Tctor cvariant ->
      let decl_id, cdecl_id = (variant.var_decl_id, cvariant.var_decl_id) in
      let unfold_typ () =
        Option.map (Envi.TypeDecl.unfold_alias typ env) ~f:(fun typ ->
            (typ, ctyp) )
      in
      let unfold_ctyp () =
        Option.map (Envi.TypeDecl.unfold_alias ctyp env) ~f:(fun ctyp ->
            (typ, ctyp) )
      in
      (* Try to unfold the oldest type definition first. *)
      if decl_id < cdecl_id then bind_none (Some (unfold_ctyp ())) unfold_typ
      else bind_none (Some (unfold_typ ())) unfold_ctyp
  | _ -> None

let rec check_type_aux typ ctyp env =
  let without_instance ~f (typ : type_expr) env =
    match Envi.Type.instance env typ with
    | Some typ' -> (
        Envi.Type.clear_instance typ env ;
        f typ' env ;
        match Envi.Type.instance env typ with
        | Some _ -> raise (Error (typ.type_loc, Recursive_variable typ))
        | None -> Some (Envi.Type.add_instance typ typ' env) )
    | None -> None
  in
  match (typ.type_desc, ctyp.type_desc) with
  | _, _ when Int.equal typ.type_id ctyp.type_id -> ()
  | Tpoly (_, typ), _ -> check_type_aux typ ctyp env
  | _, Tpoly (_, ctyp) -> check_type_aux typ ctyp env
  | Tvar (_, depth, _), Tvar (_, constr_depth, _) ->
      bind_none
        (without_instance typ env ~f:(fun typ -> check_type_aux typ ctyp))
        (fun () ->
          bind_none
            (without_instance ctyp env ~f:(fun ctyp -> check_type_aux typ ctyp))
            (fun () ->
              (* Add the outermost (in terms of lexical scope) of the variables as
                 the instance for the other. If they are at the same level, prefer
                 the lowest ID to ensure strict ordering and thus no cycles. *)
              if
                constr_depth < depth
                || (Int.equal constr_depth depth && ctyp.type_id < typ.type_id)
              then Envi.Type.add_instance typ ctyp env
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
    | Ok () -> ()
    | Unequal_lengths ->
        raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp))) )
  | Tarrow (typ1, typ2, Explicit), Tarrow (ctyp1, ctyp2, Explicit)
   |Tarrow (typ1, typ2, Implicit), Tarrow (ctyp1, ctyp2, Implicit) ->
      check_type_aux typ1 ctyp1 env ;
      check_type_aux typ2 ctyp2 env
  | Tctor variant, Tctor constr_variant ->
      if Int.equal variant.var_decl_id constr_variant.var_decl_id then
        match
          List.iter2 variant.var_params constr_variant.var_params
            ~f:(fun param constr_param -> check_type_aux param constr_param env
          )
        with
        | Ok env -> env
        | Unequal_lengths ->
            raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp)))
      else
        let typ, ctyp =
          match unpack_decls typ ctyp env with
          | Some (typ, ctyp) -> (typ, ctyp)
          | None -> raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp)))
        in
        check_type_aux typ ctyp env
  | _, _ -> raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp)))

let check_type ~loc env typ constr_typ =
  match check_type_aux typ constr_typ env with
  | exception Error (_, err) ->
      let typ = Envi.Type.flatten typ env in
      let constr_typ = Envi.Type.flatten constr_typ env in
      raise (Error (loc, Check_failed (typ, constr_typ, err)))
  | () -> ()

let rec add_implicits ~loc implicits typ env =
  match implicits with
  | [] -> typ
  | typ' :: implicits ->
      let typ = add_implicits ~loc implicits typ env in
      Envi.Type.mk ~loc (Tarrow (typ', typ, Implicit)) env

let rec free_type_vars ?depth typ =
  let free_type_vars = free_type_vars ?depth in
  match typ.type_desc with
  | Tvar _ -> Set.empty (module Envi.Type)
  | Tpoly (vars, typ) ->
      let poly_vars =
        List.fold
          ~init:(Set.empty (module Envi.Type))
          vars
          ~f:(fun set var -> Set.union set (Envi.Type.type_vars var))
      in
      Set.diff (free_type_vars typ) poly_vars
  | Tctor {var_params; _} ->
      Set.union_list (module Envi.Type) (List.map ~f:free_type_vars var_params)
  | Ttuple typs ->
      Set.union_list (module Envi.Type) (List.map ~f:free_type_vars typs)
  | Tarrow (typ1, typ2, _) ->
      Set.union (Envi.Type.type_vars ?depth typ1) (free_type_vars typ2)

let polymorphise typ env =
  let loc = typ.type_loc in
  let typ_vars = Set.to_list (free_type_vars ~depth:env.Envi.depth typ) in
  match typ_vars with
  | [] -> typ
  | _ -> Envi.Type.mk ~loc (Tpoly (typ_vars, typ)) env

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
        Envi.Type.refresh_vars tdec_params (Map.empty (module Int)) env
      in
      let name =
        Location.mkloc
          ( match field.txt with
          | Longident.Ldot (m, _) -> Longident.Ldot (m, tdec_ident.txt)
          | _ -> Longident.Lident tdec_ident.txt )
          tdec_ident.loc
      in
      let rcd_type =
        Envi.TypeDecl.mk_typ ~loc ~params:vars ~ident:name decl env
      in
      let {fld_type; _} = List.nth_exn field_decls i in
      let rcd_type = Envi.Type.copy rcd_type bound_vars env in
      let fld_type = Envi.Type.copy fld_type bound_vars env in
      (i, fld_type, rcd_type)
  | _ -> raise (Error (loc, Unbound ("record field", field)))

let get_field_of_decl typ bound_vars field_decls (field : lid) env =
  match field with
  | {txt= Longident.Lident name; _} -> (
    match
      List.findi field_decls ~f:(fun _ {fld_ident; _} ->
          String.equal fld_ident.txt name )
    with
    | Some (i, {fld_type; _}) ->
        let typ = Envi.Type.copy typ bound_vars env in
        let fld_type = Envi.Type.copy fld_type bound_vars env in
        (i, fld_type, typ)
    | None -> get_field field env )
  | _ -> get_field field env

let get_ctor (name : lid) env =
  let loc = name.loc in
  match (Envi.TypeDecl.find_of_constructor name env, name) with
  | ( Some
        ( ( { tdec_desc= TVariant ctors
            ; tdec_ident
            ; tdec_params
            ; tdec_implicit_params; _ } as decl )
        , i )
    , name )
   |( Some
        ( { tdec_desc= TExtend (name, decl, ctors)
          ; tdec_ident
          ; tdec_params
          ; tdec_implicit_params; _ }
        , i )
    , _ ) ->
      let ctor = List.nth_exn ctors i in
      let make_name (tdec_ident : str) =
        Location.mkloc
          ( match name.txt with
          | Longident.Ldot (m, _) -> Longident.Ldot (m, tdec_ident.txt)
          | _ -> Longident.Lident tdec_ident.txt )
          tdec_ident.loc
      in
      let typ, params =
        match ctor.ctor_ret with
        | Some ({type_desc= Tctor {var_params; _}; _} as typ) ->
            (typ, var_params)
        | _ ->
            ( Envi.TypeDecl.mk_typ ~loc ~params:tdec_params
                ~ident:(make_name tdec_ident) decl env
            , tdec_params )
      in
      let args_typ =
        match ctor.ctor_args with
        | Ctor_record (tdec_id, _) ->
            Envi.Type.mk ~loc
              (Tctor
                 { var_ident= make_name ctor.ctor_ident
                 ; var_params= params
                 ; var_implicit_params= tdec_implicit_params
                 ; var_decl_id= tdec_id })
              env
        | Ctor_tuple [typ] -> typ
        | Ctor_tuple typs -> Envi.Type.mk ~loc (Ttuple typs) env
      in
      let _, bound_vars, _ =
        Envi.Type.refresh_vars params (Map.empty (module Int)) env
      in
      let args_typ = Envi.Type.copy args_typ bound_vars env in
      let typ = Envi.Type.copy typ bound_vars env in
      (typ, args_typ)
  | _ -> raise (Error (loc, Unbound ("constructor", name)))

let rec check_pattern ~add env typ pat =
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | PAny -> ({pat_loc= loc; pat_type= typ; pat_desc= PAny}, env)
  | PVariable str ->
      let env = add str typ env in
      ({pat_loc= loc; pat_type= typ; pat_desc= PVariable str}, env)
  | PConstraint (p, constr_typ) ->
      let constr_typ, env = Envi.Type.import constr_typ env in
      check_type ~loc env typ constr_typ ;
      let p, env = check_pattern ~add env constr_typ p in
      ( {pat_loc= loc; pat_type= typ; pat_desc= PConstraint (p, constr_typ)}
      , env )
  | PTuple ps ->
      let vars =
        List.map ps ~f:(fun {pat_loc= loc; _} -> Envi.Type.mkvar ~loc None env)
      in
      let tuple_typ = Envi.Type.mk ~loc (Ttuple vars) env in
      check_type ~loc env typ tuple_typ ;
      let ps, env = check_patterns ~add env vars ps in
      ({pat_loc= loc; pat_type= tuple_typ; pat_desc= PTuple ps}, env)
  | POr (p1, p2) ->
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
            | `Both (var1, var2) -> check_type ~loc env var1 var2
            | _ -> () )
          ~names:(fun ~key:name ~data () ->
            match data with
            | `Both (typ1, typ2) -> check_type ~loc env typ1 typ2
            | _ -> raise (Error (loc, Variable_on_one_side name)) )
          ~type_decls:(fun ~key:name ~data _ ->
            let loc =
              match data with
              | `Both (typ, _) | `Left typ | `Right typ -> typ.tdec_loc
            in
            raise (Error (loc, Pattern_declaration ("type", name))) )
          ~fields:(fun ~key:name ~data _ ->
            let loc =
              match data with
              | `Both ((typ, _), _) | `Left (typ, _) | `Right (typ, _) ->
                  typ.tdec_loc
            in
            raise (Error (loc, Pattern_declaration ("field", name))) )
          ~ctors:(fun ~key:name ~data _ ->
            let loc =
              match data with
              | `Both ((typ, _), _) | `Left (typ, _) | `Right (typ, _) ->
                  typ.tdec_loc
            in
            raise (Error (loc, Pattern_declaration ("constructor", name))) )
          ~modules:(fun ~key:name ~data:_ _ ->
            raise (Error (loc, Pattern_declaration ("module", name))) )
          ~instances:(fun ~key:_ ~data:_ () -> ())
      in
      let env = Envi.push_scope scope2 env in
      ({pat_loc= loc; pat_type= typ; pat_desc= POr (p1, p2)}, env)
  | PInt i ->
      check_type ~loc env typ Envi.Core.Type.int ;
      ({pat_loc= loc; pat_type= typ; pat_desc= PInt i}, env)
  | PRecord [] -> raise (Error (loc, Empty_record))
  | PRecord ((field, _) :: _ as fields) ->
      let typ, field_decls, bound_vars, env =
        match Envi.TypeDecl.find_unaliased_of_type typ env with
        | Some ({tdec_desc= TRecord field_decls; _}, bound_vars, env) ->
            (typ, field_decls, bound_vars, env)
        | _ -> (
          match Envi.TypeDecl.find_of_field field env with
          | Some (({tdec_desc= TRecord field_decls; tdec_params; _} as decl), _)
            ->
              let vars, bound_vars, env =
                Envi.Type.refresh_vars tdec_params (Map.empty (module Int)) env
              in
              let ident =
                Longident.(
                  match field.txt with
                  | Lident _ -> Location.mkloc (Lident decl.tdec_ident.txt) loc
                  | Ldot (path, _) ->
                      Location.mkloc (Ldot (path, decl.tdec_ident.txt)) loc
                  | _ -> failwith "Unhandled Lapply in field name")
              in
              let decl_type =
                Envi.TypeDecl.mk_typ ~loc ~params:vars ~ident decl env
              in
              check_type ~loc env typ decl_type ;
              (decl_type, field_decls, bound_vars, env)
          | _ -> raise (Error (loc, Unbound ("record field", field))) )
      in
      let ps =
        List.map fields ~f:(fun (field, p) ->
            let _, field_typ, record_typ =
              get_field_of_decl typ bound_vars field_decls field env
            in
            ( try check_type ~loc:field.loc env record_typ typ
              with Error (_, Check_failed (_, _, Cannot_unify (typ, _))) ->
                raise (Error (field.loc, Wrong_record_field (field.txt, typ)))
            ) ;
            let field_typ = {field_typ with type_loc= field.loc} in
            {p with pat_type= field_typ} )
      in
      let ps, env = check_patterns ~add env [] ps in
      let fields =
        List.map2_exn fields ps ~f:(fun (field, _) p -> (field, p))
      in
      ({pat_loc= loc; pat_type= typ; pat_desc= PRecord fields}, env)
  | PCtor (name, arg) ->
      let typ', args_typ = get_ctor name env in
      let typ' = {typ' with type_loc= loc} in
      check_type ~loc env typ typ' ;
      let arg, env =
        match arg with
        | Some arg ->
            let arg, env =
              check_pattern ~add env {args_typ with type_loc= arg.pat_loc} arg
            in
            (Some arg, env)
        | None ->
            let typ = Envi.Type.mk ~loc (Ttuple []) env in
            check_type ~loc env args_typ typ ;
            (None, env)
      in
      ({pat_loc= loc; pat_type= typ'; pat_desc= PCtor (name, arg)}, env)

and check_patterns ~add env typs pats =
  match pats with
  | [] -> ([], env)
  | pat :: pats ->
      let typ, typs =
        match typs with
        | [] -> (* Type is passed with the pattern! *) (pat.pat_type, [])
        | typ :: typs -> (typ, typs)
      in
      let pat, env = check_pattern ~add env typ pat in
      let pats, env = check_patterns ~add env typs pats in
      (pat :: pats, env)

let rec get_expression env expected exp =
  let loc = exp.exp_loc in
  match exp.exp_desc with
  | Apply (f, es) ->
      let f_typ = Envi.Type.mkvar ~loc None env in
      let f, env = get_expression env f_typ f in
      let (typ, env), es =
        List.fold_map ~init:(f.exp_type, env) es ~f:(fun (f_typ, env) e ->
            let e_typ = Envi.Type.mkvar ~loc None env in
            let res_typ = Envi.Type.mkvar ~loc None env in
            let arrow =
              Envi.Type.mk ~loc (Tarrow (e_typ, res_typ, Explicit)) env
            in
            check_type ~loc:e.exp_loc env f_typ arrow ;
            let e, env = get_expression env e_typ e in
            ((res_typ, env), e) )
      in
      check_type ~loc env expected typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Apply (f, es)}, env)
  | Variable name ->
      let typ = Envi.find_name name env in
      check_type ~loc env expected typ ;
      let e = {exp_loc= loc; exp_type= typ; exp_desc= Variable name} in
      (Envi.Type.generate_implicits e env, env)
  | Int i ->
      let typ = Envi.Core.Type.int in
      check_type ~loc env expected typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Int i}, env)
  | Fun (p, body, explicit) ->
      let env = Envi.open_expr_scope env in
      let p_typ = Envi.Type.mkvar ~loc:p.pat_loc None env in
      let body_typ = Envi.Type.mkvar ~loc:body.exp_loc None env in
      let typ = Envi.Type.mk ~loc (Tarrow (p_typ, body_typ, explicit)) env in
      check_type ~loc env expected typ ;
      (* In OCaml, function arguments can't be polymorphic, so each check refines
       them rather than instantiating the parameters. *)
      let p, env = check_pattern ~add:Envi.add_name env p_typ p in
      let body, env = get_expression env body_typ body in
      let env = Envi.close_expr_scope env in
      ({exp_loc= loc; exp_type= typ; exp_desc= Fun (p, body, explicit)}, env)
  | Seq (e1, e2) ->
      let e1, env = get_expression env Envi.Core.Type.unit e1 in
      let e2, env = get_expression env expected e2 in
      ({exp_loc= loc; exp_type= e2.exp_type; exp_desc= Seq (e1, e2)}, env)
  | Let (p, e1, e2) ->
      let env = Envi.open_expr_scope env in
      let p, e1, env = check_binding env p e1 in
      let e2, env = get_expression env expected e2 in
      let env = Envi.close_expr_scope env in
      ({exp_loc= loc; exp_type= e2.exp_type; exp_desc= Let (p, e1, e2)}, env)
  | Constraint (e, typ') ->
      let typ, env = Envi.Type.import typ' env in
      check_type ~loc env expected typ ;
      let e, env = get_expression env typ e in
      check_type ~loc env e.exp_type typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Constraint (e, typ')}, env)
  | Tuple es ->
      let typs =
        List.map es ~f:(fun e -> Envi.Type.mkvar ~loc:e.exp_loc None env)
      in
      let typ = Envi.Type.mk ~loc (Ttuple typs) env in
      check_type ~loc env expected typ ;
      let env = ref env in
      let es =
        List.map2_exn es typs ~f:(fun e expected ->
            let e, env' = get_expression !env expected e in
            env := env' ;
            e )
      in
      let typ =
        Envi.Type.mk ~loc
          (Ttuple (List.map es ~f:(fun {exp_type= t; _} -> t)))
          !env
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Tuple es}, !env)
  | Match (e, cases) ->
      let e_typ = Envi.Type.mkvar ~loc:e.exp_loc None env in
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
      ({exp_loc= loc; exp_type= expected; exp_desc= Match (e, cases)}, env)
  | Field (e, field) ->
      let field_info =
        match field.txt with
        | Lident _ -> None
        | Ldot (path, _) -> (
          match Envi.TypeDecl.find_of_field field env with
          | Some (({tdec_desc= TRecord field_decls; tdec_params; _} as decl), i)
            ->
              let vars, bound_vars, env =
                Envi.Type.refresh_vars tdec_params (Map.empty (module Int)) env
              in
              let ident =
                Location.mkloc (Longident.Ldot (path, decl.tdec_ident.txt)) loc
              in
              let decl_type =
                Envi.TypeDecl.mk_typ ~loc ~params:vars ~ident decl env
              in
              let {fld_type; _} = List.nth_exn field_decls i in
              let fld_type = Envi.Type.copy fld_type bound_vars env in
              check_type ~loc env expected fld_type ;
              Some (fld_type, decl_type, env)
          | _ -> None )
        | Lapply _ -> failwith "Unhandled Lapply in field name"
      in
      let typ, decl_type, env, resolved =
        match field_info with
        | Some (fld_type, decl_type, env) -> (fld_type, decl_type, env, true)
        | None ->
            let fld_type = expected in
            let decl_type = Envi.Type.mkvar ~loc None env in
            (fld_type, decl_type, env, false)
      in
      let e, env = get_expression env decl_type e in
      let typ, env =
        if resolved then (typ, env)
        else
          match Envi.TypeDecl.find_unaliased_of_type e.exp_type env with
          | Some ({tdec_desc= TRecord field_decls; _}, bound_vars, env) -> (
            match
              List.find field_decls ~f:(fun {fld_ident; _} ->
                  match field.txt with
                  | Lident field -> String.equal fld_ident.txt field
                  | _ -> false
                  (* This case shouldn't happen! *) )
            with
            | Some {fld_type; _} ->
                let fld_type = Envi.Type.copy fld_type bound_vars env in
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
                  Envi.Type.refresh_vars tdec_params
                    (Map.empty (module Int))
                    env
                in
                let ident =
                  Longident.(
                    match field.txt with
                    | Lident _ ->
                        Location.mkloc (Lident decl.tdec_ident.txt) loc
                    | Ldot (path, _) ->
                        Location.mkloc (Ldot (path, decl.tdec_ident.txt)) loc
                    | _ -> failwith "Unhandled Lapply in field name")
                in
                let e_typ =
                  Envi.TypeDecl.mk_typ ~loc ~params:vars ~ident decl env
                in
                check_type ~loc env e.exp_type e_typ ;
                let {fld_type; _} = List.nth_exn field_decls i in
                let fld_type = Envi.Type.copy fld_type bound_vars env in
                let fld_type = Envi.Type.copy fld_type bound_vars env in
                (fld_type, env)
            | _ -> raise (Error (loc, Unbound ("record field", field))) )
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Field (e, field)}, env)
  | Record ([], _) -> raise (Error (loc, Empty_record))
  | Record (((field, _) :: _ as fields), ext) ->
      let typ, ext, env =
        match ext with
        | Some ext ->
            let ext, env = get_expression env expected ext in
            (ext.exp_type, Some ext, env)
        | None -> (expected, None, env)
      in
      let typ, field_decls, bound_vars, env =
        match Envi.TypeDecl.find_unaliased_of_type typ env with
        | Some ({tdec_desc= TRecord field_decls; _}, bound_vars, env) ->
            (typ, field_decls, bound_vars, env)
        | _ -> (
          match Envi.TypeDecl.find_of_field field env with
          | Some (({tdec_desc= TRecord field_decls; tdec_params; _} as decl), _)
            ->
              let vars, bound_vars, env =
                Envi.Type.refresh_vars tdec_params (Map.empty (module Int)) env
              in
              let ident =
                Longident.(
                  match field.txt with
                  | Lident _ -> Location.mkloc (Lident decl.tdec_ident.txt) loc
                  | Ldot (path, _) ->
                      Location.mkloc (Ldot (path, decl.tdec_ident.txt)) loc
                  | _ -> failwith "Unhandled Lapply in field name")
              in
              let decl_type =
                Envi.TypeDecl.mk_typ ~loc ~params:vars ~ident decl env
              in
              check_type ~loc env typ decl_type ;
              (decl_type, field_decls, bound_vars, env)
          | _ -> raise (Error (loc, Unbound ("record field", field))) )
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
      | Some _ -> () (* TODO: warn when all fields have been provided. *)
      | None ->
          let fields_filled = Array.to_list fields_filled in
          let names =
            List.fold2_exn ~init:[] fields_filled field_decls
              ~f:(fun names filled {fld_ident; _} ->
                if filled then names else fld_ident.txt :: names )
          in
          if not (List.is_empty names) then
            raise (Error (loc, Missing_fields names)) ) ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Record (fields, ext)}, !env)
  | Ctor (name, arg) ->
      let typ, arg_typ = get_ctor name env in
      check_type ~loc env expected typ ;
      let arg, env =
        match arg with
        | Some arg ->
            let arg, env = get_expression env arg_typ arg in
            (Some arg, env)
        | None ->
            let typ = Envi.Type.mk ~loc (Ttuple []) env in
            ( try check_type ~loc env arg_typ typ with _ ->
                raise (Error (loc, Argument_expected name.txt)) ) ;
            (None, env)
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Ctor (name, arg)}, env)
  | Unifiable _ -> raise (Error (loc, Unifiable_expr))

and check_binding ?(toplevel = false) (env : Envi.t) p e : 's =
  let typ = Envi.Type.mkvar ~loc:e.exp_loc None env in
  let env = Envi.open_expr_scope env in
  let e, env = get_expression env typ e in
  let env = Envi.close_expr_scope env in
  let exp_type = Envi.Type.flatten e.exp_type env in
  let e = {e with exp_type} in
  let typ_vars = free_type_vars ~depth:env.Envi.depth exp_type in
  let implicit_vars =
    Envi.Type.flattened_implicit_vars ~toplevel
      ~unify:(check_type ~loc:e.exp_loc)
      typ_vars env
  in
  let loc = e.exp_loc in
  let e, env =
    List.fold ~init:(e, env) implicit_vars ~f:(fun (e, env) var ->
        match var.exp_desc with
        | Unifiable {expression= None; name; _} ->
            let exp_type =
              Envi.Type.mk ~loc
                (Tarrow (var.exp_type, e.exp_type, Implicit))
                env
            in
            let p =
              {pat_desc= PVariable name; pat_loc= loc; pat_type= var.exp_type}
            in
            ({exp_desc= Fun (p, e, Implicit); exp_type; exp_loc= loc}, env)
        | _ -> raise (Error (var.exp_loc, No_unifiable_expr)) )
  in
  let loc = p.pat_loc in
  match (p.pat_desc, implicit_vars) with
  | PVariable str, _ ->
      let typ =
        if Set.is_empty typ_vars then e.exp_type
        else Envi.Type.mk ~loc (Tpoly (Set.to_list typ_vars, e.exp_type)) env
      in
      let env = Envi.add_name str typ env in
      let p = {p with pat_type= typ} in
      (p, e, env)
  | _, [] ->
      let p, env = check_pattern ~add:add_polymorphised env e.exp_type p in
      (p, e, env)
  | _, implicit :: _ ->
      raise (Error (e.exp_loc, No_instance implicit.exp_type))

let rec check_signature_item env item =
  match item.sig_desc with
  | SValue (name, typ) ->
      let typ, env = Envi.Type.import ~must_find:false typ env in
      add_polymorphised name typ env
  | SInstance (name, typ) ->
      let typ, env = Envi.Type.import ~must_find:false typ env in
      let env = add_polymorphised name typ env in
      Envi.add_implicit_instance name.txt typ env
  | STypeDecl decl ->
      let _decl, env = Envi.TypeDecl.import decl env in
      env
  | SModule (name, msig) -> (
      let m, env = check_module_sig env msig in
      match m with
      | Envi.Scope.Immediate m -> Envi.add_module name m env
      | Envi.Scope.Deferred path -> Envi.add_deferred_module name path env )
  | SModType (_name, _signature) -> env

and check_signature env signature =
  List.fold ~init:env signature ~f:check_signature_item

and check_module_sig env msig =
  let loc = msig.msig_loc in
  match msig.msig_desc with
  | Signature signature ->
      let env = Envi.open_module env in
      let env = check_signature env signature in
      let m, env = Envi.pop_module ~loc env in
      (Envi.Scope.Immediate m, env)
  | SigName lid -> (
    match Envi.find_module_deferred ~loc lid env with
    | Some m -> (m, env)
    | None -> (Envi.Scope.Deferred lid.txt, env) )
  | SigAbstract ->
      let env = Envi.open_module env in
      let m, env = Envi.pop_module ~loc env in
      (Envi.Scope.Immediate m, env)
  | SigFunctor (name, f, msig) ->
      let f, env = check_module_sig env f in
      (* Set up functor module *)
      let env = Envi.open_module env in
      let env =
        match f with
        | Envi.Scope.Immediate f -> Envi.add_module name f env
        | Envi.Scope.Deferred path -> Envi.add_deferred_module name path env
      in
      let f, env = Envi.pop_module ~loc env in
      let ftor f_instance =
        let env = Envi.open_module env in
        let env = Envi.open_namespace_scope f_instance env in
        (* TODO: check that f_instance matches f' *)
        let m, _env = check_module_sig env msig in
        match m with
        | Envi.Scope.Immediate m -> m
        | Envi.Scope.Deferred path ->
            Envi.find_module ~loc (Location.mkloc path loc) env
      in
      (* Check that f builds the functor as expected. *)
      ignore (ftor f) ;
      let m = Envi.make_functor ftor in
      (Envi.Scope.Immediate m, env)

let rec check_statement env stmt =
  let loc = stmt.stmt_loc in
  match stmt.stmt_desc with
  | Value (p, e) ->
      let p, e, env = check_binding ~toplevel:true env p e in
      (env, {stmt with stmt_desc= Value (p, e)})
  | Instance (name, e) ->
      let dummy_type =
        {type_desc= Tvar (None, -1, Explicit); type_id= -1; type_loc= loc}
      in
      let p =
        {pat_desc= PVariable name; pat_loc= name.loc; pat_type= dummy_type}
      in
      let _, e, env = check_binding ~toplevel:true env p e in
      let env = Envi.add_implicit_instance name.txt e.exp_type env in
      (env, {stmt with stmt_desc= Instance (name, e)})
  | TypeDecl decl ->
      let decl, env = Envi.TypeDecl.import decl env in
      (env, {stmt with stmt_desc= TypeDecl decl})
  | Module (name, m) ->
      let env = Envi.open_module env in
      let env, m = check_module_expr env m in
      let m_env, env = Envi.pop_module ~loc env in
      let env = Envi.add_module name m_env env in
      (env, {stmt with stmt_desc= Module (name, m)})
  | Open name ->
      let m = Envi.find_module ~loc name env in
      (Envi.open_namespace_scope m env, stmt)
  | TypeExtension (variant, ctors) ->
      let ( { tdec_ident
            ; tdec_params
            ; tdec_implicit_params
            ; tdec_desc
            ; tdec_id; _ } as decl ) =
        match Envi.raw_find_type_declaration variant.var_ident env with
        | open_decl -> open_decl
        | exception _ ->
            raise
              (Error (loc, Unbound ("type constructor", variant.var_ident)))
      in
      ( match tdec_desc with
      | TOpen -> ()
      | _ -> raise (Error (loc, Not_extensible variant.var_ident.txt)) ) ;
      ( match List.iter2 tdec_params variant.var_params ~f:(fun _ _ -> ()) with
      | Ok _ -> ()
      | Unequal_lengths ->
          raise (Error (loc, Extension_different_arity variant.var_ident.txt))
      ) ;
      let decl =
        { tdec_ident
        ; tdec_params= variant.var_params
        ; tdec_implicit_params
        ; tdec_id
        ; tdec_desc= TExtend (variant.var_ident, decl, ctors)
        ; tdec_loc= loc }
      in
      let decl, env = Envi.TypeDecl.import decl env in
      let ctors =
        match decl.tdec_desc with
        | TExtend (_, _, ctors) -> ctors
        | _ -> failwith "Expected a TExtend."
      in
      let variant =
        {variant with var_decl_id= tdec_id; var_params= decl.tdec_params}
      in
      (env, {stmt with stmt_desc= TypeExtension (variant, ctors)})
  | Request _ -> (env, stmt)

and check_module_expr env m =
  let loc = m.mod_loc in
  match m.mod_desc with
  | Structure stmts ->
      let env, stmts = List.fold_map ~f:check_statement ~init:env stmts in
      (env, {m with mod_desc= Structure stmts})
  | ModName name ->
      let m' = Envi.find_module ~loc name env in
      let env = Envi.push_scope m' env in
      (env, m)
  | Functor (name, f, m) ->
      let f', env = check_module_sig env f in
      (* Set up functor module *)
      let env = Envi.open_module env in
      let env =
        match f' with
        | Envi.Scope.Immediate f' -> Envi.add_module name f' env
        | Envi.Scope.Deferred path -> Envi.add_deferred_module name path env
      in
      let f', env = Envi.pop_module ~loc env in
      let ftor f_instance =
        let env = Envi.open_module env in
        let env = Envi.open_namespace_scope f_instance env in
        (* TODO: check that f_instance matches f' *)
        let env, m' = check_module_expr env m in
        let m, _env = Envi.pop_module ~loc env in
        (m, m')
      in
      (* Check that f builds the functor as expected. *)
      let _, m = ftor f' in
      let env =
        Envi.push_scope (Envi.make_functor (fun f -> fst (ftor f))) env
      in
      (env, {m with mod_desc= Functor (name, f, m)})

let check (ast : statement list) (env : Envi.t) =
  List.fold_map ast ~init:env ~f:check_statement

(* Error handling *)

open Format

let pp_typ ppf typ = Pprintast.core_type ppf (To_ocaml.of_type_expr typ)

let rec report_error ppf = function
  | Check_failed (typ, constr_typ, err) ->
      fprintf ppf "Incompatable types @['%a'@] and @['%a'@]:@.%a" pp_typ typ
        pp_typ constr_typ report_error err
  | Cannot_unify (typ, constr_typ) ->
      fprintf ppf "Cannot unify @['%a'@] and @['%a'@].@." pp_typ typ pp_typ
        constr_typ
  | Recursive_variable typ ->
      fprintf ppf
        "The variable @[%a@](%d) would have an instance that contains itself."
        pp_typ typ typ.type_id
  | Unbound (kind, value) ->
      fprintf ppf "Unbound %s %a." kind Longident.pp value.txt
  | Unbound_value value -> fprintf ppf "Unbound value %s." value.txt
  | Variable_on_one_side name ->
      fprintf ppf "Variable %s must occur on both sides of this '|' pattern."
        name
  | Pattern_declaration (kind, name) ->
      fprintf ppf "Unexpected %s declaration for %s within a pattern." kind
        name
  | Empty_record -> fprintf ppf "Unexpected empty record."
  | Wrong_record_field (field, typ) ->
      fprintf ppf
        "This record expression is expected to have type %a@.The field %a \
         does not belong to type %a."
        pp_typ typ Longident.pp field pp_typ typ
  | Repeated_field field ->
      fprintf ppf "The record field %s is defined several times." field
  | Missing_fields fields ->
      fprintf ppf "Some record fields are undefined: %a"
        (pp_print_list pp_print_string)
        fields
  | Wrong_type_description (kind, name) ->
      fprintf ppf
        "Internal error: Expected a type declaration of kind %s, but instead \
         got %s"
        kind name.txt
  | Unifiable_expr ->
      fprintf ppf "Internal error: Unexpected implicit variable."
  | No_unifiable_expr ->
      fprintf ppf "Internal error: Expected an unresolved implicit variable."
  | No_instance typ ->
      fprintf ppf
        "Could not find an instance for an implicit variable of type @[%a@]."
        pp_typ typ
  | Argument_expected lid ->
      fprintf ppf "@[The constructor %a expects an argument.@]" Longident.pp
        lid
  | Not_extensible lid ->
      fprintf ppf "@[Type definition %a is not extensible.@]" Longident.pp lid
  | Extension_different_arity lid ->
      fprintf ppf
        "@[This extension does not match the definition of type %a@.They have \
         different arities.@]"
        Longident.pp lid

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None )
