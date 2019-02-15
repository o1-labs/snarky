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
  | Wrong_type_description of string * str

exception Error of Location.t * error

let bind_none x f = match x with Some x -> x | None -> f ()

let unpack_decls typ ctyp env =
  match (typ.type_desc, ctyp.type_desc) with
  | Tctor variant, Tctor cvariant ->
      let decl_id, cdecl_id = (variant.var_decl_id, cvariant.var_decl_id) in
      let unfold_typ () =
        Option.map (Envi.TypeDecl.unfold_alias typ env) ~f:(fun (typ, env) ->
            (typ, ctyp, env) )
      in
      let unfold_ctyp () =
        Option.map (Envi.TypeDecl.unfold_alias ctyp env) ~f:(fun (ctyp, env) ->
            (typ, ctyp, env) )
      in
      (* Try to unfold the oldest type definition first. *)
      if decl_id < cdecl_id then bind_none (Some (unfold_ctyp ())) unfold_typ
      else bind_none (Some (unfold_typ ())) unfold_ctyp
  | _ -> None

let rec check_type_aux typ ctyp env =
  let without_instance ~f (typ : type_expr) env =
    match Envi.Type.instance env typ with
    | Some typ' -> (
        let env = Envi.Type.clear_instance typ env in
        let env = f typ' env in
        match Envi.Type.instance env typ with
        | Some _ -> raise (Error (typ.type_loc, Recursive_variable typ))
        | None -> Some (Envi.Type.add_instance typ typ' env) )
    | None -> None
  in
  match (typ.type_desc, ctyp.type_desc) with
  | _, _ when Int.equal typ.type_id ctyp.type_id -> env
  | Tpoly (_, typ), _ -> check_type_aux typ ctyp env
  | _, Tpoly (_, ctyp) -> check_type_aux typ ctyp env
  | Tvar (_, depth), Tvar (_, constr_depth) ->
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
      List.fold2 ~init:env typs ctyps ~f:(fun env typ ctyp ->
          check_type_aux typ ctyp env )
    with
    | Ok env -> env
    | Unequal_lengths ->
        raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp))) )
  | Tarrow (typ1, typ2), Tarrow (ctyp1, ctyp2) ->
      env |> check_type_aux typ1 ctyp1 |> check_type_aux typ2 ctyp2
  | Tctor variant, Tctor constr_variant ->
      if Int.equal variant.var_decl_id constr_variant.var_decl_id then
        match
          List.fold2 ~init:env variant.var_params constr_variant.var_params
            ~f:(fun env param constr_param ->
              check_type_aux param constr_param env )
        with
        | Ok env -> env
        | Unequal_lengths ->
            raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp)))
      else
        let typ, ctyp, env =
          match unpack_decls typ ctyp env with
          | Some (typ, ctyp, env) -> (typ, ctyp, env)
          | None -> raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp)))
        in
        check_type_aux typ ctyp env
  | _, _ -> raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp)))

let check_type env typ constr_typ =
  match check_type_aux typ constr_typ env with
  | exception Error (_, err) ->
      raise (Error (constr_typ.type_loc, Check_failed (typ, constr_typ, err)))
  | env -> env

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
  | Tctor _ -> Set.empty (module Envi.Type)
  | Ttuple typs ->
      Set.union_list (module Envi.Type) (List.map ~f:free_type_vars typs)
  | Tarrow (typ1, typ2) ->
      Set.union (Envi.Type.type_vars ?depth typ1) (free_type_vars typ2)

let polymorphise typ env =
  let loc = typ.type_loc in
  let typ_vars = Set.to_list (free_type_vars ~depth:env.Envi.depth typ) in
  match typ.type_desc with
  | Tpoly (vars, typ) -> Envi.Type.mk ~loc (Tpoly (typ_vars @ vars, typ)) env
  | _ -> Envi.Type.mk ~loc (Tpoly (typ_vars, typ)) env

let add_polymorphised name typ env =
  let typ, env = Envi.Type.flatten typ env in
  let typ, env = polymorphise typ env in
  Envi.add_name name typ env

let get_field (field : lid) env =
  let loc = field.loc in
  match Envi.TypeDecl.find_of_field field env with
  | Some
      ( ({tdec_desc= TRecord field_decls; tdec_ident; tdec_params; _} as decl)
      , i ) ->
      let vars, bound_vars, env =
        Envi.Type.refresh_vars tdec_params (Map.empty (module Int)) env
      in
      let name =
        Location.mkloc
          ( match field.txt with
          | Longident.Ldot (m, _) -> Longident.Ldot (m, tdec_ident.txt)
          | _ -> Longident.Lident tdec_ident.txt )
          tdec_ident.loc
      in
      let rcd_type, env =
        Envi.TypeDecl.mk_typ ~loc ~params:vars ~ident:name decl env
      in
      let {fld_type; _} = List.nth_exn field_decls i in
      let typ, env = Envi.Type.mk ~loc (Tarrow (rcd_type, fld_type)) env in
      Envi.Type.copy typ bound_vars env
  | _ -> raise (Error (loc, Unbound ("record field", field)))

let get_field_of_decl typ bound_vars field_decls (field : lid) env =
  match field with
  | {txt= Longident.Lident name; loc} -> (
    match
      List.find field_decls ~f:(fun {fld_ident; _} ->
          String.equal fld_ident.txt name )
    with
    | Some {fld_type; _} ->
        let typ, env = Envi.Type.mk ~loc (Tarrow (typ, fld_type)) env in
        Envi.Type.copy typ bound_vars env
    | None -> get_field field env )
  | _ -> get_field field env

let get_ctor (name : lid) env =
  let loc = name.loc in
  match Envi.TypeDecl.find_of_constructor name env with
  | Some (({tdec_desc= TVariant ctors; tdec_ident; tdec_params; _} as decl), i)
    ->
      let ctor = List.nth_exn ctors i in
      let make_name (tdec_ident : str) =
        Location.mkloc
          ( match name.txt with
          | Longident.Ldot (m, _) -> Longident.Ldot (m, tdec_ident.txt)
          | _ -> Longident.Lident tdec_ident.txt )
          tdec_ident.loc
      in
      let (typ, env), params =
        match ctor.ctor_ret with
        | Some ({type_desc= Tctor {var_params; _}; _} as typ) ->
            ((typ, env), var_params)
        | _ ->
            ( Envi.TypeDecl.mk_typ ~loc ~params:tdec_params
                ~ident:(make_name tdec_ident) decl env
            , tdec_params )
      in
      let args_typ, env =
        match ctor.ctor_args with
        | Ctor_record (tdec_id, _) ->
            Envi.Type.mk ~loc
              (Tctor
                 { var_ident= make_name ctor.ctor_ident
                 ; var_params= params
                 ; var_decl_id= tdec_id })
              env
        | Ctor_tuple [typ] -> (typ, env)
        | Ctor_tuple typs -> Envi.Type.mk ~loc (Ttuple typs) env
      in
      let typ, env = Envi.Type.mk ~loc (Tarrow (args_typ, typ)) env in
      Envi.Type.copy typ (Map.empty (module Int)) env
  | _ -> raise (Error (loc, Unbound ("constructor", name)))

let rec check_pattern_desc ~loc ~add env typ = function
  | PAny -> env
  | PVariable str -> add str typ env
  | PConstraint (p, constr_typ) ->
      let constr_typ, env = Envi.Type.import constr_typ env in
      let env = check_type env typ constr_typ in
      check_pattern ~add env constr_typ p
  | PTuple ps ->
      let vars, env =
        List.fold ~init:([], env) ps ~f:(fun (vars, env) _ ->
            let var, env = Envi.Type.mkvar ~loc None env in
            (var :: vars, env) )
      in
      let tuple_typ, env = Envi.Type.mk ~loc (Ttuple vars) env in
      let env = check_type env typ tuple_typ in
      List.fold2_exn ~init:env vars ps ~f:(check_pattern ~add)
  | POr (p1, p2) ->
      let env = Envi.open_scope env in
      let env = check_pattern ~add env typ p1 in
      let scope1, env = Envi.pop_scope env in
      let env = Envi.open_scope env in
      let env = check_pattern ~add env typ p2 in
      let scope2, env = Envi.pop_scope env in
      (* Check that the assignments in each scope match. *)
      let env =
        Envi.Scope.fold_over ~init:env scope1 scope2
          ~type_variables:(fun ~key:_ ~data env ->
            match data with
            | `Both (var1, var2) -> check_type env var1 var2
            | _ -> env )
          ~names:(fun ~key:name ~data env ->
            match data with
            | `Both (typ1, typ2) -> check_type env typ1 typ2
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
      in
      Envi.push_scope scope2 env
  | PInt _ -> check_type env typ Envi.Core.Type.int
  | PRecord fields ->
      let field_decls, env =
        match Envi.TypeDecl.find_unaliased_of_type typ env with
        | Some ({tdec_desc= TRecord field_decls; _}, bound_vars, env) ->
            (Some (field_decls, bound_vars), env)
        | _ -> (None, env)
      in
      List.fold ~init:env fields ~f:(fun env (field, p) ->
          let loc = {field.loc with Location.loc_end= p.pat_loc.loc_end} in
          let typ', env = Envi.Type.mkvar ~loc None env in
          let arrow_type, env = Envi.Type.mk ~loc (Tarrow (typ, typ')) env in
          let field_typ, env =
            match field_decls with
            | Some (field_decls, bound_vars) ->
                get_field_of_decl typ bound_vars field_decls field env
            | None -> get_field field env
          in
          let field_typ = {field_typ with type_loc= field.loc} in
          let env = check_type env arrow_type field_typ in
          check_pattern ~add env typ' p )
  | PCtor (name, arg) -> (
      let arg_typ, env =
        if Option.is_some arg then Envi.Type.mkvar ~loc None env
        else Envi.Type.mk ~loc (Ttuple []) env
      in
      let arrow_typ, env = Envi.Type.mk ~loc (Tarrow (arg_typ, typ)) env in
      let ctor_typ, env = get_ctor name env in
      let ctor_typ = {ctor_typ with type_loc= name.loc} in
      let env = check_type env arrow_typ ctor_typ in
      match arg with
      | Some arg -> check_pattern ~add env arg_typ arg
      | None -> env )

and check_pattern ~add env typ pat =
  check_pattern_desc ~loc:pat.pat_loc ~add env typ pat.pat_desc

let rec get_expression env exp =
  let loc = exp.exp_loc in
  match exp.exp_desc with
  | Apply (f, es) ->
      let f, env = get_expression env f in
      let (typ, env), es =
        List.fold_map ~init:(f.exp_type, env) es ~f:(fun (f_typ, env) e ->
            let e, env = get_expression env e in
            let retvar, env = Envi.Type.mkvar ~loc None env in
            let arrow, env =
              Envi.Type.mk ~loc (Tarrow (e.exp_type, retvar)) env
            in
            let env = check_type env f_typ arrow in
            ((retvar, env), e) )
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Apply (f, es)}, env)
  | Variable name ->
      let typ, env = Envi.find_name name env in
      ({exp_loc= loc; exp_type= typ; exp_desc= Variable name}, env)
  | Int i ->
      let typ = Envi.Core.Type.int in
      ({exp_loc= loc; exp_type= typ; exp_desc= Int i}, env)
  | Fun (p, body) ->
      let env = Envi.open_scope env in
      let p_typ, env = Envi.Type.mkvar ~loc None env in
      (* In OCaml, function arguments can't be polymorphic, so each check refines
       them rather than instantiating the parameters. *)
      let env = check_pattern ~add:Envi.add_name env p_typ p in
      let body, env = get_expression env body in
      let env = Envi.close_scope env in
      let typ, env = Envi.Type.mk ~loc (Tarrow (p_typ, body.exp_type)) env in
      ({exp_loc= loc; exp_type= typ; exp_desc= Fun (p, body)}, env)
  | Seq (e1, e2) ->
      let e1, env = get_expression env e1 in
      let e2, env = get_expression env e2 in
      ({exp_loc= loc; exp_type= e2.exp_type; exp_desc= Seq (e1, e2)}, env)
  | Let (p, e1, e2) ->
      let env = Envi.open_scope env in
      let p, e1, env = check_binding env p e1 in
      let e2, env = get_expression env e2 in
      let env = Envi.close_scope env in
      ({exp_loc= loc; exp_type= e2.exp_type; exp_desc= Let (p, e1, e2)}, env)
  | Constraint (e, typ') ->
      let typ, env = Envi.Type.import typ' env in
      let e, env = get_expression env e in
      let env = check_type env e.exp_type typ in
      ({exp_loc= loc; exp_type= typ; exp_desc= Constraint (e, typ')}, env)
  | Tuple es ->
      let env, es =
        List.fold_map ~init:env es ~f:(fun env e ->
            let e, env = get_expression env e in
            (env, e) )
      in
      let typ, env =
        Envi.Type.mk ~loc
          (Ttuple (List.map es ~f:(fun {exp_type= t; _} -> t)))
          env
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Tuple es}, env)
  | Match (e, cases) ->
      let e, env = get_expression env e in
      let typ = e.exp_type in
      let result_typ, env = Envi.Type.mkvar ~loc None env in
      let env, cases =
        List.fold_map ~init:env cases ~f:(fun env (p, e) ->
            let env = Envi.open_scope env in
            let env = check_pattern ~add:add_polymorphised env typ p in
            let e, env = get_expression env e in
            let env = Envi.close_scope env in
            (check_type env e.exp_type result_typ, (p, e)) )
      in
      ({exp_loc= loc; exp_type= result_typ; exp_desc= Match (e, cases)}, env)
  | Record ([], _) -> raise (Error (loc, Empty_record))
  | Record (fields, ext) ->
      let typ, ext, env =
        match ext with
        | Some ext -> 
          let ext, env = get_expression env ext in
          (ext.exp_type, Some ext, env)
        | None ->
          let typ, env = Envi.Type.mkvar ~loc None env in
          (typ, None, env)
      in
      let field_decls, env =
        match Envi.TypeDecl.find_unaliased_of_type typ env with
        | Some ({tdec_desc= TRecord field_decls; _}, bound_vars, env) ->
            (Some (field_decls, bound_vars), env)
        | _ -> (None, env)
      in
      let env, fields =
        List.fold_map ~init:env fields ~f:(fun env (field, e) ->
            let loc = {field.loc with Location.loc_end= e.exp_loc.loc_end} in
            let e, env = get_expression env e in
            let arrow_type, env = Envi.Type.mk ~loc (Tarrow (typ, e.exp_type)) env in
            let field_typ, env =
              match field_decls with
              | Some (field_decls, bound_vars) ->
                  get_field_of_decl typ bound_vars field_decls field env
              | None -> get_field field env
            in
            (check_type env field_typ arrow_type, (field, e)) )
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Record (fields, ext)}, env)
  | Ctor (name, arg) ->
      let arg_typ, arg, env =
        match arg with
        | Some arg -> 
          let arg, env = get_expression env arg in
          (arg.exp_type, Some arg, env)
        | None ->
          let typ, env = Envi.Type.mk ~loc (Ttuple []) env in
          (typ, None, env)
      in
      let typ, env = Envi.Type.mkvar ~loc None env in
      let arrow_typ, env = Envi.Type.mk ~loc (Tarrow (arg_typ, typ)) env in
      let ctor_typ, env = get_ctor name env in
      let env = check_type env arrow_typ ctor_typ in
      ({exp_loc= loc; exp_type= typ; exp_desc= Ctor (name, arg)}, env)

and check_binding (env : Envi.t) p e : 's =
  let e, env = get_expression env e in
  let env = check_pattern ~add:add_polymorphised env e.exp_type p in
  (p, e, env)

let rec check_statement env stmt =
  match stmt.stmt_desc with
  | Value (p, e) ->
      let p, e, env = check_binding env p e in
      (env, {stmt with stmt_desc= Value (p, e)})
  | TypeDecl decl ->
      let decl, env = Envi.TypeDecl.import decl env in
      (env, {stmt with stmt_desc= TypeDecl decl})
  | Module (name, m) ->
      let env = Envi.open_scope env in
      let env, m = check_module_expr env m in
      let m_env, env = Envi.pop_scope env in
      let env = Envi.add_module name m_env env in
      (env, {stmt with stmt_desc= Module (name, m)})

and check_module_expr env m =
  let loc = m.mod_loc in
  match m.mod_desc with
  | Structure stmts ->
      let env, stmts = List.fold_map ~f:check_statement ~init:env stmts in
      (env, {m with mod_desc= Structure stmts})
  | ModName name ->
      let env = Envi.push_scope (Envi.find_module ~loc name env) env in
      (env, m)

let check (ast : statement list) =
  List.fold_map ast ~init:Envi.Core.env ~f:check_statement

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
  | Wrong_type_description (kind, name) ->
      fprintf ppf
        "Internal error: Expected a type declaration of kind %s, but instead \
         got %s"
        kind name.txt

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None )
