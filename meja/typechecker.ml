open Core_kernel
open Parsetypes

type error =
  | Check_failed of type_expr * type_expr * error
  | Cannot_unify of type_expr * type_expr
  | Recursive_variable of type_expr
  | Unbound_value of str
  | Variable_on_one_side of string
  | Pattern_type_declaration of string
  | Pattern_field_declaration of string
  | Pattern_module_declaration of string

exception Error of Location.t * error

let rec check_type_aux typ ctyp env =
  let bind_none x f = match x with Some x -> x | None -> f () in
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
      else raise (Error (ctyp.type_loc, Cannot_unify (typ, ctyp)))
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
            raise (Error (loc, Pattern_type_declaration name)) )
          ~fields:(fun ~key:name ~data _ ->
            let loc =
              match data with
              | `Both ((typ, _), _) | `Left (typ, _) | `Right (typ, _) ->
                  typ.tdec_loc
            in
            raise (Error (loc, Pattern_field_declaration name)) )
          ~modules:(fun ~key:name ~data:_ _ ->
            raise (Error (loc, Pattern_module_declaration name)) )
      in
      Envi.push_scope scope2 env
  | PInt _ -> check_type env typ Envi.Core.Type.int

and check_pattern ~add env typ pat =
  check_pattern_desc ~loc:pat.pat_loc ~add env typ pat.pat_desc

let rec get_expression_desc ~loc env = function
  | Apply (f, xs) ->
      let f_typ, env = get_expression env f in
      let rec apply_typ xs f_typ env =
        match xs with
        | [] -> (f_typ, env)
        | x :: xs ->
            let x_typ, env = get_expression env x in
            let retvar, env = Envi.Type.mkvar ~loc None env in
            let arrow, env = Envi.Type.mk ~loc (Tarrow (x_typ, retvar)) env in
            let env = check_type env f_typ arrow in
            apply_typ xs retvar env
      in
      apply_typ xs f_typ env
  | Variable name -> Envi.find_name name env
  | Int _ -> (Envi.Core.Type.int, env)
  | Fun (p, body) ->
      let env = Envi.open_scope env in
      let p_typ, env = Envi.Type.mkvar ~loc None env in
      (* In OCaml, function arguments can't be polymorphic, so each check refines
       them rather than instantiating the parameters. *)
      let env = check_pattern ~add:Envi.add_name env p_typ p in
      let body_typ, env = get_expression env body in
      let env = Envi.close_scope env in
      Envi.Type.mk ~loc (Tarrow (p_typ, body_typ)) env
  | Seq (e1, e2) ->
      let _, env = get_expression env e1 in
      get_expression env e2
  | Let (p, e1, e2) ->
      let env = Envi.open_scope env in
      let env = check_binding env p e1 in
      let typ, env = get_expression env e2 in
      (typ, Envi.close_scope env)
  | Constraint (e, typ) ->
      let typ, env = Envi.Type.import typ env in
      let e_typ, env = get_expression env e in
      (typ, check_type env e_typ typ)
  | Tuple es ->
      let env, typs =
        List.fold_map ~init:env es ~f:(fun env e ->
            let typ, env = get_expression env e in
            (env, typ) )
      in
      Envi.Type.mk ~loc (Ttuple typs) env

and get_expression env exp =
  get_expression_desc ~loc:exp.exp_loc env exp.exp_desc

and check_binding (env : Envi.t) p e : 's =
  let e_type, env = get_expression env e in
  check_pattern ~add:add_polymorphised env e_type p

let rec check_statement_desc ~loc:_ env = function
  | Value (p, e) -> check_binding env p e
  | TypeDecl decl ->
      let _, env = Envi.TypeDecl.import decl env in
      env
  | Module (name, m) ->
      let env = Envi.open_scope env in
      let env = check_module_expr env m in
      let m, env = Envi.pop_scope env in
      Envi.add_module name m env

and check_statement env stmt =
  check_statement_desc ~loc:stmt.stmt_loc env stmt.stmt_desc

and check_module_desc ~loc env = function
  | Structure stmts -> List.fold ~f:check_statement ~init:env stmts
  | ModName name -> Envi.push_scope (Envi.find_module ~loc name env) env

and check_module_expr env m = check_module_desc ~loc:m.mod_loc env m.mod_desc

let check (ast : statement list) =
  List.fold_left ast ~init:Envi.Core.env ~f:check_statement

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
        "The variable @['%a@](%d) would have an instance that contains itself."
        pp_typ typ typ.type_id
  | Unbound_value value -> fprintf ppf "Unbound value %s." value.txt
  | Variable_on_one_side name ->
      fprintf ppf "Variable %s must occur on both sides of this '|' pattern."
        name
  | Pattern_type_declaration name ->
      fprintf ppf "Unexpected type declaration for %s within a pattern." name
  | Pattern_field_declaration name ->
      fprintf ppf "Unexpected field declaration for %s within a pattern." name
  | Pattern_module_declaration name ->
      fprintf ppf "Unexpected module declaration for %s within a pattern." name

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None )
