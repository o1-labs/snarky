open Core_kernel
open Parsetypes

let rec check_type_aux typ ctyp env =
  let bind_none x f = match x with Some x -> x | None -> f () in
  let without_instance ~f (typ : type_expr) env =
    match Envi.Type.instance env typ with
    | Some typ' -> (
        let env = Envi.Type.clear_instance typ env in
        let env = f typ' env in
        match Envi.Type.instance env typ with
        | Some _ ->
            failwith
              "Found a type variable that unifies with part of its own instance"
        | None -> Some (Envi.Type.add_instance typ typ' env) )
    | None -> None
  in
  match (typ.type_desc, ctyp.type_desc) with
  | _, _ when Int.equal typ.type_id ctyp.type_id -> env
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
  | Tvar _, _ -> (
    match without_instance typ env ~f:(fun typ -> check_type_aux typ ctyp) with
    | Some env -> env
    | None -> Envi.Type.add_instance typ ctyp env )
  | _, Tvar _ -> (
    match
      without_instance ctyp env ~f:(fun ctyp -> check_type_aux typ ctyp)
    with
    | Some env -> env
    | None -> Envi.Type.add_instance ctyp typ env )
  | Tarrow (typ1, typ2), Tarrow (ctyp1, ctyp2) ->
      env |> check_type_aux typ1 ctyp1 |> check_type_aux typ2 ctyp2
  | Tconstr name, Tconstr constr_name
    when String.equal name.txt constr_name.txt ->
      env
  | _, _ -> failwith "Type doesn't check against constr_typ."

let check_type env typ constr_typ = check_type_aux typ constr_typ env

let rec check_pattern ~add env typ = function
  | PVariable str -> add str typ env
  | PConstraint (p, constr_typ) ->
      let constr_typ, env = Envi.Type.import constr_typ env in
      let env = check_type env typ constr_typ in
      check_pattern ~add env constr_typ p

let rec get_expression env = function
  | Apply (f, xs) ->
      let f_typ, env = get_expression env f in
      let rec apply_typ xs f_typ env =
        match xs with
        | [] -> (f_typ, env)
        | x :: xs ->
            let x_typ, env = get_expression env x in
            let retvar, env = Envi.Type.mkvar None env in
            let arrow, env = Envi.Type.mk (Tarrow (x_typ, retvar)) env in
            let env = check_type env f_typ arrow in
            apply_typ xs retvar env
      in
      apply_typ xs f_typ env
  | Variable name -> Envi.get_name name env
  | Int _ -> Envi.Type.mk (Tconstr {txt= "int"; loc= Location.none}) env
  | Fun (p, body) ->
      let env = Envi.open_scope env in
      let p_typ, env = Envi.Type.mkvar None env in
      (* In OCaml, function arguments can't be polymorphic, so each check refines
       them rather than instantiating the parameters. *)
      let env = check_pattern ~add:Envi.add_in_progress env p_typ p in
      let body_typ, env = get_expression env body in
      let env = Envi.close_scope env in
      Envi.Type.mk (Tarrow (p_typ, body_typ)) env
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

and check_binding (env : Envi.t) p e : 's =
  let e_type, env = get_expression env e in
  check_pattern ~add:Envi.add_final env e_type p

let check_statement env = function Value (p, e) -> check_binding env p e

let check (ast : statement list) =
  List.fold_left ast ~init:Envi.empty ~f:(fun env stmt ->
      check_statement env stmt )
