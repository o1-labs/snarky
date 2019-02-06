open Core_kernel
open Parsetypes

(*
let rec check_type_aux ~defer_as typ constr_typ =
  let check_type_aux = check_type_aux ~defer_as in
  match (typ.desc, constr_typ.desc) with
  | _, Tdefer constr_typ -> check_type_aux typ constr_typ
  | Tdefer typ, _ -> check_type_aux typ constr_typ
  | Tvar _, Tvar _ ->
      typ.desc <- constr_typ.desc ;
      defer_as constr_typ typ
  | Tvar _, _ -> typ.desc <- constr_typ.desc
  | _, Tvar _ -> constr_typ.desc <- typ.desc
  | Tarrow (typ1, typ2), Tarrow (constr_typ1, constr_typ2) ->
      check_type_aux typ1 constr_typ1 ;
      check_type_aux typ2 constr_typ2
  | Tconstr name, Tconstr constr_name
    when String.equal name.txt constr_name.txt ->
      ()
  | _, _ -> failwith "Type doesn't check against constr_typ."
  *)

let check_type env _typ _constr_typ =
  (*
  let typs = ref [] in
  let defer_as typ new_typ =
    typ.desc <- Tdefer new_typ ;
    typs := typ :: !typs
  in
  let rec fixup_deferred typs =
    match typs with
    | [] -> ()
    | ({desc= Tdefer {desc; _}; _} as typ) :: typs ->
        typ.desc <- desc ;
        fixup_deferred typs
    | _ :: typs -> fixup_deferred typs
  in
  check_type_aux ~defer_as typ constr_typ ;
  fixup_deferred !typs ;*)
  env

let rec check_pattern ~add env typ = function
  | PVariable str -> add str typ env
  | PConstraint (p, constr_typ) ->
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
            let retvar, env = Envi.Type.mk (Tvar None) env in
            let arrow, env = Envi.Type.mk (Tarrow (x_typ, retvar)) env in
            let env = check_type env f_typ arrow in
            apply_typ xs retvar env
      in
      apply_typ xs f_typ env
  | Variable name -> (Envi.get_name name env, env)
  | Int _ -> Envi.Type.mk (Tconstr {txt= "int"; loc= Location.none}) env
  | Fun (p, body) ->
      let env = Envi.open_scope env in
      let p_typ, env = Envi.Type.mk (Tvar None) env in
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
      let e_typ, env = get_expression env e in
      (e_typ, check_type env e_typ typ)

and check_binding (env : Envi.t) p e : 's =
  let e_type, env = get_expression env e in
  check_pattern ~add:Envi.add_final env e_type p

let check_statement env = function Value (p, e) -> check_binding env p e

let check (ast : statement list) =
  List.fold_left ast ~init:Envi.empty ~f:(fun env stmt ->
      check_statement env stmt )
