open Core_kernel
open Parsetypes
open Parsetypes.Type

let rec copy_type typ =
  match typ.type_desc with
  | (Tvar _ | Tconstr _) as type_desc -> {typ with type_desc}
  | Tarrow (typ1, typ2) ->
      {typ with type_desc= Tarrow (copy_type typ1, copy_type typ2)}
  | Tdefer typ -> copy_type typ

let rec check_type_aux ~defer_as typ constr_typ =
  let check_type_aux = check_type_aux ~defer_as in
  match (typ.type_desc, constr_typ.type_desc) with
  | _, Tdefer constr_typ -> check_type_aux typ constr_typ
  | Tdefer typ, _ -> check_type_aux typ constr_typ
  | Tvar _, Tvar _ ->
      typ.type_desc <- constr_typ.type_desc ;
      defer_as constr_typ typ
  | Tvar _, _ -> typ.type_desc <- constr_typ.type_desc
  | _, Tvar _ -> constr_typ.type_desc <- typ.type_desc
  | Tarrow (typ1, typ2), Tarrow (constr_typ1, constr_typ2) ->
      check_type_aux typ1 constr_typ1 ;
      check_type_aux typ2 constr_typ2
  | Tconstr name, Tconstr constr_name
    when String.equal name.txt constr_name.txt ->
      ()
  | _, _ -> failwith "Type doesn't check against constr_typ."

let check_type typ constr_typ =
  let typs = ref [] in
  let defer_as typ new_typ =
    typ.type_desc <- Tdefer new_typ ;
    typs := typ :: !typs
  in
  let rec fixup_deferred typs =
    match typs with
    | [] -> ()
    | ({type_desc= Tdefer {type_desc; _}; _} as typ) :: typs ->
        typ.type_desc <- type_desc ;
        fixup_deferred typs
    | _ :: typs -> fixup_deferred typs
  in
  check_type_aux ~defer_as typ constr_typ ;
  fixup_deferred !typs ;
  constr_typ

type 'a ready = Final of 'a | In_progress of 'a

let add_final {Location.txt= name; _} typ map =
  Map.update map name ~f:(fun _ -> Final typ)

let add_in_progress {Location.txt= name; _} typ map =
  Map.update map name ~f:(fun _ -> In_progress typ)

let get_name {Location.txt= name; _} map =
  match Map.find map name with
  | Some (In_progress typ) -> typ
  | Some (Final typ) -> copy_type typ
  | None -> failwith "Could not find name."

let rec check_pattern ~add state typ pat =
  match pat.pat_desc with
  | PVariable str -> add str typ state
  | PConstraint (p, constr_typ) ->
      let typ = check_type typ constr_typ in
      check_pattern ~add state typ p

let rec get_expression state exp =
  match exp.exp_desc with
  | Apply (f, xs) ->
      let f_typ = get_expression state f in
      let rec apply_typ xs f_typ =
        match xs with
        | [] -> f_typ
        | x :: xs -> (
            let x_typ = get_expression state x in
            match check_type f_typ (mk (Tarrow (x_typ, mk (Tvar None)))) with
            | {type_desc= Tarrow (_, f_typ); _} -> apply_typ xs f_typ
            | _ -> failwith "Met constraint Tarrow, but didn't match Tarrow.."
            )
      in
      apply_typ xs f_typ
  | Variable name -> get_name name state
  | Int _ -> mk (Tconstr {txt= "int"; loc= Location.none})
  | Fun (p, body) ->
      let p_typ = mk (Tvar None) in
      (* In OCaml, function arguments can't be polymorphic, so each check refines
       them rather than instanciating the parameters. *)
      let state = check_pattern ~add:add_in_progress state p_typ p in
      let body_typ = get_expression state body in
      mk (Tarrow (p_typ, body_typ))
  | Seq (e1, e2) ->
      let _ = get_expression state e1 in
      get_expression state e2
  | Let (p, e1, e2) ->
      let state = check_binding state p e1 in
      get_expression state e2
  | Constraint (e, typ) ->
      let e_typ = get_expression state e in
      check_type e_typ typ

and check_binding (state : 's) p e : 's =
  let e_type = get_expression state e in
  check_pattern ~add:add_final state e_type p

let check_statement state stmt =
  match stmt.stmt_desc with Value (p, e) -> check_binding state p e

let check (ast : statement list) =
  List.fold_left ast
    ~init:(Map.empty (module String))
    ~f:(fun state stmt -> check_statement state stmt)
