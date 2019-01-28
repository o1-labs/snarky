open Core_kernel
open Parsetypes
open Parsetypes.Type

let type_print typ =
  Out_channel.output_string stderr (Sexp.to_string (sexp_of_type_expr typ)) ;
  Out_channel.newline stderr ;
  let typ = To_ocaml.of_typ typ in
  Pprintast.core_type Format.std_formatter typ ;
  Out_channel.newline stderr

let rec copy_type typ =
  match typ.type_desc with
  | (Tvar _ | Tconstr _) as type_desc -> {typ with type_desc}
  | Tarrow (typ1, typ2) ->
      {typ with type_desc= Tarrow (mk (Tcopy typ1), mk (Tcopy typ2))}
  | Tdefer typ -> copy_type typ
  | Tcopy typ -> copy_type typ
  | Tnocopy _ -> typ

exception Check_failed of type_expr * type_expr

let rec check_type_aux ~defer_as typ constr_typ =
  let check_type_aux = check_type_aux ~defer_as in
  match (typ.type_desc, constr_typ.type_desc) with
  | Tcopy typ, _ -> check_type_aux (copy_type typ) constr_typ
  | _, Tcopy constr_typ -> check_type_aux typ (copy_type constr_typ)
  | _, Tnocopy constr_typ | _, Tdefer constr_typ ->
      check_type_aux typ constr_typ
  | Tnocopy typ, _ | Tdefer typ, _ -> check_type_aux typ constr_typ
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
  | _, _ -> raise (Check_failed (typ, constr_typ))

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

type state =
  { map: (string, type_expr, Base.String.comparator_witness) Base.Map.t
  ; typ_vars:
      (string, type_expr option, Base.String.comparator_witness) Base.Map.t
  ; vars_size: int
  ; generation: int }

let next_type_var i =
  if i < 25 then String.make 1 (Char.of_int_exn (Char.to_int 'a' + i))
  else Printf.sprintf "%d" (i - 24)

let rec find_next_free_var typ_vars vars_size =
  let var = next_type_var vars_size in
  if Map.mem typ_vars var then find_next_free_var typ_vars (vars_size + 1)
  else (var, vars_size)

let rec name_type_variables typ ({typ_vars; vars_size; _} as state) =
  match typ.type_desc with
  | Tvar None ->
      let name, vars_size = find_next_free_var typ_vars vars_size in
      typ.type_desc <- Tvar (Some (Location.mkloc name Location.none)) ;
      let typ_vars = Map.add_exn typ_vars ~key:name ~data:(Some typ) in
      {state with vars_size; typ_vars}
  | Tvar (Some name) -> (
      let old = Map.find typ_vars name.txt in
      let typ_vars = Map.update typ_vars name.txt ~f:(fun _ -> None) in
      let state = {state with typ_vars} in
      match old with
      | Some (Some typ) ->
          typ.type_desc <- Tvar None ;
          name_type_variables typ state
      | _ -> state )
  | Tconstr _ -> state
  | Tarrow (typ1, typ2) ->
      let state = name_type_variables typ1 state in
      name_type_variables typ2 state
  | Tcopy _ ->
      typ.type_desc <- (copy_type typ).type_desc ;
      name_type_variables typ state
  | Tdefer typ | Tnocopy typ -> name_type_variables typ state

let add_type {Location.txt= name; _} (typ : type_expr) state =
  {state with map= Map.update state.map name ~f:(fun _ -> typ)}

let get_name {Location.txt= name; _} {map; _} =
  match Map.find map name with
  | Some typ -> typ
  | None -> failwithf "Could not find name %s." name ()

let add_type_final name typ state =
  let state = name_type_variables typ state in
  add_type name (mk (Tcopy typ)) state

let rec check_pattern ~add state typ pat =
  match pat.pat_desc with
  | PVariable str -> add str typ state
  | PConstraint (p, constr_typ) -> (
    match check_type typ constr_typ with
    | typ -> check_pattern ~add state typ p
    | exception Check_failed (_typ', _constr_typ') -> failwith "" )

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
      let state = check_pattern ~add:add_type state p_typ p in
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
  check_pattern ~add:add_type_final state e_type p

let check_statement state stmt =
  match stmt.stmt_desc with Value (p, e) -> check_binding state p e

let check (ast : statement list) =
  List.fold_left ast
    ~init:
      { map= Map.empty (module String)
      ; typ_vars= Map.empty (module String)
      ; vars_size= 0
      ; generation= 0 }
    ~f:(fun state stmt -> check_statement state stmt)
