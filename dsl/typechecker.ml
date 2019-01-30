open Core_kernel
open Parsetypes
open Parsetypes.Type

let type_print typ =
  let typ = To_ocaml.of_typ typ in
  Pprintast.core_type Format.std_formatter typ ;
  Format.pp_print_newline Format.std_formatter ()

let rec copy_type depth typ =
  let loc = typ.type_loc in
  match typ.type_desc with
  | Tvar _ -> typ
  | Tpoly (var, typ) ->
      let newvar = mk_var ~loc ~depth None in
      let oldvar = var.type_desc in
      var.type_desc <- Tdefer newvar ;
      let typ = copy_type depth typ in
      var.type_desc <- oldvar ;
      mk ~loc (Tpoly (newvar, typ))
  | Tdefer typ -> typ
  | Tconstr _ -> mk ~loc typ.type_desc
  | Tarrow (typ1, typ2) ->
      mk ~loc (Tarrow (copy_type depth typ1, copy_type depth typ2))

exception Check_failed of type_expr * type_expr

let rec check_type_aux typ constr_typ =
  if not (phys_equal typ constr_typ) then
    match (typ.type_desc, constr_typ.type_desc) with
    | Tpoly (_, typ), _ -> check_type_aux typ constr_typ
    | _, Tpoly (_, constr_typ) -> check_type_aux typ constr_typ
    | Tconstr name, Tconstr constr_name
      when String.equal name.txt constr_name.txt ->
        ()
    | Tarrow (typ1, typ2), Tarrow (constr_typ1, constr_typ2) ->
        check_type_aux typ1 constr_typ1 ;
        check_type_aux typ2 constr_typ2
    | Tdefer _, _ | _, Tdefer _ ->
        failwith "Unexpected Tdefer outside copy_type."
    | Tvar data, Tvar constr_data -> (
      match (data.instance, constr_data.instance) with
      | None, None ->
          if data.depth <= constr_data.depth then
            constr_data.instance <- Some typ
          else data.instance <- Some constr_typ
      | Some typ', None ->
          if data.depth <= constr_data.depth then
            constr_data.instance <- Some typ
          else (
            constr_data.instance <- Some typ' ;
            data.instance <- Some constr_typ )
      | None, Some constr_typ' ->
          if constr_data.depth <= data.depth then
            data.instance <- Some constr_typ
          else (
            data.instance <- Some constr_typ' ;
            constr_data.instance <- Some typ )
      | Some typ', Some _constr_typ' ->
          let in_recursion = typ.in_recursion in
          typ.in_recursion <- true ;
          data.instance <- Some constr_typ ;
          check_type_aux typ' constr_typ ;
          typ.in_recursion <- in_recursion ;
          if data.depth < constr_data.depth then (
            data.instance <- constr_data.instance ;
            constr_data.instance <- Some typ ) )
    | _, Tvar constr_data -> (
      match constr_data.instance with
      | None -> constr_data.instance <- Some typ
      | Some constr_typ' ->
          if constr_typ.in_recursion then ()
            (* Don't do anything, or we'll loop forever. *)
          else check_type_aux typ constr_typ' )
    | Tvar data, _ -> (
      match data.instance with
      | None -> data.instance <- Some constr_typ
      | Some typ' ->
          let in_recursion = typ.in_recursion in
          typ.in_recursion <- true ;
          data.instance <- Some constr_typ ;
          check_type_aux typ' constr_typ ;
          typ.in_recursion <- in_recursion )
    | _, _ -> raise (Check_failed (typ, constr_typ))

let check_type ~loc typ constr_typ =
  ( try check_type_aux typ constr_typ
    with Check_failed (typ', constr_typ') ->
      let open Format in
      let pp_typ typ =
        Pprintast.core_type err_formatter (To_ocaml.of_typ typ)
      in
      Location.print_error err_formatter loc ;
      pp_print_string err_formatter " Type error: Cannot unify '" ;
      pp_typ typ ;
      pp_print_string err_formatter "' and '" ;
      pp_typ constr_typ ;
      pp_print_string err_formatter ", types '" ;
      pp_typ typ' ;
      pp_print_string err_formatter "' and '" ;
      pp_typ constr_typ' ;
      pp_print_string err_formatter "' are incompatable." ;
      pp_print_newline err_formatter () ) ;
  constr_typ

type state =
  { map:
      ( string
      , [`Copy | `NoCopy] * type_expr
      , Base.String.comparator_witness )
      Base.Map.t
  ; typ_vars:
      ( string
      , [`User | `Generated] * type_expr
      , Base.String.comparator_witness )
      Base.Map.t
  ; vars_size: int
  ; depth: int }

let rec unify_after_parse' state typ =
  match typ.type_desc with
  | Tpoly (_, typ) -> unify_after_parse' state typ
  | Tvar {name= Some name; _} -> (
    match Map.find state.typ_vars name.txt with
    | Some (_, var) -> (var, state, Base.Set.empty (module Type))
    | None ->
        let typ = mk_var ~loc:typ.type_loc ~depth:state.depth (Some name) in
        let state =
          { state with
            typ_vars=
              Map.add_exn state.typ_vars ~key:name.txt ~data:(`User, typ) }
        in
        (typ, state, Base.Set.singleton (module Type) typ) )
  | Tvar {name= None; _} ->
      let typ = mk_var ~loc:typ.type_loc ~depth:state.depth None in
      (typ, state, Base.Set.singleton (module Type) typ)
  | Tconstr _ -> (typ, state, Base.Set.empty (module Type))
  | Tarrow (typ1, typ2) ->
      let typ1, state, vars1 = unify_after_parse' state typ1 in
      let typ2, state, vars2 = unify_after_parse' state typ2 in
      typ.type_desc <- Tarrow (typ1, typ2) ;
      (typ, state, Base.Set.union vars1 vars2)
  | Tdefer typ (*| Tcopy (typ, _) | Tnocopy (typ, _)*) ->
      unify_after_parse' state typ

let rec type_vars typ =
  match typ.type_desc with
  | Tpoly (_, typ) -> type_vars typ
  | Tvar _ -> Base.Set.singleton (module Type) typ
  | Tconstr _ -> Base.Set.empty (module Type)
  | Tarrow (typ1, typ2) -> Base.Set.union (type_vars typ1) (type_vars typ2)
  | Tdefer typ (*| Tcopy (typ, _) | Tnocopy (typ, _)*) -> type_vars typ

let unify_after_parse state typ =
  let typ, state, _ = unify_after_parse' state typ in
  (typ, state)

let polymorphise typ vars =
  let loc = typ.type_loc in
  let typ = ref typ in
  Base.Set.iter vars ~f:(fun var -> typ := mk ~loc (Tpoly (var, !typ))) ;
  !typ

let rec strip_polymorphism typ =
  match typ.type_desc with
  | Tpoly (_, typ) -> strip_polymorphism typ
  | _ -> typ

let unify_and_polymorphise_after_parse state typ =
  let typ, _, vars = unify_after_parse' state typ in
  (polymorphise typ vars, state)

let next_type_var i =
  if i < 25 then String.make 1 (Char.of_int_exn (Char.to_int 'a' + i))
  else Printf.sprintf "%d" (i - 24)

let rec find_next_free_var typ_vars vars_size =
  let var = next_type_var vars_size in
  if Map.mem typ_vars var then find_next_free_var typ_vars (vars_size + 1)
  else (var, vars_size)

(** Accepts a [type_expr] of a [Tvar] as an argument.
    Capture the set of all the irreducible type variables that make up the
    instance of the type variable.
    This may include the type variable itself, if it is self-referential or has
    no instance. *)
let capture_type_vars vars typ =
  let rec capture_type_vars depth typ vars removed_vars =
    match typ.type_desc with
    | Tpoly _ -> failwith "Unexpected Tpoly in capture_type_variables."
    | Tdefer _ -> failwith "Unexpected Tdefer in capture_type_variables."
    | Tvar {instance= Some typ'; depth= depth'; _} ->
        if Set.mem vars typ then vars
        else if Set.mem removed_vars typ then
          (* Variable is self-referential. *)
          if depth <= depth' then Set.add vars typ else vars
        else capture_type_vars depth typ' vars (Set.add removed_vars typ)
    | Tvar _ -> Set.add vars typ
    | Tconstr _ -> Set.empty (module Type)
    | Tarrow (typ1, typ2) ->
        let vars = capture_type_vars depth typ1 vars removed_vars in
        capture_type_vars depth typ2 vars removed_vars
  in
  let var_set = Set.singleton (module Type) typ in
  match typ.type_desc with
  | Tvar {instance= Some typ; depth; _} -> (
    try capture_type_vars depth typ vars var_set with a ->
      pp_type_expr Format.std_formatter typ ;
      raise a )
  | Tvar _ -> var_set
  | _ ->
      failwith "Bad argument given to capture_type_variables; expected a Tvar."

let reduce_type_vars typ =
  let rec reduce_type_vars typ vars =
    match typ.type_desc with
    | Tpoly (var, typ) -> reduce_type_vars typ (Set.add vars var)
    | _ -> Set.fold vars ~init:(Set.empty (module Type)) ~f:capture_type_vars
  in
  reduce_type_vars typ (Set.empty (module Type))

let rec name_type_variables typ ({typ_vars; vars_size; _} as state) =
  if false then
    match typ.type_desc with
    | Tvar ({name= None; _} as data) ->
        let name, vars_size = find_next_free_var typ_vars vars_size in
        typ.type_desc
        <- Tvar {data with name= Some (Location.mkloc name Location.none)} ;
        let typ_vars =
          Map.add_exn typ_vars ~key:name ~data:(`Generated, typ)
        in
        {state with vars_size; typ_vars}
    | Tvar {name= Some name; _} -> (
        let old = Map.find typ_vars name.txt in
        let typ_vars =
          Map.update typ_vars name.txt ~f:(fun _ -> (`User, typ))
        in
        let state = {state with typ_vars} in
        match old with
        | Some (`Generated, ({type_desc= Tvar data; _} as typ)) ->
            typ.type_desc <- Tvar {data with name= None} ;
            name_type_variables typ state
        | _ -> state )
    | Tpoly (_var, typ) -> name_type_variables typ state
    | Tconstr _ -> state
    | Tarrow (typ1, typ2) ->
        let state = name_type_variables typ1 state in
        name_type_variables typ2 state
    | Tdefer typ -> name_type_variables typ state
  else state

let add_type {Location.txt= name; _} typ state =
  {state with map= Map.update state.map name ~f:(fun _ -> typ)}

let get_name {Location.txt= name; _} {map; depth; _} =
  match Map.find map name with
  | Some (`Copy, typ) -> copy_type depth typ
  | Some (`NoCopy, typ) -> typ
  | None -> failwithf "Could not find name %s." name ()

let add_type_final name typ state =
  let typ_vars = reduce_type_vars typ in
  let typ = polymorphise typ typ_vars in
  let state = name_type_variables typ state in
  add_type name (`Copy, typ) state

let add_type_in_progress name typ state = add_type name (`NoCopy, typ) state

let rec check_pattern ~add ~after_parse state typ pat =
  match pat.pat_desc with
  | PVariable str -> add str typ state
  | PConstraint ({pcon_pat= p; pcon_typ= constr_typ} as data) ->
      let constr_typ, state = after_parse state constr_typ in
      data.pcon_typ <- constr_typ ;
      let typ = check_type ~loc:pat.pat_loc typ constr_typ in
      check_pattern ~add ~after_parse state typ p

let rec get_expression state exp =
  let loc = exp.exp_loc in
  match exp.exp_desc with
  | Apply (f, xs) ->
      let f_typ = get_expression state f in
      let rec apply_typ xs f_typ =
        match xs with
        | [] -> f_typ
        | x :: xs -> (
            let x_typ = get_expression state x in
            match
              check_type ~loc f_typ
                (mk ~loc (Tarrow (x_typ, mk_var ~loc None)))
            with
            | {type_desc= Tarrow (_, f_typ); _} -> apply_typ xs f_typ
            | _ -> failwith "Met constraint Tarrow, but didn't match Tarrow.."
            )
      in
      apply_typ xs f_typ
  | Variable name -> get_name name state
  | Int _ -> mk (Tconstr {txt= "int"; loc})
  | Fun (p, body) ->
      (* In OCaml, function arguments can't be polymorphic, so each check refines
       them rather than instanciating the parameters. *)
      let state = {state with depth= state.depth + 1} in
      let p_typ = mk_var ~loc None in
      let state =
        check_pattern ~add:add_type_in_progress ~after_parse:unify_after_parse
          state p_typ p
      in
      let body_typ = get_expression state body in
      mk ~loc (Tarrow (p_typ, strip_polymorphism body_typ))
  | Seq (e1, e2) ->
      let _ = get_expression state e1 in
      get_expression state e2
  | Let (p, e1, e2) ->
      let state = check_binding state p e1 in
      get_expression state e2
  | Constraint {econ_exp= e; econ_typ= typ} ->
      let e_typ = get_expression state e in
      check_type ~loc e_typ typ

and check_binding (state : 's) p e : 's =
  let e_type = get_expression state e in
  check_pattern ~add:add_type_final
    ~after_parse:unify_and_polymorphise_after_parse state e_type p

let check_statement state stmt =
  match stmt.stmt_desc with Value (p, e) -> check_binding state p e

let check (ast : statement list) =
  List.fold_left ast
    ~init:
      { map= Map.empty (module String)
      ; typ_vars= Map.empty (module String)
      ; vars_size= 0
      ; depth= 0 }
    ~f:(fun state stmt -> check_statement state stmt)
