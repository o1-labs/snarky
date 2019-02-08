open Core_kernel
open Parsetypes

type 'a name_map = (string, 'a, String.comparator_witness) Base.Map.t

type 'a int_map = (int, 'a, Int.comparator_witness) Base.Map.t

module Scope = struct
  type t = {names: type_expr name_map; type_variables: type_expr name_map}

  let empty =
    { names= Map.empty (module String)
    ; type_variables= Map.empty (module String) }

  let add_name {Location.txt= name; _} typ scope =
    {scope with names= Map.update scope.names name ~f:(fun _ -> typ)}

  let get_name {Location.txt= name; _} {names; _} = Map.find names name

  let add_type_variable name typ scope =
    { scope with
      type_variables= Map.update scope.type_variables name ~f:(fun _ -> typ) }

  let find_type_variable name scope = Map.find scope.type_variables name
end

module TypeEnvi = struct
  type t =
    { type_id: int
    ; variables: type_expr name_map
    ; variable_instances: type_expr int_map }

  let empty =
    { type_id= 1
    ; variables= Map.empty (module String)
    ; variable_instances= Map.empty (module Int) }

  let instance env (typ : type_expr) =
    Map.find env.variable_instances typ.type_id

  let add_instance (typ : type_expr) typ' env =
    { env with
      variable_instances=
        Map.update env.variable_instances typ.type_id ~f:(fun _ -> typ') }

  let clear_instance (typ : type_expr) env =
    {env with variable_instances= Map.remove env.variable_instances typ.type_id}

  let next_id env = (env.type_id, {env with type_id= env.type_id + 1})
end

type t = {scope_stack: Scope.t list; type_env: TypeEnvi.t; depth: int}

let empty = {scope_stack= [Scope.empty]; type_env= TypeEnvi.empty; depth= 0}

let current_scope {scope_stack; _} =
  match List.hd scope_stack with
  | Some scope -> scope
  | None -> failwith "No environment scopes are open"

let open_scope env =
  {env with scope_stack= Scope.empty :: env.scope_stack; depth= env.depth + 1}

let close_scope env =
  match List.tl env.scope_stack with
  | Some scope_stack -> {env with scope_stack; depth= env.depth - 1}
  | None -> failwith "No environment scopes are open"

let map_current_scope ~f env =
  match env.scope_stack with
  | current_scope :: scope_stack ->
      {env with scope_stack= f current_scope :: scope_stack}
  | [] -> failwith "No environment scopes are open"

let add_type_variable name typ =
  map_current_scope ~f:(Scope.add_type_variable name typ)

let find_type_variable name env =
  List.find_map ~f:(Scope.find_type_variable name) env.scope_stack

module Type = struct
  let mk ~loc type_desc env =
    let type_id, type_env = TypeEnvi.next_id env.type_env in
    let env = {env with type_env} in
    ({type_desc; type_id; type_loc= loc}, env)

  let mkvar ~loc name env = mk ~loc (Tvar (name, env.depth)) env

  let instance env typ = TypeEnvi.instance env.type_env typ

  let map_env ~f env = {env with type_env= f env.type_env}

  let add_instance typ typ' = map_env ~f:(TypeEnvi.add_instance typ typ')

  let clear_instance typ = map_env ~f:(TypeEnvi.clear_instance typ)

  let rec import ?(force_new = false) typ env =
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar (None, _) -> mkvar ~loc None env
    | Tvar ((Some {txt= x; _} as name), _) -> (
        let var = if force_new then None else find_type_variable x env in
        match var with
        | Some var -> (var, env)
        | None ->
            let var, env = mkvar ~loc name env in
            (var, add_type_variable x var env) )
    | Tpoly (vars, typ) ->
        let env = open_scope env in
        let env, vars =
          List.fold_map vars ~init:env ~f:(fun e t ->
              let t, e = import ~force_new:true t e in
              (e, t) )
        in
        let typ, env = import typ env in
        let env = close_scope env in
        mk ~loc (Tpoly (vars, typ)) env
    | Tctor _ -> mk ~loc typ.type_desc env
    | Ttuple typs ->
        let env, typs =
          List.fold_map typs ~init:env ~f:(fun e t ->
              let t, e = import t e in
              (e, t) )
        in
        mk ~loc (Ttuple typs) env
    | Tarrow (typ1, typ2) ->
        let typ1, env = import typ1 env in
        let typ2, env = import typ2 env in
        mk ~loc (Tarrow (typ1, typ2)) env

  let rec copy typ new_vars_map env =
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar _ -> (
      match Map.find new_vars_map typ.type_id with
      | Some var -> (var, env)
      | None -> (typ, env) )
    | Tpoly (vars, typ) ->
        let env, vars =
          List.fold_map vars ~init:env ~f:(fun e t ->
              let t, e = import ~force_new:true t e in
              (e, t) )
        in
        let new_vars_map =
          List.fold ~init:new_vars_map vars ~f:(fun map var ->
              Map.update map var.type_id ~f:(fun _ -> var) )
        in
        let typ, env = copy typ new_vars_map env in
        mk ~loc (Tpoly (vars, typ)) env
    | Tctor _ -> mk ~loc typ.type_desc env
    | Ttuple typs ->
        let env, typs =
          List.fold_map typs ~init:env ~f:(fun e t ->
              let t, e = copy t new_vars_map e in
              (e, t) )
        in
        mk ~loc (Ttuple typs) env
    | Tarrow (typ1, typ2) ->
        let typ1, env = copy typ1 new_vars_map env in
        let typ2, env = copy typ2 new_vars_map env in
        mk ~loc (Tarrow (typ1, typ2)) env

  module T = struct
    type t = type_expr

    let compare typ1 typ2 = Int.compare typ1.type_id typ2.type_id

    let sexp_of_t typ = Int.sexp_of_t typ.type_id
  end

  module Comparator = struct
    include T
    include Comparator.Make (T)
  end

  include Comparator

  let rec type_vars ?depth typ =
    let deep_enough x =
      match depth with Some depth -> depth <= x | None -> true
    in
    let type_vars' = type_vars in
    let type_vars = type_vars ?depth in
    match typ.type_desc with
    | Tvar (_, var_depth) when deep_enough var_depth ->
        Set.singleton (module Comparator) typ
    | Tvar _ -> Set.empty (module Comparator)
    | Tpoly (vars, typ) ->
        let poly_vars =
          List.fold
            ~init:(Set.empty (module Comparator))
            vars
            ~f:(fun set var -> Set.union set (type_vars' var))
        in
        Set.diff (type_vars typ) poly_vars
    | Tctor _ -> Set.empty (module Comparator)
    | Ttuple typs ->
        Set.union_list (module Comparator) (List.map ~f:type_vars typs)
    | Tarrow (typ1, typ2) -> Set.union (type_vars typ1) (type_vars typ2)

  let rec flatten typ env =
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar _ -> (
      match instance env typ with
      | Some typ' ->
          let flattened_typ, env = flatten typ' (clear_instance typ env) in
          (flattened_typ, add_instance typ typ' env)
      | None -> (typ, env) )
    | Tpoly (vars, typ) ->
        let env, var_set =
          List.fold vars
            ~init:(env, Set.empty (module Comparator))
            ~f:(fun (env, set) var ->
              let var, env = flatten var env in
              let set = Set.union set (type_vars ~depth:env.depth var) in
              (env, set) )
        in
        let typ, env = flatten typ env in
        mk ~loc (Tpoly (Set.to_list var_set, typ)) env
    | Tctor _ -> mk ~loc typ.type_desc env
    | Ttuple typs ->
        let env, typs =
          List.fold_map typs ~init:env ~f:(fun e t ->
              let t, e = flatten t e in
              (e, t) )
        in
        mk ~loc (Ttuple typs) env
    | Tarrow (typ1, typ2) ->
        let typ1, env = flatten typ1 env in
        let typ2, env = flatten typ2 env in
        mk ~loc (Tarrow (typ1, typ2)) env
end

let add_name name typ = map_current_scope ~f:(Scope.add_name name typ)

let get_name name env =
  Option.map
    (List.find_map ~f:(Scope.get_name name) env.scope_stack)
    ~f:(fun typ -> Type.copy typ (Map.empty (module Int)) env)
