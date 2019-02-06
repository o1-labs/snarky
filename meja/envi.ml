open Core_kernel
open Parsetypes

type 'a ready = Final of 'a | In_progress of 'a

type 'a name_map = (string, 'a, String.comparator_witness) Base.Map.t

type 'a int_map = (int, 'a, Int.comparator_witness) Base.Map.t

module Scope = struct
  type t = {names: type_expr ready name_map}

  let empty = {names= Map.empty (module String)}

  let add_final {Location.txt= name; _} typ scope =
    {names= Map.update scope.names name ~f:(fun _ -> Final typ)}

  let add_in_progress {Location.txt= name; _} typ scope =
    {names= Map.update scope.names name ~f:(fun _ -> In_progress typ)}

  let get_name {Location.txt= name; _} {names; _} = Map.find names name
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

  let add_variable name typ env =
    {env with variables= Map.update env.variables name ~f:(fun _ -> typ)}

  let find_variable name env = Map.find env.variables name

  let next_id env = (env.type_id, {env with type_id= env.type_id + 1})
end

type t = {scope_stack: Scope.t list; type_env: TypeEnvi.t; depth: int}

let empty = {scope_stack= [Scope.empty]; type_env= TypeEnvi.empty; depth= 0}

module Type = struct
  let mk type_desc env =
    let type_id, type_env = TypeEnvi.next_id env.type_env in
    let env = {env with type_env} in
    ({type_desc; type_id}, env)

  let mkvar name env = mk (Tvar (name, env.depth)) env

  let instance env typ = TypeEnvi.instance env.type_env typ

  let map_env ~f env = {env with type_env= f env.type_env}

  let add_instance typ typ' = map_env ~f:(TypeEnvi.add_instance typ typ')

  let clear_instance typ = map_env ~f:(TypeEnvi.clear_instance typ)

  let rec import typ env =
    match typ.type_desc with
    | Tvar (None, _) -> mkvar None env
    | Tvar ((Some {txt= x; _} as name), _) -> (
      match TypeEnvi.find_variable x env.type_env with
      | Some var -> (var, env)
      | None ->
          let var, env = mkvar name env in
          (var, {env with type_env= TypeEnvi.add_variable x var env.type_env})
      )
    | Tconstr _ -> mk typ.type_desc env
    | Tarrow (typ1, typ2) ->
        let typ1, env = import typ1 env in
        let typ2, env = import typ2 env in
        mk (Tarrow (typ1, typ2)) env

  let rec copy typ =
    match typ.type_desc with
    | (Tvar _ | Tconstr _) as type_desc -> {typ with type_desc}
    | Tarrow (typ1, typ2) -> {typ with type_desc= Tarrow (copy typ1, copy typ2)}
end

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

let add_final name typ = map_current_scope ~f:(Scope.add_final name typ)

let add_in_progress name typ =
  map_current_scope ~f:(Scope.add_in_progress name typ)

let get_name name env =
  match List.find_map ~f:(Scope.get_name name) env.scope_stack with
  | Some (In_progress typ) -> (typ, env)
  | Some (Final typ) -> Type.import typ env
  | None -> failwith "Could not find name."
