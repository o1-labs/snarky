open Core_kernel
open Parsetypes

type 'a ready = Final of 'a | In_progress of 'a

type 'a name_map = (string, 'a, String.comparator_witness) Base.Map.t

module Scope = struct
  type t = {names: type_expr ready name_map}

  let empty = {names= Map.empty (module String)}

  let add_final {Location.txt= name; _} typ scope =
    {names= Map.update scope.names name ~f:(fun _ -> Final typ)}

  let add_in_progress {Location.txt= name; _} typ scope =
    {names= Map.update scope.names name ~f:(fun _ -> In_progress typ)}

  let get_name {Location.txt= name; _} {names; _} = Map.find names name
end

type t = {scope_stack: Scope.t list; type_id: int}

let empty = {scope_stack= [Scope.empty]; type_id= 1}

module Type = struct
  let mk type_desc env =
    let {type_id; _} = env in
    let env = {env with type_id= type_id + 1} in
    ({type_desc; type_id}, env)

  let rec copy typ =
    match typ.type_desc with
    | (Tvar _ | Tconstr _) as type_desc -> {typ with type_desc}
    | Tarrow (typ1, typ2) -> {typ with type_desc= Tarrow (copy typ1, copy typ2)}
end

let current_scope {scope_stack; _} =
  match List.hd scope_stack with
  | Some scope -> scope
  | None -> failwith "No environment scopes are open"

let open_scope env = {env with scope_stack= Scope.empty :: env.scope_stack}

let close_scope env =
  match List.tl env.scope_stack with
  | Some scope_stack -> {env with scope_stack}
  | None -> failwith "No environment scopes are open"

let map_current_scope ~f env =
  match env.scope_stack with
  | current_scope :: scope_stack ->
      {env with scope_stack= f current_scope :: scope_stack}
  | [] -> failwith "No environment scopes are open"

let add_final name typ = map_current_scope ~f:(Scope.add_final name typ)

let add_in_progress name typ =
  map_current_scope ~f:(Scope.add_in_progress name typ)

let get_name name {scope_stack; _} =
  match List.find_map ~f:(Scope.get_name name) scope_stack with
  | Some (In_progress typ) -> typ
  | Some (Final typ) -> Type.copy typ
  | None -> failwith "Could not find name."
