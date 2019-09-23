open Compiler_internals
open Core_kernel
open Ast_types
open Longident
open Type0
open Type1
open Ast_build.Loc
module IdTbl = Ident.Table

type error =
  | No_open_scopes
  | Wrong_scope_kind of string
  | Multiple_definition of string * string
  | Unbound_type_var of type_expr
  | Unbound_type of Longident.t
  | Unbound_module of Longident.t
  | Unbound_value of Longident.t
  | Unbound of string * Longident.t
  | Wrong_number_args of Path.t * int * int
  | Expected_type_var of type_expr
  | Lident_unhandled of string * Longident.t
  | Constraints_not_satisfied of type_expr * type_decl
  | No_unifiable_implicit
  | Multiple_instances of type_expr
  | Recursive_load of string
  | Predeclared_types of Ident.t list
  | Functor_in_module_sig
  | Not_a_functor

exception Error of Location.t * error

module TypeEnvi = struct
  type t =
    { type_decl_id: int
    ; instance_id: int
    ; implicit_vars: Typedast.expression list
    ; implicit_id: int
    ; instances: (int * type_expr) list
    ; predeclared_types:
        (int (* id *) * int option ref (* num. args *) * Location.t) IdTbl.t }

  let empty =
    { type_decl_id= 1
    ; instance_id= 1
    ; implicit_id= 1
    ; implicit_vars= []
    ; instances= []
    ; predeclared_types= IdTbl.empty }

  let next_type_id env = (env.type_id, {env with type_id= env.type_id + 1})

  let next_decl_id env =
    (env.type_decl_id, {env with type_decl_id= env.type_decl_id + 1})

  let next_instance_id env =
    (env.instance_id, {env with instance_id= env.instance_id + 1})

  let add_implicit_instance id typ env =
    {env with instances= (id, typ) :: env.instances}
end

type 'a or_deferred =
  | Immediate of 'a
  | Deferred of string
  | In_flight of string

type 'a resolve_env =
  { mutable type_env: TypeEnvi.t
  ; mutable external_modules: 'a or_deferred IdTbl.t
  ; mutable predeclare_types: bool }

module Scope = struct
  type 'a or_path = Immediate of 'a | Deferred of Longident.t

  type 't kind =
    | Module
    | Expr
    | Open of Path.t
    | Continue
    | Functor of (Longident.t -> 't or_path -> 't)

  type t =
    { kind: t kind
    ; path: Longident.t option
    ; names: type_expr IdTbl.t
    ; type_variables: type_expr String.Map.t
    ; type_decls: type_decl IdTbl.t
    ; fields: (type_decl * int) IdTbl.t
    ; ctors: (type_decl * int) IdTbl.t
    ; modules: t or_path IdTbl.t
    ; module_types: t or_path IdTbl.t
    ; instances: Path.t Int.Map.t
    ; mode: mode }

  let load_module :
      (loc:Location.t -> name:string -> t resolve_env -> string -> t) ref =
    ref (fun ~loc ~name _env _filename ->
        raise (Error (loc, Unbound_module (Lident name))) )

  let empty ~mode path kind =
    { kind
    ; path
    ; names= IdTbl.empty
    ; type_variables= String.Map.empty
    ; type_decls= IdTbl.empty
    ; fields= IdTbl.empty
    ; ctors= IdTbl.empty
    ; modules= IdTbl.empty
    ; module_types= IdTbl.empty
    ; instances= Int.Map.empty
    ; mode }

  let set_path path env = {env with path= Some path}

  let add_name key typ scope =
    {scope with names= IdTbl.add scope.names ~key ~data:typ}

  let get_name name {names; _} = IdTbl.find name names

  let find_name ~mode name {names; _} =
    IdTbl.find_name ~modes:(modes_of_mode mode) name names

  let add_type_variable key typ scope =
    {scope with type_variables= Map.set scope.type_variables ~key ~data:typ}

  let find_type_variable name scope = Map.find scope.type_variables name

  let add_field decl index scope field_decl =
    { scope with
      fields=
        IdTbl.add scope.fields ~key:field_decl.fld_ident ~data:(decl, index) }

  let get_field ident scope = IdTbl.find ident scope.fields

  let find_field ~mode name scope =
    IdTbl.find_name ~modes:(modes_of_mode mode) name scope.fields

  let add_ctor decl index scope ctor_decl =
    { scope with
      ctors= IdTbl.add scope.ctors ~key:ctor_decl.ctor_ident ~data:(decl, index)
    }

  let get_ctor name scope = IdTbl.find name scope.ctors

  let find_ctor ~mode name scope =
    IdTbl.find_name ~modes:(modes_of_mode mode) name scope.ctors

  let add_type_declaration decl scope =
    { scope with
      type_decls= IdTbl.add scope.type_decls ~key:decl.tdec_ident ~data:decl }

  let get_type_declaration name scope = IdTbl.find name scope.type_decls

  let find_type_declaration ~mode name scope =
    IdTbl.find_name ~modes:(modes_of_mode mode) name scope.type_decls

  let register_type_declaration decl scope =
    let scope' = scope in
    let scope = add_type_declaration decl scope in
    match decl.tdec_desc with
    | TAbstract | TAlias _ | TOpen | TForward _ ->
        scope
    | TRecord fields ->
        List.foldi ~f:(add_field decl) ~init:scope fields
    | TVariant ctors ->
        List.foldi ~f:(add_ctor decl) ~init:scope ctors
    | TExtend (_, _, ctors) ->
        (* Use [scope'] to avoid adding the type name. *)
        List.foldi ~f:(add_ctor decl) ~init:scope' ctors

  let fold_over ~init:acc ~names ~type_variables ~type_decls ~fields ~ctors
      ~modules ~module_types ~instances
      { kind= _
      ; path= _
      ; names= names1
      ; type_variables= type_variables1
      ; type_decls= type_decls1
      ; fields= fields1
      ; ctors= ctors1
      ; modules= modules1
      ; module_types= module_types1
      ; instances= instances1
      ; mode= _ }
      { kind= _
      ; path= _
      ; names= names2
      ; type_variables= type_variables2
      ; type_decls= type_decls2
      ; fields= fields2
      ; ctors= ctors2
      ; modules= modules2
      ; module_types= module_types2
      ; instances= instances2
      ; mode= _ } =
    let acc =
      Map.fold2 type_variables1 type_variables2 ~init:acc ~f:type_variables
    in
    let acc =
      IdTbl.fold2_names type_decls1 type_decls2 ~init:acc ~f:type_decls
    in
    let acc = IdTbl.fold2_names ctors1 ctors2 ~init:acc ~f:ctors in
    let acc = IdTbl.fold2_names fields1 fields2 ~init:acc ~f:fields in
    let acc = IdTbl.fold2_names modules1 modules2 ~init:acc ~f:modules in
    let acc =
      IdTbl.fold2_names module_types1 module_types2 ~init:acc ~f:module_types
    in
    let acc = Map.fold2 instances1 instances2 ~init:acc ~f:instances in
    let acc = IdTbl.fold2_names names1 names2 ~init:acc ~f:names in
    acc

  (* [join ~loc scope1 scope2] attaches the definitions in [scope2] to
     [scope1], raising an error at [loc] if a name is defined in both scopes
     when a single definition of that kind is allowed.

     The context (kind, path, etc.) of [scope1] is preserved; the context of
     [scope2] is discarded.
  *)
  let join ~loc
      { kind
      ; path
      ; names= names1
      ; type_variables= type_variables1
      ; type_decls= type_decls1
      ; fields= fields1
      ; ctors= ctors1
      ; modules= modules1
      ; module_types= module_types1
      ; instances= instances1
      ; mode= mode1 }
      { kind= _
      ; path= _
      ; names= names2
      ; type_variables= type_variables2
      ; type_decls= type_decls2
      ; fields= fields2
      ; ctors= ctors2
      ; modules= modules2
      ; module_types= module_types2
      ; instances= instances2
      ; mode= mode2 } =
    { kind
    ; path
    ; names=
        IdTbl.merge_skewed_names names1 names2 ~combine:(fun ~key:_ _ v -> v)
    ; type_variables=
        Map.merge_skewed type_variables1 type_variables2
          ~combine:(fun ~key:_ _ v -> v)
    ; type_decls=
        IdTbl.merge_skewed_names type_decls1 type_decls2
          ~combine:(fun ~key _ _ ->
            raise (Error (loc, Multiple_definition ("type", key))) )
    ; fields=
        IdTbl.merge_skewed_names fields1 fields2 ~combine:(fun ~key:_ _ v -> v)
    ; ctors=
        IdTbl.merge_skewed_names ctors1 ctors2 ~combine:(fun ~key:_ _ v -> v)
    ; modules=
        IdTbl.merge_skewed_names modules1 modules2 ~combine:(fun ~key _ _ ->
            raise (Error (loc, Multiple_definition ("module", key))) )
    ; module_types=
        IdTbl.merge_skewed_names module_types1 module_types2
          ~combine:(fun ~key _ _ ->
            raise (Error (loc, Multiple_definition ("module type", key))) )
    ; instances=
        Map.merge_skewed instances1 instances2 ~combine:(fun ~key:_ _ v -> v)
    ; mode= weakest_mode mode1 mode2 }

  let add_module name m scope =
    {scope with modules= IdTbl.add scope.modules ~key:name ~data:m}

  let add_module_type name m scope =
    {scope with module_types= IdTbl.add scope.module_types ~key:name ~data:m}

  let get_module_type ~mode name scope =
    IdTbl.find_name ~modes:(modes_of_mode mode) name scope.module_types

  let rec outer_mod_name ~loc lid =
    let outer_mod_name = outer_mod_name ~loc in
    match lid with
    | Lident name ->
        (name, None)
    | Ldot (lid, name) -> (
      match outer_mod_name lid with
      | outer_name, Some lid ->
          (outer_name, Some (Ldot (lid, name)))
      | outer_name, None ->
          (outer_name, Some (Lident name)) )
    | Lapply (lid1, lid2) -> (
      match outer_mod_name lid1 with
      | outer_name, Some lid ->
          (outer_name, Some (Lapply (lid, lid2)))
      | _, None ->
          (* You can't apply a toplevel module, so we just error out instead. *)
          raise (Error (loc, Unbound_module lid)) )

  let subst s =
    let subst_type_expr = Subst.type_expr s in
    let subst_type_decl = Subst.type_decl s in
    let rec subst
        { kind
        ; path
        ; names
        ; type_variables
        ; type_decls
        ; fields
        ; ctors
        ; modules
        ; module_types
        ; instances
        ; mode } =
      { kind
      ; path
      ; names= IdTbl.map ~f:subst_type_expr names
      ; type_variables
      ; type_decls= IdTbl.map ~f:subst_type_decl type_decls
      ; fields=
          IdTbl.map fields ~f:(fun (decl, i) -> (subst_type_decl decl, i))
      ; ctors= IdTbl.map ctors ~f:(fun (decl, i) -> (subst_type_decl decl, i))
      ; modules= IdTbl.map ~f:subst_scope_or_path modules
      ; module_types= IdTbl.map ~f:subst_scope_or_path module_types
      ; instances
      ; mode }
    and subst_scope_or_path = function
      | Immediate scope ->
          Immediate (subst scope)
      | Deferred _ as deferred ->
          (* TODO: This is not the desired behaviour.. *)
          deferred
    in
    subst

  let build_subst ~type_subst ~module_subst s
      { kind= _
      ; path= _
      ; names= _
      ; type_variables= _
      ; type_decls
      ; fields= _
      ; ctors= _
      ; modules
      ; module_types= _
      ; instances= _
      ; mode= _ } =
    let s =
      IdTbl.fold_keys ~init:s type_decls ~f:(fun s ident ->
          let src_path, dst_path = type_subst ident in
          Subst.with_type src_path dst_path s )
    in
    let s =
      IdTbl.fold_keys ~init:s modules ~f:(fun s ident ->
          let src_path, dst_path = module_subst ident in
          Subst.with_module src_path dst_path s )
    in
    s

  let register_external_module name x resolve_env =
    let x =
      match x with
      | In_flight _ | Deferred _ ->
          x
      | Immediate x ->
          let add_name ident =
            (Path.Pident ident, Path.dot (Pident name) ident)
          in
          let name_subst =
            build_subst Subst.empty x ~type_subst:add_name
              ~module_subst:add_name
          in
          Immediate (subst name_subst x)
    in
    resolve_env.external_modules
    <- IdTbl.add ~key:name ~data:x resolve_env.external_modules

  let load_external_module ~loc filename name resolve_env =
    register_external_module name (In_flight filename) resolve_env ;
    let m = !load_module ~loc ~name:(Ident.name name) resolve_env filename in
    register_external_module name (Immediate m : _ or_deferred) resolve_env ;
    m

  let get_global_module ~mode ~loc resolve_env name =
    match
      IdTbl.find_name ~modes:(modes_of_mode mode) name
        resolve_env.external_modules
    with
    | Some (name, Immediate m) ->
        Some (name, m)
    | Some (name, Deferred filename) ->
        resolve_env.external_modules
        <- IdTbl.add resolve_env.external_modules ~key:name
             ~data:(In_flight filename) ;
        let m =
          !load_module ~loc ~name:(Ident.name name) resolve_env filename
        in
        resolve_env.external_modules
        <- IdTbl.add resolve_env.external_modules ~key:name
             ~data:(Immediate m : _ or_deferred) ;
        Some (name, m)
    | Some (_name, In_flight filename) ->
        raise (Error (loc, Recursive_load filename))
    | None ->
        None

  let rec find_module_ ~mode ~loc ~scopes resolve_env lid scope =
    let open Option.Let_syntax in
    match lid with
    | Lident name ->
        let%map ident, m =
          get_module ~mode ~loc ~scopes resolve_env name scope
        in
        (Path.Pident ident, m)
    | Ldot (path, name) -> (
        let%map path, m =
          find_module_ ~mode ~loc ~scopes resolve_env path scope
        in
        match get_module ~mode ~loc ~scopes resolve_env name m with
        | Some (ident, m) ->
            (Path.dot path ident, m)
        | None ->
            raise (Error (loc, Unbound_module lid)) )
    | Lapply (fpath, path) ->
        let%map fpath, m =
          find_module_ ~mode ~loc ~scopes resolve_env fpath scope
        in
        apply_functor ~mode ~loc ~scopes resolve_env fpath path m

  and apply_functor ~mode ~loc ~scopes resolve_env fpath lid scope =
    let f =
      match scope.kind with
      | Functor f ->
          f
      | _ ->
          raise (Error (loc, Not_a_functor))
    in
    let path, m = find_module ~mode ~loc lid resolve_env scopes in
    (* HACK *)
    let flid = Untype_ast.longident_of_path fpath in
    (Path.Papply (fpath, path), f flid (Immediate m))

  and get_module ~mode ~loc ~scopes resolve_env name scope =
    match IdTbl.find_name ~modes:(modes_of_mode mode) name scope.modules with
    | Some (ident, Immediate m) ->
        Some (ident, m)
    | Some (ident, Deferred lid) ->
        Option.map (find_global_module ~mode ~loc ~scopes resolve_env lid)
          ~f:(fun (_ident, m) -> (ident, m))
    | None ->
        None

  and find_global_module ~mode ~loc ~scopes resolve_env lid =
    let name, lid = outer_mod_name ~loc lid in
    let m = get_global_module ~mode ~loc resolve_env name in
    match (m, lid) with
    | Some (ident, m), Some lid ->
        Option.map (find_module_ ~mode ~loc ~scopes resolve_env lid m)
          ~f:(fun (path, m) -> (Path.add_outer_module ident path, m))
    | Some (ident, m), None ->
        Some (Pident ident, m)
    | None, _ ->
        None

  and find_module ~mode ~loc lid resolve_env scopes =
    match
      List.find_map ~f:(find_module_ ~mode ~loc ~scopes resolve_env lid) scopes
    with
    | Some m ->
        m
    | None -> (
      match find_global_module ~mode ~loc ~scopes resolve_env lid with
      | Some m ->
          m
      | None ->
          raise (Error (loc, Unbound_module lid)) )

  let rec find_module_deferred ~mode ~loc ~scopes resolve_env lid scope =
    let open Option.Let_syntax in
    let modes = modes_of_mode mode in
    match lid with
    | Lident name ->
        let%map ident, m = IdTbl.find_name ~modes name scope.modules in
        (Path.Pident ident, m)
    | Ldot (lid, name) -> (
      match find_module_deferred ~mode ~loc ~scopes resolve_env lid scope with
      | Some (path, Immediate m) -> (
        match IdTbl.find_name ~modes name m.modules with
        | Some (ident, m) ->
            Some (Path.dot path ident, m)
        | None ->
            raise (Error (loc, Unbound_module lid)) )
      | Some (path, Deferred lid) ->
          (* TODO: Rework this. We need to know whether deferred modules are
             Prover or Checked mode.
          *)
          Some (Path.Pdot (path, Checked, name), Deferred (Ldot (lid, name)))
      | None ->
          None )
    | Lapply (lid1, lid2) ->
        (* Don't defer functor applications *)
        let fpath, m_functor =
          find_module ~mode ~loc lid1 resolve_env scopes
        in
        let path, m =
          apply_functor ~mode ~loc ~scopes resolve_env fpath lid2 m_functor
        in
        Some (path, Immediate m)

  let join_expr_scope (expr_scope : t) (scope : t) =
    assert (expr_scope.kind = Expr) ;
    let select_new ~key:_ _ new_value = new_value in
    { scope with
      names=
        IdTbl.merge_skewed_names scope.names expr_scope.names
          ~combine:select_new }
end

let empty_resolve_env : Scope.t resolve_env =
  { type_env= TypeEnvi.empty
  ; external_modules= IdTbl.empty
  ; predeclare_types= false }

type t =
  {scope_stack: Scope.t list; depth: int; resolve_env: Scope.t resolve_env}

let empty resolve_env =
  { scope_stack= [Scope.empty ~mode:Checked None Scope.Module]
  ; depth= 0
  ; resolve_env }

let current_scope {scope_stack; _} =
  match List.hd scope_stack with
  | Some scope ->
      scope
  | None ->
      raise (Error (of_prim __POS__, No_open_scopes))

let push_scope scope env =
  {env with scope_stack= scope :: env.scope_stack; depth= env.depth + 1}

let current_path env = (current_scope env).path

let relative_path env name = join_name (current_path env) name

let current_mode env = (current_scope env).mode

let mode_or_default mode env =
  match mode with Some mode -> mode | None -> current_mode env

let make_functor ~mode path f = Scope.empty ~mode (Some path) (Functor f)

let open_expr_scope ?mode env =
  let mode = mode_or_default mode env in
  push_scope Scope.(empty ~mode (current_path env) Expr) env

let open_module ?mode name env =
  let mode = mode_or_default mode env in
  push_scope Scope.(empty ~mode (Some (relative_path env name)) Module) env

let open_absolute_module ?mode path env =
  let mode = mode_or_default mode env in
  push_scope Scope.(empty ~mode path Module) env

let open_namespace_scope ?mode path scope env =
  let add_name ident = (Path.dot path ident, Path.Pident ident) in
  let subst =
    Scope.build_subst Subst.empty scope ~type_subst:add_name
      ~module_subst:add_name
  in
  let scope = Scope.subst subst scope in
  let mode = mode_or_default mode env in
  env
  |> push_scope {scope with kind= Scope.Open path}
  |> push_scope Scope.(empty ~mode (current_scope env).path Continue)

let open_mode_module_scope mode env =
  push_scope Scope.(empty ~mode (current_scope env).path Continue) env

let pop_scope env =
  match env.scope_stack with
  | [] ->
      raise (Error (of_prim __POS__, No_open_scopes))
  | scope :: scope_stack ->
      (scope, {env with scope_stack; depth= env.depth - 1})

let pop_expr_scope env =
  let scope, env = pop_scope env in
  match scope.Scope.kind with
  | Scope.Expr ->
      (scope, env)
  | _ ->
      raise (Error (of_prim __POS__, Wrong_scope_kind "expression"))

let pop_module ~loc env =
  let rec all_scopes scopes env =
    let scope, env = pop_scope env in
    match scope.kind with
    | Scope.Module ->
        (scope :: scopes, env)
    | Expr ->
        raise (Error (of_prim __POS__, Wrong_scope_kind "module"))
    | Open path ->
        let add_name ident = (Path.Pident ident, Path.dot path ident) in
        let subst =
          Scope.build_subst Subst.empty scope ~type_subst:add_name
            ~module_subst:add_name
        in
        let scopes = List.map ~f:(Scope.subst subst) scopes in
        all_scopes scopes env
    | Continue ->
        all_scopes (scope :: scopes) env
    | Functor _ ->
        if List.is_empty scopes then ([scope], env)
        else raise (Error (of_prim __POS__, Functor_in_module_sig))
  in
  let scopes, env = all_scopes [] env in
  let m =
    match scopes with
    | [] ->
        assert false
    | hd :: scopes ->
        List.fold_left ~init:hd scopes ~f:(Scope.join ~loc)
  in
  (m, env)

let close_expr_scope env = snd (pop_expr_scope env)

let set_type_predeclaring env = env.resolve_env.predeclare_types <- true

let unset_type_predeclaring env =
  if IdTbl.is_empty env.resolve_env.type_env.predeclared_types then
    env.resolve_env.predeclare_types <- false
  else
    let _, (_, _, loc) =
      IdTbl.first_exn env.resolve_env.type_env.predeclared_types
    in
    let predeclared = IdTbl.keys env.resolve_env.type_env.predeclared_types in
    raise (Error (loc, Predeclared_types predeclared))

let map_current_scope ~f env =
  match env.scope_stack with
  | current_scope :: scope_stack ->
      {env with scope_stack= f current_scope :: scope_stack}
  | [] ->
      raise (Error (of_prim __POS__, No_open_scopes))

let set_path path = map_current_scope ~f:(Scope.set_path path)

let add_type_variable name typ =
  map_current_scope ~f:(Scope.add_type_variable name typ)

let find_type_variable name env =
  List.find_map ~f:(Scope.find_type_variable name) env.scope_stack

let add_module (name : Ident.t) m =
  let add_name ident = (Path.Pident ident, Path.dot (Pident name) ident) in
  let subst =
    Scope.build_subst Subst.empty m ~type_subst:add_name ~module_subst:add_name
  in
  let m = Scope.subst subst m in
  map_current_scope ~f:(fun scope ->
      let scope = Scope.add_module name (Scope.Immediate m) scope in
      { scope with
        instances=
          Map.merge scope.instances m.instances ~f:(fun ~key:_ data ->
              match data with
              | `Left x ->
                  Some x
              | `Both (_, x) | `Right x ->
                  Some (Path.add_outer_module name x) ) } )

let add_deferred_module (name : Ident.t) lid =
  map_current_scope ~f:(Scope.add_module name (Scope.Deferred lid))

let register_external_module name x env =
  Scope.register_external_module name x env.resolve_env

let find_module ~loc (lid : lid) env =
  Scope.find_module ~loc lid.txt env.resolve_env env.scope_stack

let find_module_deferred ~mode ~loc (lid : lid) env =
  match
    List.find_map
      ~f:
        (Scope.find_module_deferred ~mode ~loc ~scopes:env.scope_stack
           env.resolve_env lid.txt)
      env.scope_stack
  with
  | Some m ->
      Some m
  | None -> (
    match lid.txt with
    | Lident name -> (
      match
        IdTbl.find_name name ~modes:(modes_of_mode mode)
          env.resolve_env.external_modules
      with
      | Some (ident, _) ->
          Some (Pident ident, Deferred lid.txt)
      | None ->
          None )
    | _ ->
        let path, m = find_module ~mode ~loc lid env in
        Some (path, Immediate m) )

let add_implicit_instance name typ env =
  let path = Path.Pident name in
  let id, type_env = TypeEnvi.next_instance_id env.resolve_env.type_env in
  let env =
    map_current_scope env ~f:(fun scope ->
        {scope with instances= Map.set ~key:id ~data:path scope.instances} )
  in
  env.resolve_env.type_env <- TypeEnvi.add_implicit_instance id typ type_env ;
  env

let find_of_lident ~mode ~kind ~get_name (lid : lid) env =
  let open Option.Let_syntax in
  let loc = lid.loc in
  let full_get_name =
    match lid.txt with
    | Lident name ->
        fun scope ->
          let%map ident, data = get_name ~mode name scope in
          (Path.Pident ident, data)
    | Ldot (path, name) -> (
        fun scope ->
          let%map path, m =
            Scope.find_module_ ~mode ~loc ~scopes:env.scope_stack
              env.resolve_env path scope
          in
          match get_name ~mode name m with
          | Some (ident, data) ->
              (Path.dot path ident, data)
          | None ->
              raise (Error (loc, Unbound (kind, lid.txt))) )
    | Lapply _ ->
        raise (Error (loc, Lident_unhandled (kind, lid.txt)))
  in
  match List.find_map ~f:full_get_name env.scope_stack with
  | Some v ->
      Some v
  | None -> (
    match lid.txt with
    | Ldot (path, name) ->
        let%bind path, m =
          Scope.find_global_module ~mode ~loc ~scopes:env.scope_stack
            env.resolve_env path
        in
        let%map ident, data = get_name ~mode name m in
        (Path.dot path ident, data)
    | _ ->
        None )

let join_expr_scope env expr_scope =
  map_current_scope ~f:(Scope.join_expr_scope expr_scope) env

let raw_find_type_declaration ~mode (lid : lid) env =
  let modes = modes_of_mode mode in
  match
    find_of_lident ~mode ~kind:"type" ~get_name:Scope.find_type_declaration lid
      env
  with
  | Some v ->
      v
  | None -> (
    match lid.txt with
    | Lident name when env.resolve_env.predeclare_types ->
        let {type_env; _} = env.resolve_env in
        let ident, id, num_args =
          match IdTbl.find_name ~modes name type_env.predeclared_types with
          | Some (ident, (id, num_args, _loc)) ->
              (ident, id, num_args)
          | None ->
              let id, type_env = TypeEnvi.next_decl_id type_env in
              let num_args = ref None in
              let ident = Ident.create ~mode name in
              let type_env =
                { type_env with
                  predeclared_types=
                    IdTbl.add ~key:ident
                      ~data:(id, ref None, lid.loc)
                      type_env.predeclared_types }
              in
              env.resolve_env.type_env <- type_env ;
              (ident, id, num_args)
        in
        ( Pident ident
        , { tdec_ident= ident
          ; tdec_params= []
          ; tdec_desc= TForward num_args
          ; tdec_id= id } )
    | _ ->
        raise (Error (lid.loc, Unbound_type lid.txt)) )

let add_module_type name m =
  map_current_scope ~f:(Scope.add_module_type name m)

let find_module_type =
  find_of_lident ~kind:"module type" ~get_name:Scope.get_module_type

module Type = struct
  type env = t

  let mk type_desc env = Type1.mk env.depth type_desc

  let mkvar name env = Type1.mkvar env.depth name

  let map_env ~f env = env.resolve_env.type_env <- f env.resolve_env.type_env

  let refresh_var ~loc ?must_find env typ =
    match typ.type_desc with
    | Tvar None -> (
      match must_find with
      | Some true ->
          raise (Error (loc, Unbound_type_var typ))
      | _ ->
          (env, mkvar None env) )
    | Tvar (Some x as name) -> (
        let var =
          match must_find with
          | Some true ->
              let var = find_type_variable x env in
              if Option.is_none var then
                raise (Error (loc, Unbound_type_var typ)) ;
              var
          | Some false ->
              None
          | None ->
              find_type_variable x env
        in
        match var with
        | Some var ->
            (env, var)
        | None ->
            let var = mkvar name env in
            (add_type_variable x var env, var) )
    | _ ->
        raise (Error (loc, Expected_type_var typ))

  (** Replace the representatives of each of list of variables with a fresh
      variable.

      The old values can be restored by taking a snapshot before calling this
      and backtracking to it once the new values have been used.
  *)
  let refresh_vars vars env =
    List.iter vars ~f:(fun var ->
        (* Sanity check. *)
        (match var.type_desc with Tvar _ -> () | _ -> assert false) ;
        set_repr var (mkvar None env) )

  let copy typ env =
    let rec copy typ =
      let typ = repr typ in
      match typ.type_desc with
      | Tvar _ ->
          (* Don't copy variables! *)
          typ
      | Tpoly _ ->
          (* Tpoly should only ever appear at the top level of a type. *)
          assert false
      | _ ->
          mk (copy_desc ~f:copy typ.type_desc) env
    in
    let typ = repr typ in
    let snap = Snapshot.create () in
    let typ =
      match typ.type_desc with
      | Tpoly (vars, typ) ->
          (* Make fresh variables to instantiate [Tpoly]s. *)
          refresh_vars vars env ; copy typ
      | _ ->
          copy typ
    in
    (* Restore the values of 'refreshed' variables. *)
    backtrack snap ; typ

  (** [instantiate params typs typ env] creates a new type by replacing the
      each of the type variables in [params] with the corresponding instance
      from [typs] in [typ].
  *)
  let instantiate params typs typ env =
    let snap = Snapshot.create () in
    List.iter2_exn params typs ~f:(fun param typ ->
        (* Sanity check. *)
        (match param.type_desc with Tvar _ -> () | _ -> assert false) ;
        set_repr param typ ) ;
    let typ = copy typ env in
    (* Restore the original values of the parameters. *)
    backtrack snap ; typ

  module T = struct
    type t = type_expr

    let compare typ1 typ2 = Int.compare typ1.type_id typ2.type_id

    let sexp_of_t typ = Int.sexp_of_t typ.type_id
  end

  let rec update_depths env typ =
    Type1.update_depth env.depth typ ;
    Type1.iter ~f:(update_depths env) typ

  let or_compare cmp ~f = if Int.equal cmp 0 then f () else cmp

  let rec compare typ1 typ2 =
    if Int.equal typ1.type_id typ2.type_id then 0
    else
      match ((repr typ1).type_desc, (repr typ2).type_desc) with
      | Tref _, _ | _, Tref _ ->
          assert false
      | Tpoly (_, typ1), _ ->
          compare typ1 typ2
      | _, Tpoly (_, typ2) ->
          compare typ1 typ2
      | Tvar _, Tvar _ ->
          Int.compare typ1.type_id typ2.type_id
      | Tvar _, _ ->
          -1
      | _, Tvar _ ->
          1
      | ( Tctor {var_decl= {tdec_id= id1; _}; var_params= params1; _}
        , Tctor {var_decl= {tdec_id= id2; _}; var_params= params2; _} ) ->
          or_compare (Int.compare id1 id2) ~f:(fun () ->
              compare_all params1 params2 )
      | Tctor _, _ ->
          -1
      | _, Tctor _ ->
          1
      | Ttuple typs1, Ttuple typs2 ->
          compare_all typs1 typs2
      | Ttuple _, _ ->
          -1
      | _, Ttuple _ ->
          1
      | ( Tarrow (typ1a, typ1b, Explicit, label1)
        , Tarrow (typ2a, typ2b, Explicit, label2) )
      | ( Tarrow (typ1a, typ1b, Implicit, label1)
        , Tarrow (typ2a, typ2b, Implicit, label2) ) ->
          or_compare (compare_arg_label label1 label2) ~f:(fun () ->
              or_compare (compare typ1a typ2a) ~f:(fun () ->
                  compare typ1b typ2b ) )
      | Tarrow (_, _, Explicit, _), _ ->
          -1
      | _, Tarrow (_, _, Explicit, _) ->
          1

  and compare_all typs1 typs2 =
    match (typs1, typs2) with
    | [], [] ->
        0
    | [], _ ->
        -1
    | _, [] ->
        1
    | typ1 :: typs1, typ2 :: typs2 ->
        or_compare (compare typ1 typ2) ~f:(fun () -> compare_all typs1 typs2)

  let rec weak_variables depth set typ =
    match typ.type_desc with
    | Tvar _ when typ.type_depth > depth ->
        Set.add set typ
    | _ ->
        Type1.fold ~init:set ~f:(weak_variables depth) typ

  let rec get_implicits acc typ =
    match typ.type_desc with
    | Tarrow (typ1, typ2, Implicit, label) ->
        get_implicits ((label, typ1) :: acc) typ2
    | _ ->
        (List.rev acc, typ)

  let new_implicit_var ?(loc = Location.none) typ env =
    let mode = current_mode env in
    let {TypeEnvi.implicit_vars; implicit_id; _} = env.resolve_env.type_env in
    let mk exp_loc exp_desc = {Typedast.exp_loc; exp_desc; exp_type= typ} in
    let name =
      Location.mkloc
        (Ident.create ~mode (sprintf "__implicit%i__" implicit_id))
        loc
    in
    let new_exp =
      mk loc (Texp_unifiable {expression= None; name; id= implicit_id})
    in
    env.resolve_env.type_env
    <- { env.resolve_env.type_env with
         implicit_vars= new_exp :: implicit_vars
       ; implicit_id= implicit_id + 1 } ;
    new_exp

  let implicit_instances ~(unifies : env -> type_expr -> type_expr -> bool)
      (typ : type_expr) typ_vars env =
    List.filter_map env.resolve_env.type_env.instances
      ~f:(fun (id, instance_typ) ->
        let instance_typ = copy instance_typ env in
        let snapshot = Snapshot.create () in
        if unifies env typ instance_typ then
          if
            Set.exists typ_vars ~f:(fun var -> not (phys_equal var (repr var)))
          then (
            (* There is at least one variable that hasn't been instantiated.
               In particular, this must mean that it was less general than the
               variable that it unified with, or that a parent type expression
               instantiated a type variable in the target type, and so this
               instance isn't general enough to satisfy the target type.
            *)
            backtrack snapshot ;
            None )
          else
            List.find_map env.scope_stack ~f:(fun {instances; _} ->
                Option.map (Map.find instances id) ~f:(fun path ->
                    (path, instance_typ) ) )
        else None )

  let generate_implicits e env =
    let loc = e.Typedast.exp_loc in
    let implicits, typ = get_implicits [] e.exp_type in
    match implicits with
    | [] ->
        e
    | _ ->
        let es =
          List.map implicits ~f:(fun (label, typ) ->
              (label, new_implicit_var ~loc typ env) )
        in
        {exp_loc= loc; exp_type= typ; exp_desc= Texp_apply (e, es)}

  let rec instantiate_implicits ~loc ~unifies implicit_vars env =
    let env_implicits = env.resolve_env.type_env.implicit_vars in
    env.resolve_env.type_env
    <- {env.resolve_env.type_env with implicit_vars= []} ;
    let implicit_vars =
      List.filter implicit_vars
        ~f:(fun ({Typedast.exp_loc; exp_type; _} as exp) ->
          let exp_type = flatten exp_type in
          let typ_vars = type_vars exp_type in
          match implicit_instances ~unifies exp_type typ_vars env with
          | [(name, instance_typ)] ->
              let name = Location.mkloc name exp_loc in
              let e =
                generate_implicits
                  { exp_loc
                  ; exp_type= instance_typ
                  ; exp_desc= Texp_variable name }
                  env
              in
              ( match exp.exp_desc with
              | Texp_unifiable desc ->
                  desc.expression <- Some e
              | _ ->
                  raise (Error (exp.exp_loc, No_unifiable_implicit)) ) ;
              false
          | [] ->
              true
          | _ ->
              raise (Error (exp_loc, Multiple_instances exp_type)) )
    in
    let new_implicits = env.resolve_env.type_env.implicit_vars in
    env.resolve_env.type_env
    <- {env.resolve_env.type_env with implicit_vars= env_implicits} ;
    match new_implicits with
    | [] ->
        implicit_vars
    | _ ->
        instantiate_implicits ~loc ~unifies (new_implicits @ implicit_vars) env

  let flattened_implicit_vars ~loc ~toplevel ~unifies typ_vars env =
    let unifies env typ ctyp = unifies env typ (snd (get_implicits [] ctyp)) in
    let {TypeEnvi.implicit_vars; _} = env.resolve_env.type_env in
    let implicit_vars =
      List.map implicit_vars ~f:(fun exp ->
          {exp with exp_type= flatten exp.exp_type} )
    in
    let implicit_vars =
      instantiate_implicits ~loc ~unifies implicit_vars env
    in
    let implicit_vars =
      List.dedup_and_sort implicit_vars ~compare:(fun exp1 exp2 ->
          let cmp = compare exp1.Typedast.exp_type exp2.Typedast.exp_type in
          ( if Int.equal cmp 0 then
            match (exp1.exp_desc, exp2.exp_desc) with
            | Texp_unifiable desc1, Texp_unifiable desc2 ->
                if desc1.id < desc2.id then desc2.expression <- Some exp1
                else desc1.expression <- Some exp2
            | _ ->
                raise (Error (exp2.exp_loc, No_unifiable_implicit)) ) ;
          cmp )
    in
    let implicit_vars =
      (* Eliminate unifiable implicit variables containing 'weak type
         variables'. *)
      let consider_weak = ref true in
      let weak_vars_set = ref Typeset.empty in
      let strong_implicit_vars, weak_implicit_vars =
        List.partition_tf implicit_vars ~f:(fun {exp_type; _} ->
            if !consider_weak then
              let weak_vars =
                weak_variables env.depth Typeset.empty exp_type
              in
              if Set.is_empty weak_vars then true
              else (
                if Set.is_empty (Set.inter !weak_vars_set weak_vars) then
                  weak_vars_set := Set.union !weak_vars_set weak_vars
                else
                  (* Several implicit variables contain the same weak type
                     variable, so we give up on eliminating variables.

                     This avoids an expensive proof search for a dependent-type
                     witness of the form:
                       forall (T1, ..., Tn : Type -> Type),
                         exists (A1, ..., An : Type),
                         T1(A1, ..., An) + ... + Tn(A1, ..., An).
                *)
                  consider_weak := false ;
                false )
            else true )
      in
      if !consider_weak then
        let weak_implicit_vars =
          List.filter weak_implicit_vars ~f:(fun e_weak ->
              not
                (List.exists strong_implicit_vars ~f:(fun e_strong ->
                     if
                       Type1.equal_at_depth ~depth:env.depth e_weak.exp_type
                         e_strong.exp_type
                     then (
                       ignore (unifies env e_strong.exp_type e_weak.exp_type) ;
                       ( match e_weak.exp_desc with
                       | Texp_unifiable desc ->
                           desc.expression <- Some e_strong
                       | _ ->
                           raise
                             (Error (e_weak.exp_loc, No_unifiable_implicit)) ) ;
                       true )
                     else false )) )
        in
        strong_implicit_vars @ weak_implicit_vars
      else implicit_vars
    in
    let local_implicit_vars, implicit_vars =
      if toplevel then (implicit_vars, [])
      else
        List.partition_tf implicit_vars ~f:(fun {exp_type; _} ->
            let exp_vars = type_vars exp_type in
            let instantiated_vars = Set.inter exp_vars typ_vars in
            not (Set.is_empty instantiated_vars) )
    in
    env.resolve_env.type_env <- {env.resolve_env.type_env with implicit_vars} ;
    local_implicit_vars
end

module TypeDecl = struct
  let next_id env =
    let tdec_id, type_env = TypeEnvi.next_decl_id env.resolve_env.type_env in
    env.resolve_env.type_env <- type_env ;
    tdec_id

  let mk = Type1.Decl.mk

  let mk_typ ~params ?ident decl env =
    Type1.Decl.mk_typ ~params ?ident env.depth decl

  (* NOTE: the unused parameters here will be used when [var_decl] is removed
     from [variant] and we need to search through the environment for the type
     based on its path.
  *)
  let find_of_variant ~loc:_ variant _env = variant.var_decl

  let find_of_field (field : lid) env =
    find_of_lident ~kind:"field" ~get_name:Scope.find_field field env

  let find_of_constructor (ctor : lid) env =
    find_of_lident ~kind:"constructor" ~get_name:Scope.find_ctor ctor env

  let unfold_alias_aux ~loc typ env =
    match (repr typ).type_desc with
    | Tctor variant -> (
      match find_of_variant ~loc variant env with
      | {tdec_desc= TAlias alias_typ; tdec_params; _} as desc ->
          (* NOTE: Ignoring tdec_implicit_params; they are never used and
               will be removed.
            *)
          Some
            ( desc
            , Some
                (Type.instantiate tdec_params variant.var_params alias_typ env)
            )
      | desc ->
          Some (desc, None) )
    | _ ->
        None

  let unfold_alias ~loc typ env =
    match unfold_alias_aux ~loc typ env with
    | Some (_desc, typ) ->
        typ
    | None ->
        None

  let rec find_unaliased_of_type ~loc typ env =
    match unfold_alias_aux ~loc typ env with
    | Some (_desc, Some typ) ->
        find_unaliased_of_type ~loc typ env
    | Some (desc, None) ->
        Some (desc, typ)
    | None ->
        None
end

let add_name name typ = map_current_scope ~f:(Scope.add_name name typ)

let get_name ~mode (name : str) env =
  let loc = name.loc in
  match List.find_map ~f:(Scope.find_name ~mode name.txt) env.scope_stack with
  | Some (ident, typ) ->
      (ident, Type.copy typ env)
  | None ->
      raise (Error (loc, Unbound_value (Lident name.txt)))

let find_name ~mode (lid : lid) env =
  match
    find_of_lident ~mode ~kind:"name" ~get_name:Scope.find_name lid env
  with
  | Some (ident, typ) ->
      (ident, Type.copy typ env)
  | None ->
      raise (Error (lid.loc, Unbound_value lid.txt))

(* Error handling *)

open Format

let pp_typ = Typeprint.type_expr

let pp_decl_typ ppf decl =
  pp_typ ppf
    { type_desc=
        Tctor
          { var_ident= Pident decl.tdec_ident
          ; var_params= decl.tdec_params
          ; var_decl= decl }
    ; type_id= -1
    ; type_depth= -1 }

let report_error ppf = function
  | No_open_scopes ->
      fprintf ppf "Internal error: There is no current open scope."
  | Wrong_scope_kind kind ->
      fprintf ppf
        "Internal error: Expected the current scope to be a %s scope." kind
  | Multiple_definition (kind, name) ->
      fprintf ppf "@[<hov>Multiple definition of the %s name@ %s@]" kind name
  | Unbound_type_var var ->
      fprintf ppf "@[<hov>Unbound type parameter@ @[<h>%a@].@]" pp_typ var
  | Unbound_type lid ->
      fprintf ppf "@[<hov>Unbound type constructor@ @[<h>%a@].@]" Longident.pp
        lid
  | Unbound_module lid ->
      fprintf ppf "@[<hov>Unbound module @[<h>%a@].@]" Longident.pp lid
  | Unbound_value lid ->
      fprintf ppf "@[<hov>Unbound value @[<h>%a@].@]" Longident.pp lid
  | Unbound (kind, lid) ->
      fprintf ppf "@[<hov>Unbound %s @[<h>%a@].@]" kind Longident.pp lid
  | Wrong_number_args (path, given, expected) ->
      fprintf ppf
        "@[The type constructor @[<h>%a@] expects %d argument(s)@ but is here \
         applied to %d argument(s).@]"
        Path.pp path expected given
  | Expected_type_var typ ->
      fprintf ppf
        "@[<hov>Syntax error: Expected a type parameter, but got @[<h>%a@].@]"
        pp_typ typ
  | Lident_unhandled (kind, lid) ->
      fprintf ppf "@[<hov>Don't know how to find %s @[<h>%a@].@]" kind
        Longident.pp lid
  | Constraints_not_satisfied (typ, decl) ->
      fprintf ppf
        "@[<hov>Constraints are not satisfied in this type.@ Type @[<h>%a@] \
         should be an instance of @[<h>%a@].@]"
        pp_typ typ pp_decl_typ decl
  | No_unifiable_implicit ->
      fprintf ppf "Internal error: Implicit variable is not unifiable."
  | Multiple_instances typ ->
      fprintf ppf
        "@[<hov>Multiple instances were found satisfying @[<h>%a@],@ could \
         not decide between them.@]"
        pp_typ typ
  | Recursive_load filename ->
      fprintf ppf
        "@[<hov>Circular dependency found; tried to re-load @[<h>%s@]@]"
        filename
  | Predeclared_types types ->
      fprintf ppf "@[<hov>Could not find declarations for some types:@]@;%a@"
        (pp_print_list ~pp_sep:pp_print_space Ident.pprint)
        types
  | Functor_in_module_sig ->
      fprintf ppf
        "Internal error: Bare functor found as part of a module signature."
  | Not_a_functor ->
      fprintf ppf "This module is not a functor."

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
    | _ ->
        None )
