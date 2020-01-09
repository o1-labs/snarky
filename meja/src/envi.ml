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
  | Unbound_path of string * Path.t
  | Wrong_number_args of Path.t * int * int
  | Expected_type_var of type_expr
  | Lident_unhandled of string * Longident.t
  | Constraints_not_satisfied of type_expr * type_decl
  | No_unifiable_implicit
  | Recursive_load of string
  | Predeclared_types of Ident.t list
  | Functor_in_module_sig
  | Not_a_functor
  | Duplicate_name of string * bool * string
  | Shadowed_checked of string * Longident.t

exception Error of Location.t * error

module TypeEnvi = struct
  type t =
    { implicit_vars: Typedast.expression list
    ; implicit_id: int
    ; predeclared_types: int IdTbl.t }

  let empty =
    {implicit_id= 1; implicit_vars= []; predeclared_types= IdTbl.empty}
end

type 'a or_deferred =
  | Immediate of 'a
  | Deferred of string
  | In_flight of string

type 'a resolve_env =
  { mutable type_env: TypeEnvi.t
  ; mutable external_modules: 'a or_deferred IdTbl.t }

module Shadows = struct
  (** A register of names already present in the current module.
      Used to reject duplicate names that would shadow existing path-dependent
      items.
  *)
  type t =
    { types: Ident.t String.Map.t
    ; modules: String.Set.t
    ; module_types: String.Set.t }

  let empty =
    { types= String.Map.empty
    ; modules= String.Set.empty
    ; module_types= String.Set.empty }
end

(** Rejecting mode checker. If the mode is checked but a prover-mode name is
    found first, raise an error complaining about shadowing.
    When in prover mode, this accepts names from either mode.
*)
let reject_shadowed ~loc kind name = function
  | Checked -> (
      function
      | Checked ->
          true
      | Prover ->
          raise (Error (loc, Shadowed_checked (kind, name))) )
  | Prover ->
      fun _ -> true

module Scope = struct
  type 'a or_path = Immediate of 'a | Deferred of string

  type 't kind =
    | Module of Shadows.t ref
    | Expr
    | Open of Path.t
    | Open_instance of Path.t
    | Continue of Shadows.t ref
    | Functor of ('t or_path -> 't)

  type t =
    { kind: t kind
    ; names: type_expr IdTbl.t
    ; type_variables: type_expr String.Map.t
    ; type_decls: type_decl IdTbl.t
    ; fields: (type_decl * int) IdTbl.t
    ; ctors: (type_decl * int) IdTbl.t
    ; modules: t or_path IdTbl.t
    ; module_types: t or_path IdTbl.t
    ; instances: (Path.t * Path.t * type_expr) list
          (* First path is relative to the scope, second is relative to the
             current scope.
          *)
    ; mode: mode }

  let load_module :
      (loc:Location.t -> name:string -> t resolve_env -> string -> t) ref =
    ref (fun ~loc ~name _env _filename ->
        raise (Error (loc, Unbound_module (Lident name))) )

  let empty ~mode kind =
    { kind
    ; names= IdTbl.empty
    ; type_variables= String.Map.empty
    ; type_decls= IdTbl.empty
    ; fields= IdTbl.empty
    ; ctors= IdTbl.empty
    ; modules= IdTbl.empty
    ; module_types= IdTbl.empty
    ; instances= []
    ; mode }

  let get_shadow {kind; _} =
    match kind with
    | Module shadow | Continue shadow ->
        shadow
    | _ ->
        raise (Error (of_prim __POS__, Wrong_scope_kind "module"))

  let add_name key typ scope =
    {scope with names= IdTbl.add scope.names ~key ~data:typ}

  let get_name name {names; _} = IdTbl.find name names

  let find_name ~modes name {names; _} = IdTbl.find_name ~modes name names

  let add_type_variable key typ scope =
    {scope with type_variables= Map.set scope.type_variables ~key ~data:typ}

  let find_type_variable ~mode name scope =
    Option.map ~f:(Type1.get_mode mode) (Map.find scope.type_variables name)

  let add_field decl index scope field_decl =
    { scope with
      fields=
        IdTbl.add scope.fields ~key:field_decl.fld_ident ~data:(decl, index) }

  let get_field ident scope = IdTbl.find ident scope.fields

  let find_field ~mode ~modes:_ name scope =
    IdTbl.find_name ~modes:(modes_of_mode mode) name scope.fields

  let add_ctor decl index scope ctor_decl =
    { scope with
      ctors= IdTbl.add scope.ctors ~key:ctor_decl.ctor_ident ~data:(decl, index)
    }

  let get_ctor name scope = IdTbl.find name scope.ctors

  let find_ctor ~mode ~modes:_ name scope =
    IdTbl.find_name ~modes:(modes_of_mode mode) name scope.ctors

  let add_type_declaration ~loc ?(may_shadow = false) ident decl scope =
    ( if not may_shadow then
      (* The types for type constructor record arguments may shadow because
         they are always unambiguous.
         This also circumvents an error when newtypes are defined where there
         is no shadowing scope.
      *)
      let shadow = get_shadow scope in
      let name = Ident.ocaml_name ident in
      match Map.find !shadow.types name with
      | Some ident' ->
          (* If the identifiers are equal, we are defining a recursive type,
             which should be allowed.
          *)
          if not (Ident.equal ident ident') then
            let generated = Option.is_some (Ident.ocaml_name_ref ident') in
            raise (Error (loc, Duplicate_name ("type", generated, name)))
      | None ->
          shadow :=
            {!shadow with types= Map.set !shadow.types ~key:name ~data:ident}
    ) ;
    {scope with type_decls= IdTbl.add scope.type_decls ~key:ident ~data:decl}

  let get_type_declaration name scope = IdTbl.find name scope.type_decls

  let find_type_declaration ~mode ~modes:_ name scope =
    IdTbl.find_name ~modes:(modes_of_mode mode) name scope.type_decls

  let register_type_declaration ~loc ?may_shadow ident decl scope =
    let scope =
      match decl.tdec_desc with
      | TExtend _ ->
          (* Don't re-register this type. *)
          scope
      | _ ->
          add_type_declaration ~loc ?may_shadow ident decl scope
    in
    match decl.tdec_desc with
    | TAbstract | TAlias _ | TOpen ->
        scope
    | TRecord fields ->
        List.foldi ~f:(add_field decl) ~init:scope fields
    | TVariant ctors ->
        List.foldi ~f:(add_ctor decl) ~init:scope ctors
    | TExtend (_, ctors) ->
        List.foldi ~f:(add_ctor decl) ~init:scope ctors

  let fold_over ~init:acc ~names ~type_variables ~type_decls ~fields ~ctors
      ~modules ~module_types ~instances
      { kind= _
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
    let acc = instances acc instances1 instances2 in
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
      ; names= names2
      ; type_variables= type_variables2
      ; type_decls= type_decls2
      ; fields= fields2
      ; ctors= ctors2
      ; modules= modules2
      ; module_types= module_types2
      ; instances= instances2
      ; mode= mode2 } =
    let multiple_definition kind ~key (ident1, _) ((ident2, _) as v) =
      (* Don't consider the same name in different modes to be the same. *)
      if Ident.mode ident1 = Ident.mode ident2 then
        raise (Error (loc, Multiple_definition (kind, key))) ;
      v
    in
    { kind
    ; names=
        IdTbl.merge_skewed_names names1 names2 ~combine:(fun ~key:_ _ v -> v)
    ; type_variables=
        Map.merge_skewed type_variables1 type_variables2
          ~combine:(fun ~key:_ _ v -> v)
    ; type_decls=
        IdTbl.merge_skewed_names type_decls1 type_decls2
          ~combine:(multiple_definition "type")
    ; fields=
        IdTbl.merge_skewed_names fields1 fields2 ~combine:(fun ~key:_ _ v -> v)
    ; ctors=
        IdTbl.merge_skewed_names ctors1 ctors2 ~combine:(fun ~key:_ _ v -> v)
    ; modules=
        IdTbl.merge_skewed_names modules1 modules2
          ~combine:(multiple_definition "module")
    ; module_types=
        IdTbl.merge_skewed_names module_types1 module_types2
          ~combine:(multiple_definition "module type")
    ; instances= instances2 @ instances1
    ; mode= weakest_mode mode1 mode2 }

  let add_module ~loc ident m scope =
    let shadow = get_shadow scope in
    let name = Ident.name ident in
    if Set.mem !shadow.modules name then
      raise (Error (loc, Duplicate_name ("module", false, name)))
    else shadow := {!shadow with modules= Set.add !shadow.modules name} ;
    {scope with modules= IdTbl.add scope.modules ~key:ident ~data:m}

  let add_module_type ~loc ident m scope =
    let shadow = get_shadow scope in
    let name = Ident.name ident in
    if Set.mem !shadow.module_types name then
      raise (Error (loc, Duplicate_name ("module type", false, name)))
    else
      shadow := {!shadow with module_types= Set.add !shadow.module_types name} ;
    {scope with module_types= IdTbl.add scope.module_types ~key:ident ~data:m}

  let get_module_type ~modes name scope =
    IdTbl.find_name ~modes name scope.module_types

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
      ; names= IdTbl.map ~f:subst_type_expr names
      ; type_variables
      ; type_decls= IdTbl.map ~f:subst_type_decl type_decls
      ; fields=
          IdTbl.map fields ~f:(fun (decl, i) -> (subst_type_decl decl, i))
      ; ctors= IdTbl.map ctors ~f:(fun (decl, i) -> (subst_type_decl decl, i))
      ; modules= IdTbl.map ~f:subst_scope_or_path modules
      ; module_types= IdTbl.map ~f:subst_scope_or_path module_types
      ; instances=
          List.map instances ~f:(fun (local_path, global_path, typ) ->
              ( local_path
              , Subst.expression_path s global_path
              , subst_type_expr typ ) )
      ; mode }
    and subst_scope_or_path = function
      | Immediate scope ->
          Immediate (subst scope)
      | Deferred _ as deferred ->
          (* TODO: This is not the desired behaviour.. *)
          deferred
    in
    fun scope ->
      let snap = Snapshot.create () in
      let scope = subst scope in
      backtrack_replace snap ; scope

  let build_subst ~type_subst ~module_subst ~expr_subst s
      { kind= _
      ; names= _
      ; type_variables= _
      ; type_decls
      ; fields= _
      ; ctors= _
      ; modules
      ; module_types= _
      ; instances
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
    let s =
      List.fold ~init:s instances ~f:(fun s (local_path, global_path, _) ->
          let src_path, dst_path = expr_subst ~local_path global_path in
          Subst.with_expression src_path dst_path s )
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
          let add_outer_module ~local_path:_ path =
            (path, Path.add_outer_module name path)
          in
          let name_subst =
            build_subst Subst.empty x ~type_subst:add_name
              ~module_subst:add_name ~expr_subst:add_outer_module
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
      IdTbl.find_name
        ~modes:(reject_shadowed ~loc "module" (Longident.Lident name) mode)
        name resolve_env.external_modules
    with
    | Some (name, Immediate m) ->
        Some (name, m)
    | Some (name, Deferred filename) ->
        let m = load_external_module ~loc filename name resolve_env in
        Some (name, m)
    | Some (_name, In_flight filename) ->
        raise (Error (loc, Recursive_load filename))
    | None ->
        None

  let rec find_module_ ~mode ~loc ~scopes resolve_env lid scope =
    let open Option.Let_syntax in
    let modes = reject_shadowed ~loc "module" lid mode in
    match lid with
    | Lident name ->
        let%map ident, m =
          get_module ~modes ~loc ~scopes resolve_env name scope
        in
        (Path.Pident ident, m)
    | Ldot (path, name) -> (
        let%map path, m =
          find_module_ ~mode ~loc ~scopes resolve_env path scope
        in
        match get_module ~modes ~loc ~scopes resolve_env name m with
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
    (Path.Papply (fpath, path), f (Immediate m))

  and get_module ~modes ~loc ~scopes resolve_env name scope =
    match IdTbl.find_name ~modes name scope.modules with
    | Some (ident, Immediate m) ->
        Some (ident, m)
    | Some (ident, Deferred lid) ->
        Option.map
          (find_global_module ~mode:Checked ~loc ~scopes resolve_env
             (Lident lid)) ~f:(fun (_ident, m) -> (ident, m))
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

  let find_module_deferred ~mode ~loc ~scopes resolve_env lid scope =
    let open Option.Let_syntax in
    let modes = reject_shadowed ~loc "module" lid mode in
    match lid with
    | Lident name ->
        let%map ident, m = IdTbl.find_name ~modes name scope.modules in
        (Path.Pident ident, m)
    | Ldot (lid, name) -> (
      match find_module_ ~mode ~loc ~scopes resolve_env lid scope with
      | Some (path, m) -> (
        match IdTbl.find_name ~modes name m.modules with
        | Some (ident, m) ->
            Some (Path.dot path ident, m)
        | None ->
            raise (Error (loc, Unbound_module lid)) )
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

  (** Get the module referred to by the given [Ident.t].

      This skips over any modules which are not immediate. As such, this should
      only be used to retrieve sub-values of the module, which must have forced
      the loading of the module at their creation time.
  *)
  let get_module_by_ident ~loc resolve_env (ident : Ident.t) (scope : t) =
    match IdTbl.find ident scope.modules with
    | Some (Immediate m) ->
        Some m
    | Some (Deferred path) ->
        (* Use Prover mode so that we pick up all the modules. *)
        Option.map ~f:snd
          (get_global_module ~mode:Prover ~loc resolve_env path)
    | _ ->
        None

  (** Get the module referred to by the given name for the given mode.

      This skips over any modules which are not immediate. As such, this should
      only be used to retrieve sub-values of the module, which must have forced
      the loading of the module at their creation time.
  *)
  let get_module_no_load ~loc resolve_env ~modes name scope =
    match IdTbl.find_name name ~modes scope.modules with
    | Some (ident, Immediate m) ->
        Some (ident, m)
    | Some (ident, Deferred path) ->
        (* Use Prover mode so that we pick up all the modules. *)
        Option.map (get_global_module ~mode:Prover ~loc resolve_env path)
          ~f:(fun (_, m) -> (ident, m))
    | _ ->
        None

  let global {external_modules; _} =
    { (empty ~mode:Checked (Module (ref Shadows.empty))) with
      modules=
        IdTbl.mapi external_modules ~f:(fun ident m ->
            match m with
            | Immediate v ->
                Immediate v
            | _ ->
                Deferred (Ident.name ident) ) }
end

let empty_resolve_env : Scope.t resolve_env =
  {type_env= TypeEnvi.empty; external_modules= IdTbl.empty}

type t =
  {scope_stack: Scope.t list; depth: int; resolve_env: Scope.t resolve_env}

let empty resolve_env =
  { scope_stack= [Scope.empty ~mode:Checked (Scope.Module (ref Shadows.empty))]
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

let current_mode env = (current_scope env).mode

let current_shadow env = Scope.get_shadow (current_scope env)

let mode_or_default mode env =
  match mode with Some mode -> mode | None -> current_mode env

let make_functor ~mode f = Scope.empty ~mode (Functor f)

let open_expr_scope ?mode env =
  let mode = mode_or_default mode env in
  push_scope Scope.(empty ~mode Expr) env

let open_module ?mode env =
  let mode = mode_or_default mode env in
  push_scope Scope.(empty ~mode (Module (ref Shadows.empty))) env

let open_namespace_scope ?mode path scope env =
  let add_name ident = (Path.dot path ident, Path.Pident ident) in
  let expr_subst ~local_path path = (path, local_path) in
  let subst =
    Scope.build_subst Subst.empty scope ~type_subst:add_name
      ~module_subst:add_name ~expr_subst
  in
  let scope = Scope.subst subst scope in
  let mode = mode_or_default mode env in
  let shadows = current_shadow env in
  env
  |> push_scope {scope with kind= Scope.Open path}
  |> push_scope Scope.(empty ~mode (Continue shadows))

let open_instance_scope ?mode path scope env =
  let scope =
    { (Scope.empty ~mode:scope.Scope.mode (Open_instance path)) with
      instances= scope.instances }
  in
  let shadows = current_shadow env in
  let mode = mode_or_default mode env in
  env |> push_scope scope |> push_scope Scope.(empty ~mode (Continue shadows))

let open_mode_module_scope mode env =
  push_scope Scope.(empty ~mode (Continue (current_shadow env))) env

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
    | Scope.Module _ ->
        (scope :: scopes, env)
    | Expr ->
        raise (Error (of_prim __POS__, Wrong_scope_kind "module"))
    | Open path ->
        let add_name ident = (Path.Pident ident, Path.dot path ident) in
        let expr_subst ~local_path:_ path' = (path', Path.pdot path path') in
        let subst =
          Scope.build_subst Subst.empty scope ~type_subst:add_name
            ~module_subst:add_name ~expr_subst
        in
        let scopes = List.map ~f:(Scope.subst subst) scopes in
        all_scopes scopes env
    | Open_instance _ ->
        all_scopes scopes env
    | Continue _ ->
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

let map_current_scope ~f env =
  match env.scope_stack with
  | current_scope :: scope_stack ->
      {env with scope_stack= f current_scope :: scope_stack}
  | [] ->
      raise (Error (of_prim __POS__, No_open_scopes))

let get_var_names env =
  let rec go map set scope_stack =
    match scope_stack with
    | ({Scope.kind= Expr; _} as current_scope) :: scope_stack ->
        let map, set =
          Map.fold ~init:(map, set) current_scope.type_variables
            ~f:(fun ~key ~data (map, set) ->
              (Map.add_exn map ~key:data ~data:key, Set.add set key) )
        in
        go map set scope_stack
    | _ ->
        (map, set)
  in
  go Typeset.Map.empty String.Set.empty env.scope_stack

let set_var_names env =
  let typ_map, name_set = get_var_names env in
  let typ_map = ref typ_map in
  let name_set = ref name_set in
  let next_name = ref 0 in
  let get_next_name () =
    let id = !next_name in
    incr next_name ;
    if id < 26 then String.of_char (Char.of_int_exn (Char.to_int 'a' + id))
    else sprintf "a%i" (id - 25)
  in
  let rec get_fresh_name () =
    let name = get_next_name () in
    if Set.mem !name_set name then get_fresh_name ()
    else (
      name_set := Set.add !name_set name ;
      name )
  in
  (* This mapper doesn't recurse into type_alternates. *)
  let type_expr mapper typ =
    ( match typ.Type0.type_desc with
    | Tvar _ -> (
      match Map.find !typ_map typ with
      | Some name ->
          Type1.set_desc typ (Tvar (Some name))
      | None ->
          let name = get_fresh_name () in
          typ_map := Map.add_exn !typ_map ~key:typ ~data:name ;
          Type1.set_desc typ (Tvar (Some name)) )
    (* TODO: Handle both branches of Tconv when it is merged. *)
    | _ ->
        Type1.set_desc typ (mapper.Type0_map.type_desc mapper typ.type_desc) ) ;
    typ
  in
  {Type0_map.default_mapper with type_expr}

let add_type_variable name typ =
  map_current_scope ~f:(Scope.add_type_variable name typ)

let find_type_variable ~mode name env =
  List.find_map ~f:(Scope.find_type_variable ~mode name) env.scope_stack

let add_module ~loc (name : Ident.t) m =
  let add_name ident = (Path.Pident ident, Path.dot (Pident name) ident) in
  let expr_subst ~local_path:_ path =
    (path, Path.add_outer_module name path)
  in
  let subst =
    Scope.build_subst Subst.empty m ~type_subst:add_name ~module_subst:add_name
      ~expr_subst
  in
  let m = Scope.subst subst m in
  map_current_scope ~f:(fun scope ->
      let scope = Scope.add_module ~loc name (Scope.Immediate m) scope in
      { scope with
        instances=
          (* Use the rev versions to automatically get tail-recursion for
             append.
          *)
          List.rev_append
            (List.rev_map m.instances ~f:(fun (local_path, global_path, typ) ->
                 (Path.add_outer_module name local_path, global_path, typ) ))
            scope.instances } )

let add_deferred_module ~loc (name : Ident.t) lid =
  map_current_scope ~f:(Scope.add_module ~loc name (Scope.Deferred lid))

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
        IdTbl.find_name name
          ~modes:(reject_shadowed ~loc "module" lid.txt mode)
          env.resolve_env.external_modules
      with
      | Some (ident, _) ->
          Some (Pident ident, Deferred name)
      | None ->
          None )
    | _ ->
        let path, m = find_module ~mode ~loc lid env in
        Some (path, Immediate m) )

let add_implicit_instance name typ env =
  let path = Path.Pident name in
  let env =
    map_current_scope env ~f:(fun scope ->
        {scope with instances= (path, path, typ) :: scope.instances} )
  in
  env

let find_of_lident ~mode ~kind ~get_name (lid : lid) env =
  let open Option.Let_syntax in
  let loc = lid.loc in
  let modes = reject_shadowed ~loc kind lid.txt mode in
  let full_get_name =
    match lid.txt with
    | Lident name ->
        fun scope ->
          let%map ident, data = get_name ~modes name scope in
          (Path.Pident ident, data)
    | Ldot (path, name) -> (
        fun scope ->
          let%map path, m =
            Scope.find_module_ ~mode ~loc ~scopes:env.scope_stack
              env.resolve_env path scope
          in
          match get_name ~modes name m with
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
        let%map ident, data = get_name ~modes name m in
        (Path.dot path ident, data)
    | _ ->
        None )

let get_of_path ~loc ~kind ~get_name ~find_name (path : Path.t) env =
  let open Option.Let_syntax in
  let rec find
            : 'a.    kind:string -> get_name:(Ident.t -> Scope.t -> 'a option)
              -> find_name:(   modes:(mode -> bool)
                            -> string
                            -> Scope.t
                            -> (Ident.t * 'a) option) -> Path.t -> Scope.t
              -> (Ident.t * 'a) option =
   fun ~kind ~get_name ~find_name path scope ->
    match path with
    | Path.Pident ident ->
        Option.map (get_name ident scope) ~f:(fun x -> (ident, x))
    | Path.Pdot (path', mode, name) | Path.Pocamldot (path', mode, name, _)
      -> (
        (* NOTE: It's safe to ignore ocamldot here because there can be no
                 shadowing opens for types, and currently it is only used for
                 those.
        *)
        let%map _, scope =
          find ~kind:"module"
            ~get_name:(Scope.get_module_by_ident ~loc env.resolve_env)
            ~find_name:(Scope.get_module_no_load ~loc env.resolve_env)
            path' scope
        in
        match
          find_name
            ~modes:(reject_shadowed ~loc kind (Path.to_longident path) mode)
            name scope
        with
        | Some ret ->
            ret
        | None ->
            raise (Error (loc, Unbound_path (kind, path))) )
    | Path.Papply _ ->
        raise (Error (loc, Unbound_path (kind, path)))
  in
  match
    List.find_map ~f:(find ~kind ~get_name ~find_name path) env.scope_stack
  with
  | Some v ->
      v
  | None -> (
    match
      find ~kind ~get_name ~find_name path (Scope.global env.resolve_env)
    with
    | Some v ->
        v
    | None ->
        raise (Error (loc, Unbound_path (kind, path))) )

let join_expr_scope env expr_scope =
  map_current_scope ~f:(Scope.join_expr_scope expr_scope) env

let has_type_declaration ~mode (lid : lid) env =
  Option.is_some
    (find_of_lident ~mode ~kind:"type"
       ~get_name:(Scope.find_type_declaration ~mode:Prover)
       lid env)

let raw_find_type_declaration ~mode (lid : lid) env =
  match
    find_of_lident ~mode ~kind:"type"
      ~get_name:(Scope.find_type_declaration ~mode)
      lid env
  with
  | Some v ->
      v
  | None ->
      raise (Error (lid.loc, Unbound_type lid.txt))

let raw_get_type_declaration ~loc path =
  get_of_path ~loc ~kind:"type" ~get_name:Scope.get_type_declaration
    ~find_name:(Scope.find_type_declaration ~mode:(Path.mode path))
    path

let add_module_type ~loc name m =
  map_current_scope ~f:(Scope.add_module_type ~loc name m)

let find_module_type (lid : lid) =
  find_of_lident ~kind:"module type" ~get_name:Scope.get_module_type lid

module Type = struct
  type env = t

  let mkvar ~mode name env = Type1.mkvar ~mode env.depth name

  module Mk = struct
    open Type1.Mk

    let var ~mode name env = var ~mode env.depth name

    let tuple ~mode typs env = tuple ~mode env.depth typs

    let arrow ~mode ?explicit ?label typ1 typ2 env =
      arrow ~mode ?explicit ?label env.depth typ1 typ2

    let ctor ~mode path params env = ctor ~mode env.depth path params

    let poly ~mode vars typ env = poly ~mode env.depth vars typ

    let conv ~mode typ1 typ2 env = conv ~mode env.depth typ1 typ2

    let opaque ~mode typ env = opaque ~mode env.depth typ

    let other_mode ~mode typ env = other_mode ~mode env.depth typ

    let row ~mode row env = Type1.Mk.row ~mode env.depth row

    let row_of_ctor ~mode ident args env =
      row_of_ctor ~mode env.depth ident args
  end

  let map_env ~f env = env.resolve_env.type_env <- f env.resolve_env.type_env

  (** Replace the representatives of each of list of variables with a fresh
      variable.

      The old values can be restored by taking a snapshot before calling this
      and backtracking to it once the new values have been used.

      [copy] must be called on each of the values, to ensure that a
      tri-stitching [Tvar -> _ <-> _] (for non-[Tvar] stitched types) is
      properly instantiated.
  *)
  let refresh_vars vars env =
    List.iter vars ~f:(fun var ->
        let var = repr var in
        match var.type_desc with
        | Treplace _ ->
            (* Don't choke if the same variable appears multiple times in the
               list.
            *)
            ()
        | Tvar _ -> (
          match (repr var.type_alternate).type_desc with
          | Tvar _ ->
              set_replacement var (mkvar ~mode:var.type_mode None env)
          | Treplace alt_var ->
              (* Tri-stitched type variable, where the stitched part has
                 already been substituted. Tri-stitch to the substitution.
              *)
              assert (equal_mode Checked var.type_mode) ;
              let new_var = mk' ~mode:Checked env.depth (Tvar None) in
              unsafe_set_single_replacement var new_var ;
              new_var.type_alternate <- alt_var
          | _ ->
              (* Tri-stitched type variable where the stitched types have been
               instantiated.

               CAUTION: The returned variable has an invalid [type_alternate].
                        This will be resolved during copying, and must not be
                        used until this happens.
              *)
              assert (equal_mode Checked var.type_mode) ;
              unsafe_set_single_replacement var
                (mk' ~mode:Checked env.depth (Tvar None)) )
        | _ ->
            assert false )

  let copy typ env =
    (*Format.eprintf "Copying:@.%a@." typ_debug_print typ ;*)
    let rec copy typ =
      (*Format.eprintf "Copying (step):@.%a@." typ_debug_print typ ;*)
      let typ = repr typ in
      match typ.type_desc with
      | Tpoly _ ->
          (* Tpoly should only ever appear at the top level of a type. *)
          assert false
      | Treplace typ' -> (
          let alt = repr typ.type_alternate in
          match alt.type_desc with
          | Treplace _ ->
              (* Recursion breaking. *)
              typ'
          | _ ->
              (* A tri-stitched variable has been replaced by [refresh_vars],
                 but it is stitched to a non-variable. We check that the
                 [type_alternate] is invalid attach the copy of the original
                 [type_alternate] in the node.
              *)
              assert (is_invalid typ'.type_alternate) ;
              typ'.type_alternate <- copy alt ;
              typ' )
      | Tvar _ ->
          (* Sanity check: if this is stitched to a non-variable, it must not
             contain any [Treplace]s.
             Levels checking should ensure that this is done correctly, but
             manually setting a replacement can cause a failure here.

             TODO: Row types integration.
          *)
          let rec check_replace = function
            | {type_desc= Treplace _; _} ->
                Format.eprintf
                  "Fatal error:@ Found a replaceable type linked to a \
                   variable.@.%a@."
                  typ_debug_print_alts typ ;
                assert false
            | typ ->
                iter ~f:check_replace typ
          in
          check_replace typ.type_alternate ;
          (* Don't copy variables! *)
          typ
      | Trow {row_tags; row_closed; row_rest; row_presence_proxy} ->
          let row_rest' = repr row_rest in
          let row_rest = copy row_rest' in
          let row_presence_proxy = rp_repr row_presence_proxy in
          let () =
            match (row_rest'.type_desc, row_rest.type_desc) with
            | Treplace _, Tvar _ | Treplace _, Topaque {type_desc= Tvar _; _}
              -> (
              match row_presence_proxy.rp_desc with
              | RpPresent ->
                  set_rp_desc row_presence_proxy (RpReplace (mk_rp RpPresent))
              | RpReplace _ ->
                  ()
              | _ ->
                  (* This can only happen if the proxy has leaked to represent
                     an actual row. Fail hard.
                  *)
                  Format.eprintf "Invalid row_presence_proxy:@.%a@."
                    row_presence_debug_print row_presence_proxy ;
                  assert false )
            | Tvar _, Tvar _
            | Topaque {type_desc= Tvar _; _}, Topaque {type_desc= Tvar _; _} ->
                (* Sanity check *)
                assert (row_presence_proxy.rp_desc = RpPresent)
            | Treplace _, Trow _ | Treplace _, Topaque {type_desc= Trow _; _}
              ->
                (* The copy will have updated (or not) the underlying
                   [row_presence_proxy], which is shared with this row.
                   Nothing to do here.
                *)
                ()
            | _ ->
                Format.eprintf
                  "Unexpected value for [row_rest]. \
                   Original:@.%a@.Copied:@.%a@."
                  typ_debug_print row_rest' typ_debug_print row_rest ;
                assert false
          in
          let row_presence_proxy' = row_presence_proxy in
          let row_presence_proxy =
            match row_presence_proxy.rp_desc with
            | RpReplace pres ->
                pres
            | RpPresent ->
                row_presence_proxy
            | _ ->
                (* This can only happen if the proxy has leaked to represent an
                   actual row. Fail hard.
                *)
                Format.eprintf "Invalid row_presence_proxy:@.%a@."
                  row_presence_debug_print row_presence_proxy ;
                assert false
          in
          let should_freshen =
            not (phys_equal row_presence_proxy row_presence_proxy')
          in
          let typ' =
            Type1.mk' ~mode:typ.type_mode typ.type_depth (Tvar None)
          in
          unsafe_set_single_replacement typ typ' ;
          let replace_desc = typ.type_desc in
          let row_tags' =
            Map.map row_tags ~f:(fun (path, pres, args) ->
                let pres = rp_repr pres in
                let pres =
                  match pres.rp_desc with
                  | RpReplace pres ->
                      pres
                  | rp_desc when should_freshen ->
                      (* Freshen row variables and copy arguments. *)
                      let new_pres = mk_rp rp_desc in
                      set_rp_desc pres (RpReplace new_pres) ;
                      new_pres
                  | _ ->
                      (* Copy arguments only. *)
                      pres
                in
                (path, pres, List.map ~f:copy args) )
          in
          let row_tags = row_tags' in
          typ'.type_desc
          <- Trow {row_tags; row_closed; row_rest; row_presence_proxy} ;
          typ'.type_alternate <- copy typ.type_alternate ;
          (* Sanity check. Copying the alternates should not overwrite the
             replacement for this type, otherwise we will end up in an
             inconsistent state.
          *)
          assert (phys_equal typ.type_desc replace_desc) ;
          typ'
      | desc -> (
          let alt = repr typ.type_alternate in
          match alt.type_desc with
          | Treplace alt ->
              (* Tri-stitching, where the stitched part has already been
                 copied.
              *)
              assert (not (phys_equal typ typ.type_alternate.type_alternate)) ;
              assert (equal_mode typ.type_mode Checked) ;
              let typ' = mk' ~mode:typ.type_mode typ.type_depth (Tvar None) in
              typ'.type_alternate <- alt ;
              unsafe_set_single_replacement typ typ' ;
              typ'.type_desc <- copy_desc ~f:copy desc ;
              typ'
          | Tvar _ ->
              (* If the tri-stitched type isn't a type variable, this should
                 also have been instantiated.
              *)
              assert false
          | Trow _ ->
              (* This should never happen; a row may only ever be stitched to a
                 type variable or a row, both of which are handled above.
              *)
              assert false
          | _ ->
              let alt_desc = typ.type_alternate.type_desc in
              let alt_alt_desc = typ.type_alternate.type_alternate.type_desc in
              let typ' = Type1.mkvar ~mode:typ.type_mode typ.type_depth None in
              let stitched =
                phys_equal typ typ.type_alternate.type_alternate
              in
              if stitched then typ'.type_alternate.type_alternate <- typ' ;
              set_replacement typ typ' ;
              (* NOTE: the variable description of [typ'] was just a
                       placeholder, so we want this new value to be preserved
                       after backtracking.
                 DO NOT replace this with a [Type1.set_repr] or equivalent
                 call.
              *)
              typ'.type_desc <- copy_desc ~f:copy desc ;
              typ'.type_alternate.type_desc <- copy_desc ~f:copy alt_desc ;
              if not stitched then
                (* tri-stitched *)
                typ'.type_alternate.type_alternate.type_desc
                <- copy_desc ~f:copy alt_alt_desc ;
              typ' )
    in
    let typ = repr typ in
    let snap = Snapshot.create () in
    let typ =
      match typ.type_desc with
      | Tpoly (vars, typ) ->
          (* Make fresh variables to instantiate [Tpoly]s. *)
          ignore (refresh_vars vars env) ;
          copy typ
      | _ ->
          copy typ
    in
    (*Format.eprintf "Copied:@.%a@." typ_debug_print typ ;*)
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
        assert (is_var param) ;
        assert (is_var param.type_alternate.type_alternate) ;
        set_replacement param typ ) ;
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

  let rec compare ~loc env typ1 typ2 =
    let compare = compare ~loc env in
    let compare_all = compare_all ~loc env in
    if Int.equal typ1.type_id typ2.type_id then 0
    else
      match ((repr typ1).type_desc, (repr typ2).type_desc) with
      | Tref _, _ | _, Tref _ ->
          assert false
      | Treplace _, _ | _, Treplace _ ->
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
      | ( Tctor {var_params= params1; var_ident= path1; _}
        , Tctor {var_params= params2; var_ident= path2; _} ) ->
          let _, decl1 = raw_get_type_declaration ~loc path1 env in
          let _, decl2 = raw_get_type_declaration ~loc path2 env in
          or_compare (Int.compare decl1.tdec_id decl2.tdec_id) ~f:(fun () ->
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
      | Tarrow (_, _, Implicit, _), _ ->
          -1
      | _, Tarrow (_, _, Implicit, _) ->
          1
      | Tconv typ1, Tconv typ2 ->
          compare typ1 typ2
      | _, Tconv _ ->
          -1
      | Tconv _, _ ->
          1
      | Topaque typ1, Topaque typ2 ->
          compare typ1 typ2
      | Topaque _, _ ->
          1
      | _, Topaque _ ->
          -1
      | Tother_mode typ1, _ when equal_mode typ1.type_mode typ2.type_mode ->
          (* The types may be equal underneath the mode conversion. *)
          compare typ1 typ2
      | _, Tother_mode typ2 when equal_mode typ1.type_mode typ2.type_mode ->
          (* The types may be equal underneath the mode conversion. *)
          compare typ1 typ2
      | Tother_mode typ1, Tother_mode typ2 ->
          compare typ1 typ2
      | Tother_mode _, _ ->
          1
      | _, Tother_mode _ ->
          -1
      | Trow row1, Trow row2 ->
          compare_row ~loc env row1 row2

  and compare_all ~loc env typs1 typs2 =
    match (typs1, typs2) with
    | [], [] ->
        0
    | [], _ ->
        -1
    | _, [] ->
        1
    | typ1 :: typs1, typ2 :: typs2 ->
        or_compare (compare ~loc env typ1 typ2) ~f:(fun () ->
            compare_all ~loc env typs1 typs2 )

  and compare_row ~loc env row1 row2 =
    or_compare (compare_closed_flag row1.row_closed row2.row_closed)
      ~f:(fun () ->
        Map.compare_direct
          (fun (_path1, pres1, args1) (_path2, pres2, args2) ->
            or_compare (compare_row_presence pres1 pres2) ~f:(fun () ->
                compare_all ~loc env args1 args2 ) )
          row1.row_tags row2.row_tags )

  let rec weak_variables depth set typ =
    match typ.type_desc with
    | Tvar _ when typ.type_depth > depth ->
        Set.add set typ
    | _ ->
        Type1.fold ~init:set ~f:(weak_variables depth) typ

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

  let implicit_instance ~(unifies : env -> type_expr -> type_expr -> bool)
      (typ : type_expr) typ_vars env =
    List.find_map env.scope_stack ~f:(fun scope ->
        List.find_map scope.instances ~f:(fun (_, path, instance_typ) ->
            let instance_typ = copy instance_typ env in
            let snapshot = Snapshot.create () in
            let _, base_typ = get_implicits instance_typ in
            if unifies env typ base_typ then
              if
                Set.exists typ_vars ~f:(fun var ->
                    not (phys_equal var (repr var)) )
              then (
                (* There is at least one variable that hasn't been instantiated.
                   In particular, this must mean that it was less general than
                   the variable that it unified with, or that a parent type
                   expression instantiated a type variable in the target type,
                   and so this instance isn't general enough to satisfy the
                   target type.
                *)
                backtrack snapshot ;
                None )
              else (* TODO: Shadowing check. *)
                Some (path, instance_typ)
            else None ) )

  let generate_implicits e env =
    let loc = e.Typedast.exp_loc in
    let implicits, typ = get_implicits e.exp_type in
    match implicits with
    | [] ->
        e
    | _ ->
        let es =
          List.map implicits ~f:(fun (label, typ) ->
              (Implicit, label, new_implicit_var ~loc typ env) )
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
          match implicit_instance ~unifies exp_type typ_vars env with
          | Some (name, instance_typ) ->
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
          | None ->
              true )
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
    let unifies env typ ctyp = unifies env typ (snd (get_implicits ctyp)) in
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
          let cmp =
            compare ~loc env exp1.Typedast.exp_type exp2.Typedast.exp_type
          in
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
                       Type1.equal_at_depth
                         ~get_decl:(fun path ->
                           snd (raw_get_type_declaration ~loc path env) )
                         ~depth:env.depth e_weak.exp_type e_strong.exp_type
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
  let next_id = Type1.Decl.next_id

  let mk = Type1.Decl.mk

  let mk_typ ~mode ~params decl env =
    (* Sanity check. *)
    List.iter params ~f:(fun param -> assert (equal_mode mode param.type_mode)) ;
    let vars =
      List.map decl.tdec_params ~f:(fun var ->
          assert (Type1.is_var var) ;
          Type1.get_mode mode var )
    in
    let typ = Type1.get_mode mode decl.tdec_ret in
    Type.instantiate vars params typ env

  let predeclare ident decl env =
    env.resolve_env.type_env
    <- { env.resolve_env.type_env with
         predeclared_types=
           Ident.Table.add ~key:ident ~data:decl.tdec_id
             env.resolve_env.type_env.predeclared_types }

  let clear_predeclared env =
    env.resolve_env.type_env
    <- {env.resolve_env.type_env with predeclared_types= Ident.Table.empty}

  let find_of_variant ~loc variant env =
    snd (raw_get_type_declaration ~loc variant.var_ident env)

  let find_of_field ~mode (field : lid) env =
    find_of_lident ~mode ~kind:"field" ~get_name:(Scope.find_field ~mode) field
      env

  let find_of_constructor ~mode (ctor : lid) env =
    find_of_lident ~mode ~kind:"constructor" ~get_name:(Scope.find_ctor ~mode)
      ctor env

  let unfold_alias_aux ~loc typ env =
    match (repr typ).type_desc with
    | Tctor variant -> (
      match find_of_variant ~loc variant env with
      | {tdec_desc= TAlias alias_typ; tdec_params; _} as desc ->
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

let find_name ~mode (lid : lid) env =
  match
    find_of_lident ~mode ~kind:"name" ~get_name:Scope.find_name lid env
  with
  | Some (ident, typ) ->
      let typ' = Type.copy typ env in
      (*Format.eprintf "Copying. Original:@.%a@.Copy:@.%a@." typ_debug_print typ
        typ_debug_print typ' ;*)
      (ident, typ')
  | None ->
      raise (Error (lid.loc, Unbound_value lid.txt))

let get_of_constructor ~loc path =
  get_of_path ~loc ~kind:"constructor" ~get_name:Scope.get_ctor
    ~find_name:(Scope.find_ctor ~mode:(Path.mode path))
    path

let find_conversion ~unifies typ env =
  let typ_vars = Type1.type_vars typ in
  let typ =
    Mk.conv ~mode:typ.type_mode typ.type_depth typ typ.type_alternate
  in
  match Type.implicit_instance ~unifies typ typ_vars env with
  | None ->
      None
  | Some (path, typ) ->
      let args, _ = get_implicits typ in
      Some (path, args)

(** Wrap any prover implicit arguments with [Tprover], so that they may be
    surfaced as implicit arguments within checked types.
*)
let wrap_prover_implicits env =
  if equal_mode Checked (current_mode env) then
    let ({type_env; _} as resolve_env) = env.resolve_env in
    resolve_env.type_env
    <- { type_env with
         implicit_vars=
           List.map type_env.implicit_vars ~f:(fun exp ->
               let typ = exp.Typedast.exp_type in
               if equal_mode typ.type_mode Prover then
                 (* NOTE: This is fine, because the expression overwriting is
                           done in a subvalue of [exp_desc], which is
                           preserved.
                  *)
                 { exp with
                   exp_type=
                     Type1.Mk.other_mode ~mode:Checked typ.type_depth typ }
               else exp ) }

(* Error handling *)

open Format

let pp_typ = Typeprint.type_expr

let pp_decl_typ ppf decl = pp_typ ppf decl.tdec_ret

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
  | Unbound_path (kind, path) ->
      fprintf ppf "@[<hov>Internal error: Could not resolve %s @[<h>%a@].@]"
        kind Path.debug_print path
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
  | Duplicate_name (kind, generated, name) ->
      let generated = if generated then "generated " else "" in
      fprintf ppf "@[<hov>There is already a %s%s@ with name@ %s.@]" generated
        kind name
  | Shadowed_checked (kind, name) ->
      fprintf ppf
        "@[<hov>Could not find a %s@ %a:@ The current definition is only \
         available in prover mode.@]"
        kind Longident.pp name

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
    | _ ->
        None )
