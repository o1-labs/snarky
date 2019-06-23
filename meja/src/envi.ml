open Core_kernel
open Ast_types
open Type0
open Ast_build.Loc
open Longident

let id = ref 0

let next_id () = incr id ; !id

type error =
  | No_open_scopes
  | Wrong_scope_kind of string
  | Multiple_definition of string * string
  | Unbound_type_var of type_expr
  | Unbound_type of Longident.t
  | Unbound_module of Longident.t
  | Unbound_value of Longident.t
  | Wrong_number_args of Longident.t * int * int
  | Expected_type_var of type_expr
  | Lident_unhandled of string * Longident.t
  | Constraints_not_satisfied of type_expr * type_decl
  | No_unifiable_implicit
  | Multiple_instances of type_expr
  | Recursive_load of string
  | Predeclared_types of string list
  | Functor_in_module_sig
  | Not_a_functor

exception Error of Location.t * error

module TypeEnvi = struct
  type t =
    { variable_instances: type_expr Int.Map.t
    ; implicit_id: int
    ; implicit_vars: Parsetypes.expression list
    ; instances: (int * Longident.t * type_expr) list
    ; predeclared_types:
        (int (* id *) * int option ref (* num. args *) * Location.t)
        String.Map.t }

  let empty =
    { variable_instances= Int.Map.empty
    ; implicit_id= 1
    ; implicit_vars= []
    ; instances= []
    ; predeclared_types= String.Map.empty }

  let instance env (typ : type_expr) =
    Map.find env.variable_instances typ.type_id

  let add_instance (typ : type_expr) typ' env =
    { env with
      variable_instances=
        Map.set env.variable_instances ~key:typ.type_id ~data:typ' }

  let clear_instance (typ : type_expr) env =
    {env with variable_instances= Map.remove env.variable_instances typ.type_id}

  let next_type_id env = (next_id (), env)

  let next_decl_id env = (next_id (), env)

  let next_instance_id env = (next_id (), env)

  let add_implicit_instance id canonical_path typ env =
    {env with instances= (id, canonical_path, typ) :: env.instances}
end

type 'a or_deferred =
  | Immediate of 'a
  | Deferred of string
  | In_flight of string

type 'a resolve_env =
  { mutable type_env: TypeEnvi.t
  ; mutable external_modules: 'a or_deferred String.Map.t
  ; mutable predeclare_types: bool }

module Scope = struct
  type 'a or_path = Immediate of 'a | Deferred of Longident.t

  type kind = Module | Open | Expr | ExprOpen | Continue | Functor

  let kind_to_string = function
    | Module ->
        "module"
    | Open ->
        "open"
    | Expr ->
        "expr"
    | ExprOpen ->
        "expr-open"
    | Continue ->
        "continue"
    | Functor ->
        "functor"

  type paths = {paths_of_ids: Longident.t Int.Map.t}

  type t =
    { path: Longident.t option
    ; functor_: (mode -> Longident.t -> t or_path -> full_scope) option
    ; names: (type_expr * int) String.Map.t
    ; type_variables: type_expr String.Map.t
    ; type_decls: type_decl String.Map.t
    ; fields: (type_decl * int) String.Map.t
    ; ctors: (type_decl * int) String.Map.t
    ; instances: Longident.t Int.Map.t
    ; modules: full_scope or_path String.Map.t
    ; module_types: full_scope or_path String.Map.t
    ; paths: paths }

  and full_scope =
    {kind: kind; ocaml_scope: t; checked_scope: t option; prover_scope: t}

  let get_scope_opt mode full_scope =
    match mode with
    | OCaml ->
        Some full_scope.ocaml_scope
    | Checked ->
        full_scope.checked_scope
    | Prover ->
        Some full_scope.prover_scope

  let load_module :
      (   loc:Location.t
       -> name:string
       -> full_scope resolve_env
       -> string
       -> full_scope)
      ref =
    ref (fun ~loc ~name _env _filename ->
        raise (Error (loc, Unbound_module (Lident name))) )

  let empty_paths = {paths_of_ids= Int.Map.empty}

  let empty path =
    { path
    ; functor_= None
    ; names= String.Map.empty
    ; type_variables= String.Map.empty
    ; type_decls= String.Map.empty
    ; fields= String.Map.empty
    ; ctors= String.Map.empty
    ; instances= Int.Map.empty
    ; modules= String.Map.empty
    ; module_types= String.Map.empty
    ; paths= empty_paths }

  let set_path path env = {env with path= Some path}

  let add_preferred_name path id {paths_of_ids} =
    {paths_of_ids= Map.set paths_of_ids ~key:id ~data:path}

  let add_name_raw key typ id scope =
    { scope with
      names= Map.set scope.names ~key ~data:(typ, id)
    ; paths= add_preferred_name (Lident key) id scope.paths }

  let add_name key typ scope =
    let id = next_id () in
    add_name_raw key typ id scope

  let get_name name {names; _} = Map.find names name

  let add_type_variable key typ scope =
    {scope with type_variables= Map.set scope.type_variables ~key ~data:typ}

  let find_type_variable name scope = Map.find scope.type_variables name

  let add_field decl index scope field_decl =
    { scope with
      fields=
        Map.set scope.fields ~key:field_decl.fld_ident.txt ~data:(decl, index)
    }

  let get_field name scope = Map.find scope.fields name

  let add_ctor decl index scope ctor_decl =
    { scope with
      ctors=
        Map.set scope.ctors ~key:ctor_decl.ctor_ident.txt ~data:(decl, index)
    }

  let get_ctor name scope = Map.find scope.ctors name

  let add_preferred_type_name path decl_id {paths_of_ids} =
    {paths_of_ids= Map.set paths_of_ids ~key:decl_id ~data:path}

  let get_preferred_type_name decl_id {paths= {paths_of_ids}; _} =
    Map.find paths_of_ids decl_id

  let add_type_declaration_explicit name path decl scope =
    match decl.tdec_desc with
    | TExtend _ ->
        scope
    | _ ->
        { scope with
          type_decls=
            Option.fold ~init:scope.type_decls name ~f:(fun type_decls name ->
                Map.set type_decls ~key:name ~data:decl )
        ; paths= add_preferred_type_name path decl.tdec_id scope.paths }

  let add_type_declaration decl scope =
    add_type_declaration_explicit (Some decl.tdec_ident.txt)
      (Lident decl.tdec_ident.txt) decl scope

  let get_type_declaration name scope = Map.find scope.type_decls name

  let register_type_declaration_parts decl scope =
    match decl.tdec_desc with
    | TAbstract | TAlias _ | TUnfold _ | TOpen | TForward _ ->
        scope
    | TRecord fields ->
        List.foldi ~f:(add_field decl) ~init:scope fields
    | TVariant ctors ->
        List.foldi ~f:(add_ctor decl) ~init:scope ctors
    | TExtend (_, _, ctors) ->
        List.foldi ~f:(add_ctor decl) ~init:scope ctors

  let register_type_declaration decl scope =
    let scope = add_type_declaration decl scope in
    register_type_declaration_parts decl scope

  let fold_over ~init:acc ~names ~type_variables ~type_decls ~fields ~ctors
      ~modules ~module_types ~instances
      { path= _
      ; functor_= functor1
      ; names= names1
      ; type_variables= type_variables1
      ; type_decls= type_decls1
      ; fields= fields1
      ; ctors= ctors1
      ; instances= instances1
      ; modules= modules1
      ; module_types= module_types1
      ; paths= _ }
      { path= _
      ; functor_= functor2
      ; names= names2
      ; type_variables= type_variables2
      ; type_decls= type_decls2
      ; fields= fields2
      ; ctors= ctors2
      ; instances= instances2
      ; modules= modules2
      ; module_types= module_types2
      ; paths= _ } =
    assert (Option.is_none functor1 && Option.is_none functor2) ;
    let acc =
      Map.fold2 type_variables1 type_variables2 ~init:acc ~f:type_variables
    in
    let acc = Map.fold2 type_decls1 type_decls2 ~init:acc ~f:type_decls in
    let acc = Map.fold2 ctors1 ctors2 ~init:acc ~f:ctors in
    let acc = Map.fold2 fields1 fields2 ~init:acc ~f:fields in
    let acc = Map.fold2 modules1 modules2 ~init:acc ~f:modules in
    let acc =
      Map.fold2 module_types1 module_types2 ~init:acc ~f:module_types
    in
    let acc = Map.fold2 instances1 instances2 ~init:acc ~f:instances in
    let acc = Map.fold2 names1 names2 ~init:acc ~f:names in
    acc

  (* Extend the paths in the first argument with those in the second,
     overwriting them where both exist.
  *)
  let join_paths {paths_of_ids= paths_of_ids1} {paths_of_ids= paths_of_ids2} =
    { paths_of_ids=
        Map.merge_skewed paths_of_ids1 paths_of_ids2
          ~combine:(fun ~key:_ _ v -> v) }

  (* [join ~loc scope1 scope2] attaches the definitions in [scope2] to
     [scope1], raising an error at [loc] if a name is defined in both scopes
     when a single definition of that kind is allowed.

     The context (path, etc.) of [scope1] is preserved; the context of
     [scope2] is discarded.
  *)
  let join ~loc
      { path
      ; functor_= functor1
      ; names= names1
      ; type_variables= type_variables1
      ; type_decls= type_decls1
      ; fields= fields1
      ; ctors= ctors1
      ; modules= modules1
      ; module_types= module_types1
      ; instances= instances1
      ; paths= paths1 }
      { path= _
      ; functor_= functor2
      ; names= names2
      ; type_variables= type_variables2
      ; type_decls= type_decls2
      ; fields= fields2
      ; ctors= ctors2
      ; modules= modules2
      ; module_types= module_types2
      ; instances= instances2
      ; paths= paths2 } =
    assert (Option.is_none functor1 && Option.is_none functor2) ;
    { path
    ; functor_= None
    ; names= Map.merge_skewed names1 names2 ~combine:(fun ~key:_ _ v -> v)
    ; type_variables=
        Map.merge_skewed type_variables1 type_variables2
          ~combine:(fun ~key:_ _ v -> v)
    ; type_decls=
        Map.merge_skewed type_decls1 type_decls2 ~combine:(fun ~key _ _ ->
            raise (Error (loc, Multiple_definition ("type", key))) )
    ; fields= Map.merge_skewed fields1 fields2 ~combine:(fun ~key:_ _ v -> v)
    ; ctors= Map.merge_skewed ctors1 ctors2 ~combine:(fun ~key:_ _ v -> v)
    ; modules=
        Map.merge_skewed modules1 modules2 ~combine:(fun ~key _ _ ->
            raise (Error (loc, Multiple_definition ("module", key))) )
    ; module_types=
        Map.merge_skewed module_types1 module_types2 ~combine:(fun ~key _ _ ->
            raise (Error (loc, Multiple_definition ("module type", key))) )
    ; instances=
        Map.merge_skewed instances1 instances2 ~combine:(fun ~key:_ _ v -> v)
    ; paths= join_paths paths1 paths2 }

  let extend_paths name {paths_of_ids} =
    {paths_of_ids= Map.map ~f:(add_outer_module name) paths_of_ids}

  let add_module name m scope =
    {scope with modules= Map.set scope.modules ~key:name ~data:m}

  let add_module_type name m scope =
    {scope with module_types= Map.set scope.module_types ~key:name ~data:m}

  let get_module_type name scope = Map.find scope.module_types name

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

  let rec find_module_ ~loc ~scopes mode resolve_env lid scope : t option =
    match lid with
    | Lident name ->
        get_module ~loc ~scopes mode resolve_env name scope
    | Ldot (path, name) ->
        Option.bind
          (find_module_ ~loc ~scopes mode resolve_env path scope)
          ~f:(get_module ~loc ~scopes mode resolve_env name)
    | Lapply (fpath, path) ->
        Option.Let_syntax.(
          let%bind m_f =
            find_module_ ~loc ~scopes mode resolve_env fpath scope
          in
          let m = apply_functor ~loc ~scopes mode resolve_env fpath path m_f in
          get_scope_opt mode m)

  and apply_functor ~loc ~scopes mode resolve_env fpath lid scope =
    let f =
      match scope.functor_ with
      | Some f ->
          f
      | _ ->
          raise (Error (loc, Not_a_functor))
    in
    let m = find_module ~loc mode lid resolve_env scopes in
    f mode fpath (Immediate m)

  and get_module ~loc ~scopes mode resolve_env name scope =
    match Map.find scope.modules name with
    | Some (Immediate m) ->
        get_scope_opt mode m
    | Some (Deferred lid) ->
        get_global_module ~loc ~scopes mode resolve_env lid
    | None ->
        None

  and get_global_module ~loc ~scopes mode resolve_env lid =
    let name, lid = outer_mod_name ~loc lid in
    let m =
      match Map.find resolve_env.external_modules name with
      | Some (Immediate m) ->
          Some m
      | Some (Deferred filename) ->
          resolve_env.external_modules
          <- Map.set resolve_env.external_modules ~key:name
               ~data:(In_flight filename) ;
          let m = !load_module ~loc ~name resolve_env filename in
          resolve_env.external_modules
          <- Map.set resolve_env.external_modules ~key:name ~data:(Immediate m) ;
          Some m
      | Some (In_flight filename) ->
          raise (Error (loc, Recursive_load filename))
      | None ->
          None
    in
    let m = Option.bind m ~f:(get_scope_opt mode) in
    match (m, lid) with
    | Some m, Some lid ->
        find_module_ ~loc ~scopes mode resolve_env lid m
    | Some m, None ->
        Some m
    | None, _ ->
        None

  and find_module ~loc mode lid resolve_env scopes =
    match
      List.find_map
        ~f:(fun scope ->
          Option.bind (get_scope_opt mode scope)
            ~f:(find_module_ ~loc ~scopes mode resolve_env lid) )
        scopes
    with
    | Some m ->
        m
    | None -> (
      match get_global_module ~loc ~scopes mode resolve_env lid with
      | Some m ->
          m
      | None ->
          raise (Error (loc, Unbound_module lid)) )

  let rec find_module_deferred ~loc ~scopes mode resolve_env lid scope =
    match lid with
    | Lident name ->
        Map.find scope.modules name
    | Ldot (path, name) -> (
      match find_module_deferred ~loc ~scopes mode resolve_env path scope with
      | Some (Immediate m) ->
          Option.bind (get_scope_opt mode m) ~f:(fun m ->
              Map.find m.modules name )
      | Some (Deferred lid) ->
          Some (Deferred (Ldot (lid, name)))
      | None ->
          None )
    | Lapply (lid1, lid2) ->
        (* Don't defer functor applications *)
        let m_functor = find_module ~loc mode lid1 resolve_env scopes in
        let m =
          apply_functor ~loc ~scopes mode resolve_env lid1 lid2 m_functor
        in
        Some (Immediate m)

  let join_expr_scope (scope : t) (expr_scope : t) =
    let select_new ~key:_ _ new_value = new_value in
    { scope with
      names= Map.merge_skewed scope.names expr_scope.names ~combine:select_new
    }
end

module FullScope = struct
  (* Three scopes:
     * OCaml has OCaml names for all types.
     * Checked has checked names and types available during checking code only.
     * Prover has prover names, but access to Checked types.
  *)
  type t = Scope.full_scope =
    { kind: Scope.kind
    ; ocaml_scope: Scope.t
    ; checked_scope: Scope.t option
    ; prover_scope: Scope.t }

  let empty mode path kind =
    let empty = Scope.empty path in
    match mode with
    | Prover ->
        {ocaml_scope= empty; checked_scope= None; prover_scope= empty; kind}
    | _ ->
        { ocaml_scope= empty
        ; checked_scope= Some empty
        ; prover_scope= empty
        ; kind }

  let get_scope_opt = Scope.get_scope_opt

  let get_scope mode full_scope =
    Option.value_exn (get_scope_opt mode full_scope)

  let map_scope mode ~f scope =
    match mode with
    | OCaml ->
        {scope with ocaml_scope= f scope.ocaml_scope}
    | Checked ->
        { scope with
          checked_scope= Some (f (Option.value_exn scope.checked_scope)) }
    | Prover ->
        {scope with prover_scope= f scope.prover_scope}

  let map_scopes ~f scope =
    { scope with
      ocaml_scope= f scope.ocaml_scope
    ; checked_scope= Option.map ~f scope.checked_scope
    ; prover_scope= f scope.prover_scope }

  let map2_scopes ~f scope1 scope2 =
    { scope1 with
      ocaml_scope= f scope1.ocaml_scope scope2.ocaml_scope
    ; checked_scope= Option.map2 ~f scope1.checked_scope scope2.checked_scope
    ; prover_scope= f scope1.prover_scope scope2.prover_scope }

  let on_scope mode ~f scope = f (get_scope mode scope)

  let on_scope_opt mode ~f scope = Option.bind (get_scope_opt mode scope) ~f

  let join ~loc = map2_scopes ~f:(Scope.join ~loc)

  let join_expr_scope expr_scope scope =
    assert (expr_scope.kind = Expr) ;
    map2_scopes ~f:Scope.join_expr_scope scope expr_scope

  let get_type_declaration mode name =
    on_scope mode ~f:(Scope.get_type_declaration name)

  let register_type_declaration_raw ~add name lid mode decl scope =
    let scope =
      map_scope mode scope
        ~f:(Scope.add_type_declaration_explicit name lid decl)
    in
    if add then
      map_scope mode scope ~f:(Scope.register_type_declaration_parts decl)
    else scope
end

let empty_resolve_env : FullScope.t resolve_env =
  { type_env= TypeEnvi.empty
  ; external_modules= String.Map.empty
  ; predeclare_types= false }

type t =
  { scope_stack: FullScope.t list
  ; depth: int
  ; resolve_env: FullScope.t resolve_env }

let empty resolve_env =
  { scope_stack= [FullScope.empty Checked None Scope.Module]
  ; depth= 0
  ; resolve_env }

let current_scope {scope_stack; _} =
  match List.hd scope_stack with
  | Some scope ->
      scope
  | None ->
      raise (Error (of_prim __POS__, No_open_scopes))

let push_scope scope env =
  (*( match (env.scope_stack, scope.FullScope.kind) with
  | {kind= (Expr | ExprOpen) as kind; _} :: _, (Module | Open | Continue) ->
    failwith (Scope.kind_to_string kind ^ " " ^ Scope.kind_to_string scope.FullScope.kind)
      (*raise (Error (of_prim __POS__, Wrong_scope_kind "expression"))*)
  | {kind= Module | Open | Continue | Expr; _} :: _, Functor ->
      raise (Error (of_prim __POS__, Wrong_scope_kind "non_functor"))
  | {kind= Functor; _} :: _, _ ->
      raise (Error (of_prim __POS__, Functor_in_module_sig))
  | _ ->
      () ) ;*)
  {env with scope_stack= scope :: env.scope_stack; depth= env.depth + 1}

let current_path mode env =
  let scope = current_scope env in
  match FullScope.get_scope_opt mode scope with
  | Some scope ->
      scope.path
  | None ->
      scope.ocaml_scope.path

let relative_path env mode name = join_name (current_path mode env) name

let make_functor mode path f =
  let scope = FullScope.empty Checked (Some path) Functor in
  FullScope.map_scope mode scope ~f:(fun scope -> {scope with functor_= Some f})

let open_expr_scope mode env =
  push_scope FullScope.(empty mode (current_path mode env) Expr) env

let open_module name mode env =
  push_scope
    FullScope.(empty mode (Some (relative_path env mode name)) Module)
    env

let open_absolute_module path mode env =
  push_scope FullScope.(empty mode path Module) env

let open_continue_module mode env =
  push_scope FullScope.(empty mode (current_path mode env) Continue) env

let open_expr_namespace_scope scope mode env =
  let path = current_path mode env in
  env
  |> push_scope {scope with kind= Scope.ExprOpen}
  |> push_scope FullScope.(empty mode path Continue)

let open_namespace_scope scope mode env =
  let path = current_path mode env in
  env
  |> push_scope {scope with kind= Scope.Open}
  |> push_scope FullScope.(empty mode path Continue)

let pop_scope env =
  match env.scope_stack with
  | [] ->
      raise (Error (of_prim __POS__, No_open_scopes))
  | scope :: scope_stack ->
      (scope, {env with scope_stack; depth= env.depth - 1})

let pop_expr_scope env =
  let scope, env = pop_scope env in
  match scope.Scope.kind with
  | Scope.Expr | Scope.ExprOpen ->
      (scope, env)
  | _ ->
      raise (Error (of_prim __POS__, Wrong_scope_kind "expression"))

let pop_module ~loc env =
  let rec all_scopes scopes env =
    let scope, env = pop_scope env in
    match scope.kind with
    | Scope.Module ->
        (scope :: scopes, env)
    | Expr | ExprOpen ->
        (*TODO(Matt): Re-enable this. *)
        (*raise (Error (of_prim __POS__, Wrong_scope_kind "module"))*)
        all_scopes scopes env
    | Open ->
        all_scopes scopes env
    | Continue ->
        all_scopes (scope :: scopes) env
    | Functor ->
        if List.is_empty scopes then ([scope], env)
        else raise (Error (of_prim __POS__, Functor_in_module_sig))
  in
  let scopes, env = all_scopes [] env in
  let m =
    match scopes with
    | [] ->
        assert false
    | hd :: scopes ->
        List.fold_left ~init:hd scopes ~f:(FullScope.join ~loc)
  in
  (m, env)

let close_expr_scope env = snd (pop_expr_scope env)

let set_type_predeclaring env = env.resolve_env.predeclare_types <- true

let unset_type_predeclaring env =
  if Map.is_empty env.resolve_env.type_env.predeclared_types then
    env.resolve_env.predeclare_types <- false
  else
    let _, (_, _, loc) =
      Map.min_elt_exn env.resolve_env.type_env.predeclared_types
    in
    let predeclared = Map.keys env.resolve_env.type_env.predeclared_types in
    raise (Error (loc, Predeclared_types predeclared))

let map_current_scope ~f env =
  match env.scope_stack with
  | current_scope :: scope_stack ->
      {env with scope_stack= f current_scope :: scope_stack}
  | [] ->
      raise (Error (of_prim __POS__, No_open_scopes))

let set_path path =
  map_current_scope ~f:(FullScope.map_scopes ~f:(Scope.set_path path))

let add_type_variable name typ =
  map_current_scope
    ~f:(FullScope.map_scopes ~f:(Scope.add_type_variable name typ))

let find_type_variable name mode env =
  List.find_map
    ~f:(FullScope.on_scope_opt mode ~f:(Scope.find_type_variable name))
    env.scope_stack

let add_module mode (name : str) m =
  map_current_scope
    ~f:
      (FullScope.map_scope mode ~f:(fun scope ->
           let scope = Scope.add_module name.txt (Scope.Immediate m) scope in
           let m = FullScope.get_scope mode m in
           let paths = Scope.extend_paths name.txt m.Scope.paths in
           { scope with
             instances=
               Map.merge scope.instances m.instances ~f:(fun ~key:_ data ->
                   match data with
                   | `Left x ->
                       Some x
                   | `Both (_, x) | `Right x ->
                       Some (Longident.add_outer_module name.txt x) )
           ; (* Prefer the shorter paths in the current module to those in the
           module we are adding. *)
             paths= Scope.join_paths paths scope.paths } ))

let add_deferred_module mode (name : str) lid =
  map_current_scope
    ~f:
      (FullScope.map_scope mode
         ~f:(Scope.add_module name.txt (Scope.Deferred lid)))

let register_external_module name x env =
  env.resolve_env.external_modules
  <- Map.set ~key:name ~data:x env.resolve_env.external_modules

let find_module ~loc mode (lid : lid) env =
  Scope.find_module ~loc mode lid.txt env.resolve_env env.scope_stack

let find_module_deferred ~loc mode (lid : lid) env =
  List.find_map env.scope_stack ~f:(fun scope ->
      Option.bind
        (FullScope.get_scope_opt mode scope)
        ~f:
          (Scope.find_module_deferred mode ~loc ~scopes:env.scope_stack
             env.resolve_env lid.txt) )

let register_type_declaration_raw mode ?name lid decl =
  map_current_scope
    ~f:
      (FullScope.register_type_declaration_raw ~add:(Option.is_some name) name
         lid mode decl)

let add_implicit_instance name typ env =
  let path = Lident name in
  let id, type_env = TypeEnvi.next_instance_id env.resolve_env.type_env in
  let env =
    map_current_scope env
      ~f:
        (FullScope.map_scopes ~f:(fun scope ->
             {scope with instances= Map.set ~key:id ~data:path scope.instances}
         ))
  in
  let canonical_path = relative_path env OCaml name in
  env.resolve_env.type_env
  <- TypeEnvi.add_implicit_instance id canonical_path typ type_env ;
  env

let find_of_lident ~kind ~get_name mode (lid : lid) env =
  let loc = lid.loc in
  let full_get_name =
    let open Option.Let_syntax in
    match lid.txt with
    | Lident name ->
        fun scope ->
          let%bind scope = FullScope.get_scope_opt mode scope in
          get_name name scope
    | Ldot (path, name) ->
        fun scope ->
          let%bind scope = FullScope.get_scope_opt mode scope in
          let%bind scope =
            Scope.find_module_ ~loc ~scopes:env.scope_stack mode
              env.resolve_env path scope
          in
          get_name name scope
    | Lapply _ ->
        raise (Error (loc, Lident_unhandled (kind, lid.txt)))
  in
  match List.find_map ~f:full_get_name env.scope_stack with
  | Some v ->
      Some v
  | None -> (
    match lid.txt with
    | Ldot (path, name) ->
        let m =
          Scope.get_global_module mode ~loc ~scopes:env.scope_stack
            env.resolve_env path
        in
        Option.bind m ~f:(get_name name)
    | _ ->
        None )

let join_expr_scope env expr_scope =
  map_current_scope ~f:(FullScope.join_expr_scope expr_scope) env

let raw_find_type_declaration mode (lid : lid) env =
  match
    find_of_lident mode ~kind:"type" ~get_name:Scope.get_type_declaration lid
      env
  with
  | Some v ->
      v
  | None -> (
    match lid.txt with
    | Lident name when env.resolve_env.predeclare_types ->
        let {type_env; _} = env.resolve_env in
        let id, num_args =
          match Map.find type_env.predeclared_types name with
          | Some (id, num_args, _loc) ->
              (id, num_args)
          | None ->
              let id, type_env = TypeEnvi.next_decl_id type_env in
              let num_args = ref None in
              let type_env =
                { type_env with
                  predeclared_types=
                    Map.add_exn ~key:name
                      ~data:(id, ref None, lid.loc)
                      type_env.predeclared_types }
              in
              env.resolve_env.type_env <- type_env ;
              (id, num_args)
        in
        { tdec_ident= Location.mkloc name lid.loc
        ; tdec_params= []
        ; tdec_implicit_params= []
        ; tdec_desc= TForward num_args
        ; tdec_id= id }
    | _ ->
        raise (Error (lid.loc, Unbound_type lid.txt)) )

let add_module_type mode name m =
  map_current_scope
    ~f:(FullScope.map_scope mode ~f:(Scope.add_module_type name m))

let find_module_type =
  find_of_lident ~kind:"module type" ~get_name:Scope.get_module_type

let find_preferred_name mode id env =
  List.find_map env.scope_stack
    ~f:(FullScope.on_scope_opt mode ~f:(Scope.get_preferred_type_name id))

module Type = struct
  type env = t

  let mk' mode env depth type_desc =
    let type_id, type_env = TypeEnvi.next_type_id env.resolve_env.type_env in
    env.resolve_env.type_env <- type_env ;
    {type_desc; type_id; type_depth= depth; type_mode= mode}

  let mk mode type_desc env = mk' mode env env.depth type_desc

  let mkvar mode ?(explicitness = Explicit) name env =
    mk mode (Tvar (name, explicitness)) env

  let instance env typ = TypeEnvi.instance env.resolve_env.type_env typ

  let map_env ~f env = env.resolve_env.type_env <- f env.resolve_env.type_env

  let add_instance typ typ' = map_env ~f:(TypeEnvi.add_instance typ typ')

  let clear_instance typ = map_env ~f:(TypeEnvi.clear_instance typ)

  let refresh_var mode ~loc ?must_find env typ =
    match typ.type_desc with
    | Tvar (None, explicitness) -> (
      match (must_find, explicitness) with
      | Some true, Explicit ->
          raise (Error (loc, Unbound_type_var typ))
      | _ ->
          (env, mkvar mode ~explicitness None env) )
    | Tvar ((Some {txt= x; _} as name), explicitness) -> (
        let var =
          match must_find with
          | Some true ->
              let var = find_type_variable x mode env in
              if (not (Option.is_some var)) && explicitness = Explicit then
                raise (Error (loc, Unbound_type_var typ)) ;
              var
          | Some false ->
              None
          | None ->
              find_type_variable x mode env
        in
        match var with
        | Some var ->
            (env, var)
        | None ->
            let var = mkvar mode ~explicitness name env in
            (add_type_variable x var env, var) )
    | _ ->
        raise (Error (loc, Expected_type_var typ))

  let refresh_vars mode ~loc vars new_vars_map env =
    let env, new_vars =
      List.fold_map vars ~init:env ~f:(refresh_var mode ~loc ~must_find:false)
    in
    let new_vars_map =
      List.fold2_exn ~init:new_vars_map vars new_vars
        ~f:(fun map var new_var -> Map.set map ~key:var.type_id ~data:new_var
      )
    in
    (new_vars, new_vars_map, env)

  let rec copy mode ~loc typ new_vars_map env =
    let mode = if typ.type_mode = OCaml then mode else typ.type_mode in
    let copy = copy mode ~loc in
    match typ.type_desc with
    | Tvar _ -> (
      match Map.find new_vars_map typ.type_id with
      | Some var ->
          var
      | None ->
          typ )
    | Tpoly (vars, typ) ->
        let _vars, new_vars_map, env =
          refresh_vars mode ~loc vars new_vars_map env
        in
        copy typ new_vars_map env
    | Tctor {var_decl= {tdec_desc= TUnfold typ; _}; _} ->
        typ
    | Tctor ({var_params; var_implicit_params; _} as variant) ->
        let var_params =
          List.map var_params ~f:(fun t -> copy t new_vars_map env)
        in
        let var_implicit_params =
          List.map var_implicit_params ~f:(fun t -> copy t new_vars_map env)
        in
        mk mode (Tctor {variant with var_params; var_implicit_params}) env
    | Ttuple typs ->
        let typs = List.map typs ~f:(fun t -> copy t new_vars_map env) in
        mk mode (Ttuple typs) env
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1 = copy typ1 new_vars_map env in
        let typ2 = copy typ2 new_vars_map env in
        mk mode (Tarrow (typ1, typ2, explicit, label)) env

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

  let type_vars ?depth typ =
    let deep_enough =
      match depth with
      | Some depth ->
          fun typ -> depth <= typ.type_depth
      | None ->
          fun _ -> true
    in
    let empty = Set.empty (module Comparator) in
    let rec type_vars set typ =
      match typ.type_desc with
      | Tvar _ when deep_enough typ ->
          Set.add set typ
      | Tpoly (vars, typ) ->
          let poly_vars = List.fold ~init:empty vars ~f:type_vars in
          Set.union set (Set.diff (type_vars empty typ) poly_vars)
      | _ ->
          fold ~init:set typ ~f:type_vars
    in
    type_vars empty typ

  let rec update_depths env typ =
    Type0.update_depth env.depth typ ;
    match typ.type_desc with
    | Tvar _ ->
        Option.iter ~f:(update_depths env) (instance env typ)
    | _ ->
        Type0.iter ~f:(update_depths env) typ

  let rec flatten typ env =
    let mk' = mk' typ.type_mode env typ.type_depth in
    match typ.type_desc with
    | Tvar _ -> (
      match instance env typ with
      | Some typ' ->
          clear_instance typ env ;
          let flattened_typ = flatten typ' env in
          add_instance typ typ' env ; flattened_typ
      | None ->
          typ )
    | Tpoly (vars, typ) ->
        let var_set =
          Set.union_list
            (module Comparator)
            (List.map vars ~f:(type_vars ~depth:env.depth))
        in
        let typ = flatten typ env in
        mk' (Tpoly (Set.to_list var_set, typ))
    | Tctor variant ->
        let var_params =
          List.map variant.var_params ~f:(fun typ -> flatten typ env)
        in
        let var_implicit_params =
          List.map variant.var_implicit_params ~f:(fun typ -> flatten typ env)
        in
        mk' (Tctor {variant with var_params; var_implicit_params})
    | Ttuple typs ->
        let typs = List.map typs ~f:(fun typ -> flatten typ env) in
        mk' (Ttuple typs)
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1 = flatten typ1 env in
        let typ2 = flatten typ2 env in
        mk' (Tarrow (typ1, typ2, explicit, label))

  let or_compare cmp ~f = if Int.equal cmp 0 then f () else cmp

  let compare_label label1 label2 =
    match (label1, label2) with
    | Asttypes.Nolabel, Asttypes.Nolabel ->
        0
    | Nolabel, _ ->
        -1
    | _, Nolabel ->
        1
    | Labelled x, Labelled y ->
        String.compare x y
    | Labelled _, _ ->
        -1
    | _, Labelled _ ->
        1
    | Optional x, Optional y ->
        String.compare x y

  let rec compare typ1 typ2 =
    if Int.equal typ1.type_id typ2.type_id then 0
    else
      match (typ1.type_desc, typ2.type_desc) with
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
      | ( Tctor
            { var_decl= {tdec_id= id1; _}
            ; var_params= params1
            ; var_length= length1
            ; _ }
        , Tctor
            { var_decl= {tdec_id= id2; _}
            ; var_params= params2
            ; var_length= length2
            ; _ } ) ->
          or_compare (Int.compare id1 id2) ~f:(fun () ->
              or_compare
                ([%compare: int option] length1 length2)
                ~f:(fun () -> compare_all params1 params2) )
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
          or_compare (compare_label label1 label2) ~f:(fun () ->
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
        Type0.fold ~init:set ~f:(weak_variables depth) typ

  let get_implicits acc typ =
    let rec get_implicits accept_labelless acc typ =
      match typ.type_desc with
      | Tarrow (typ1, typ2, Implicit, label)
        when accept_labelless
             || match label with Labelled _ -> true | _ -> false ->
          get_implicits accept_labelless ((label, typ1) :: acc) typ2
      | Tarrow (typ1, typ2, _, label) ->
          let acc, typ = get_implicits false acc typ2 in
          ( acc
          , { type_desc= Tarrow (typ1, typ, Explicit, label)
            ; type_id= next_id ()
            ; type_depth= typ.type_depth
            ; type_mode= typ.type_mode } )
      | _ ->
          (List.rev acc, typ)
    in
    get_implicits true acc typ

  let rec add_implicits mode env implicits_list typ =
    match implicits_list with
    | [] ->
        typ
    | (label, typ1) :: implicits_list ->
        add_implicits mode env implicits_list
          (mk mode (Tarrow (typ1, typ, Implicit, label)) env)

  let new_implicit_var ?(loc = Location.none) typ env =
    let {TypeEnvi.implicit_vars; implicit_id; _} = env.resolve_env.type_env in
    let mk exp_loc exp_desc = {Parsetypes.exp_loc; exp_desc; exp_type= typ} in
    let name = Location.mkloc (sprintf "__implicit%i__" implicit_id) loc in
    let new_exp =
      mk loc (Unifiable {expression= None; name; id= implicit_id})
    in
    env.resolve_env.type_env
    <- { env.resolve_env.type_env with
         implicit_vars= new_exp :: implicit_vars
       ; implicit_id= implicit_id + 1 } ;
    new_exp

  let is_concrete_typ_t env typ =
    match typ.type_desc with
    | Tctor {var_params= [typ1; typ2]; var_decl; _} -> (
      match find_preferred_name OCaml var_decl.tdec_id env with
      | Some (Ldot (Lident "Typ", "t")) -> (
        match (typ1.type_desc, typ2.type_desc) with
        | Tvar _, Tvar _ ->
            false
        | _ ->
            true )
      | _ ->
          false )
    | _ ->
        false

  let rec strip_implicits typ =
    match typ.type_desc with
    | Tarrow (_, typ, Implicit, _) ->
        strip_implicits typ
    | _ ->
        typ

  (* Invariant: This assumes that typ1 is a known concrete Typ.t. *)
  let is_same_concrete_typ_t env typ1 typ2 =
    let typ1 = flatten typ1 env in
    let typ2 = strip_implicits (flatten typ2 env) in
    match (typ1.type_desc, typ2.type_desc) with
    | ( Tctor {var_params= [typ1a; typ1b]; _}
      , Tctor {var_decl; var_params= [typ2a; typ2b]; _} ) -> (
      match find_preferred_name OCaml var_decl.tdec_id env with
      | Some (Ldot (Lident "Typ", "t")) -> (
          (* Note: we don't check that the type parameters typ2a, typ2b are
             non-variables, since it is impossible to construct this instance.
          *)
          (* WARNING: Bad hack! A type alias may resolve to a variable type
             such as [type t('a) = 'a] and here we will recognise it as a
             stable constructor.
             This should be mostly mitigated by the fact that the instance
             matches. I hope.
          *)
          ( match (typ1a.type_desc, typ2a.type_desc) with
          | ( Tctor {var_decl= {tdec_id= id1; _}; _}
            , Tctor {var_decl= {tdec_id= id2; _}; _} )
            when id1 = id2 ->
              true
          | _ ->
              false )
          ||
          match (typ1b.type_desc, typ2b.type_desc) with
          | ( Tctor {var_decl= {tdec_id= id1; _}; _}
            , Tctor {var_decl= {tdec_id= id2; _}; _} )
            when id1 = id2 ->
              true
          | _ ->
              false )
      | _ ->
          false )
    | _ ->
        false

  let implicit_instances mode ~loc
      ~(is_subtype : env -> type_expr -> of_:type_expr -> bool)
      ~(unify : env -> type_expr -> type_expr -> unit) (typ : type_expr) env =
    let is_concrete_typ_t = is_concrete_typ_t env typ in
    let _ignore_unify = unify in
    List.filter_map env.resolve_env.type_env.instances
      ~f:(fun (id, canonical_path, instance_typ) ->
        let instance_typ = copy mode ~loc instance_typ Int.Map.empty env in
        if
          is_concrete_typ_t
          && is_same_concrete_typ_t env typ instance_typ
          && (* HACK *)
             (*unify env typ instance_typ;*) true
          || is_subtype env typ ~of_:instance_typ
        then
          List.find_map env.scope_stack
            ~f:
              (FullScope.on_scope_opt mode ~f:(fun {instances; _} ->
                   match Map.find instances id with
                   | Some path ->
                       Some (path, instance_typ)
                   | None ->
                       Some (canonical_path, instance_typ) ))
        else None )

  let generate_implicits e env =
    let loc = e.Parsetypes.exp_loc in
    let implicits, typ = get_implicits [] e.exp_type in
    match implicits with
    | [] ->
        e
    | _ ->
        let es =
          List.map implicits ~f:(fun (label, typ) ->
              (label, new_implicit_var ~loc typ env) )
        in
        {exp_loc= loc; exp_type= typ; exp_desc= Apply (e, es)}

  let rec instantiate_implicits mode ~loc ~is_subtype ~unify implicit_vars env
      =
    let env_implicits = env.resolve_env.type_env.implicit_vars in
    env.resolve_env.type_env
    <- {env.resolve_env.type_env with implicit_vars= []} ;
    let implicit_vars =
      List.filter implicit_vars
        ~f:(fun ({Parsetypes.exp_loc; exp_type; _} as exp) ->
          match
            implicit_instances mode ~loc ~is_subtype ~unify exp_type env
          with
          (* TODO: Re-enable multiple instances being an error. *)
          | (name, instance_typ) :: _xs ->
              let name = Location.mkloc name exp_loc in
              let e =
                generate_implicits
                  {exp_loc; exp_type= instance_typ; exp_desc= Variable name}
                  env
              in
              ( match exp.exp_desc with
              | Unifiable desc ->
                  desc.expression <- Some e
              | _ ->
                  raise (Error (exp.exp_loc, No_unifiable_implicit)) ) ;
              false
          | [] ->
              true )
    in
    let new_implicits = env.resolve_env.type_env.implicit_vars in
    env.resolve_env.type_env
    <- {env.resolve_env.type_env with implicit_vars= env_implicits} ;
    match new_implicits with
    | [] ->
        implicit_vars
    | _ ->
        instantiate_implicits mode ~loc ~is_subtype ~unify
          (new_implicits @ implicit_vars)
          env

  let flattened_implicit_vars mode ~loc ~toplevel ~is_subtype ~unify typ_vars
      env =
    let is_subtype env typ ~of_:ctyp =
      is_subtype env typ ~of_:(snd (get_implicits [] ctyp))
    in
    let {TypeEnvi.implicit_vars; _} = env.resolve_env.type_env in
    let implicit_vars =
      List.map implicit_vars ~f:(fun exp ->
          {exp with exp_type= flatten exp.exp_type env} )
    in
    let implicit_vars =
      instantiate_implicits mode ~loc ~is_subtype ~unify implicit_vars env
    in
    let implicit_vars =
      List.dedup_and_sort implicit_vars ~compare:(fun exp1 exp2 ->
          let cmp =
            compare exp1.Parsetypes.exp_type exp2.Parsetypes.exp_type
          in
          ( if Int.equal cmp 0 then
            match (exp1.exp_desc, exp2.exp_desc) with
            | Unifiable desc1, Unifiable desc2 ->
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
      let weak_vars_set = ref (Set.empty (module Comparator)) in
      let strong_implicit_vars, weak_implicit_vars =
        List.partition_tf implicit_vars ~f:(fun {exp_type; _} ->
            if !consider_weak then
              let weak_vars =
                weak_variables env.depth
                  (Set.empty (module Comparator))
                  exp_type
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
                       Type0.equal_at_depth ~depth:env.depth e_weak.exp_type
                         e_strong.exp_type
                     then (
                       ignore
                         (is_subtype env e_strong.exp_type ~of_:e_weak.exp_type) ;
                       ( match e_weak.exp_desc with
                       | Unifiable desc ->
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

  let implicit_params _env typ =
    let rec implicit_params set typ =
      match typ.type_desc with
      | Tvar (_, Implicit) ->
          Set.add set typ
      | Tpoly (_, typ) ->
          implicit_params set typ
      | _ ->
          fold ~init:set typ ~f:implicit_params
    in
    implicit_params (Set.empty (module Comparator)) typ

  let rec constr_map mode env ~f typ =
    match typ.type_desc with
    | Tvar _ ->
        typ
    | Ttuple typs ->
        let typs = List.map ~f:(constr_map mode env ~f) typs in
        mk mode (Ttuple typs) env
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1 = constr_map mode env ~f typ1 in
        let typ2 = constr_map mode env ~f typ2 in
        mk mode (Tarrow (typ1, typ2, explicit, label)) env
    | Tctor variant ->
        let var_params =
          List.map ~f:(constr_map mode env ~f) variant.var_params
        in
        let var_implicit_params =
          List.map ~f:(constr_map mode env ~f) variant.var_implicit_params
        in
        mk mode (f {variant with var_params; var_implicit_params}) env
    | Tpoly (typs, typ) ->
        mk mode (Tpoly (typs, constr_map mode env ~f typ)) env

  let variant_normalise_constr_names mode env variant =
    match find_preferred_name mode variant.var_decl.tdec_id env with
    | Some ident ->
        {variant with var_ident= {txt= ident; loc= variant.var_ident.loc}}
    | None ->
        variant

  let normalise_one_constr mode env variant =
    Tctor (variant_normalise_constr_names mode env variant)

  let normalise_constr_names mode env typ =
    constr_map mode env typ ~f:(normalise_one_constr mode env)

  let rec bubble_label_aux mode env label typ =
    match typ.type_desc with
    | Tarrow (typ1, typ2, explicit, arr_label)
      when Int.equal (compare_label label arr_label) 0 ->
        (Some (typ1, explicit, arr_label), typ2)
    | Tarrow (typ1, typ2, explicit, arr_label)
      when match (label, arr_label) with
           | Labelled lbl, Optional arr_lbl ->
               String.equal lbl arr_lbl
           | _ ->
               false ->
        (Some (typ1, explicit, label), typ2)
    | Tarrow (typ1, typ2, explicit, arr_label) -> (
      match bubble_label_aux mode env label typ2 with
      | None, _ ->
          (None, typ)
      | res, typ2 ->
          (res, mk mode (Tarrow (typ1, typ2, explicit, arr_label)) env) )
    | _ ->
        (None, typ)

  let bubble_label mode env label typ =
    match bubble_label_aux mode env label typ with
    | Some (typ1, explicit, arr_label), typ2 ->
        mk mode (Tarrow (typ1, typ2, explicit, arr_label)) env
    | None, typ ->
        typ

  let discard_optional_labels typ =
    let rec go typ' =
      match typ'.type_desc with
      | Tarrow (_, typ2, _, Optional _) ->
          go typ2
      | Tarrow (_, _, _, _) ->
          typ
      | _ ->
          typ'
    in
    go typ

  let is_arrow typ =
    match typ.type_desc with
    | Tarrow _ | Tpoly (_, {type_desc= Tarrow _; _}) ->
        true
    | _ ->
        false

  let get_rev_args typ =
    let rec get_args acc typ =
      match typ.type_desc with
      | Tarrow (typ1, typ2, _, _)
      | Tpoly (_, {type_desc= Tarrow (typ1, typ2, _, _); _}) ->
          get_args (typ1 :: acc) typ2
      | _ ->
          (acc, typ)
    in
    get_args [] typ
end

module TypeDecl = struct
  let next_id env =
    let tdec_id, type_env = TypeEnvi.next_decl_id env.resolve_env.type_env in
    env.resolve_env.type_env <- type_env ;
    tdec_id

  let mk ~name ~params ?(implicit_params = []) desc env =
    let tdec_id = next_id env in
    { tdec_ident= name
    ; tdec_params= params
    ; tdec_implicit_params= implicit_params
    ; tdec_desc= desc
    ; tdec_id }

  let mk_typ mode ~params ?ident ?length decl =
    let ident = Option.value ident ~default:(mk_lid decl.tdec_ident) in
    Type.mk mode
      (Tctor
         { var_ident= ident
         ; var_params= params
         ; var_implicit_params= []
         ; var_decl= decl
         ; var_length= length })

  let find_of_type ~loc typ env =
    let open Option.Let_syntax in
    let%map variant =
      match typ.type_desc with Tctor variant -> Some variant | _ -> None
    in
    let decl = variant.var_decl in
    let bound_vars =
      match
        List.fold2 ~init:Int.Map.empty variant.var_params decl.tdec_params
          ~f:(fun bound_vars param var ->
            Map.set bound_vars ~key:var.type_id ~data:param )
      with
      | Ok bound_vars ->
          bound_vars
      | Unequal_lengths ->
          raise
            (Error
               ( loc
               , Wrong_number_args
                   ( variant.var_ident.txt
                   , List.length decl.tdec_params
                   , List.length variant.var_params ) ))
    in
    (decl, bound_vars, env)

  let find_of_field mode (field : lid) env =
    find_of_lident mode ~kind:"field" ~get_name:Scope.get_field field env

  let find_of_constructor mode (ctor : lid) env =
    find_of_lident mode ~kind:"constructor" ~get_name:Scope.get_ctor ctor env

  let unfold_alias mode ~loc typ env =
    match find_of_type ~loc typ env with
    | Some ({tdec_desc= TUnfold typ'; _}, _, _) when not (phys_equal typ typ')
      ->
        Some typ'
    | Some ({tdec_desc= TAlias alias_typ; _}, bound_vars, env) ->
        Some (Type.copy mode ~loc alias_typ bound_vars env)
    | _ ->
        None

  let rec find_unaliased_of_type mode ~loc typ env =
    match find_of_type ~loc typ env with
    | Some ({tdec_desc= TUnfold typ'; _}, _, _) when not (phys_equal typ typ')
      ->
        find_unaliased_of_type mode ~loc typ' env
    | Some ({tdec_desc= TAlias alias_typ; _}, bound_vars, env) ->
        let typ = Type.copy mode ~loc alias_typ bound_vars env in
        find_unaliased_of_type mode ~loc typ env
    | ret ->
        ret

  let field_constr_map mode env ~f field_decl =
    {field_decl with fld_type= Type.constr_map mode env ~f field_decl.fld_type}

  let field_normalise_constr_names mode env typ =
    field_constr_map mode env typ ~f:(Type.normalise_one_constr mode env)

  let rec constr_map mode env ~f decl =
    match decl.tdec_desc with
    | TAbstract | TOpen | TForward _ | TUnfold _ ->
        decl
    | TRecord field_decls ->
        { decl with
          tdec_desc=
            TRecord (List.map ~f:(field_constr_map mode env ~f) field_decls) }
    | TVariant ctors ->
        { decl with
          tdec_desc=
            TVariant (List.map ~f:(ctor_arg_constr_map mode env ~f) ctors) }
    | TAlias typ ->
        {decl with tdec_desc= TAlias (Type.constr_map mode env ~f typ)}
    | TExtend (lid, type_decl, ctors) ->
        { decl with
          tdec_desc=
            TExtend
              ( lid
              , constr_map mode env ~f type_decl
              , List.map ~f:(ctor_arg_constr_map mode env ~f) ctors ) }

  and ctor_arg_constr_map mode env ~f ctor =
    let ctor_args =
      match ctor.ctor_args with
      | Ctor_tuple typs ->
          Ctor_tuple (List.map ~f:(Type.constr_map mode env ~f) typs)
      | Ctor_record decl ->
          Ctor_record (constr_map mode env ~f decl)
    in
    let ctor_ret = Option.map ~f:(Type.constr_map mode env ~f) ctor.ctor_ret in
    {ctor with ctor_args; ctor_ret}

  let normalise_constr_names mode env typ =
    constr_map mode env typ ~f:(Type.normalise_one_constr mode env)

  let ctor_arg_normalise_constr_names mode env typ =
    ctor_arg_constr_map mode env typ ~f:(Type.normalise_one_constr mode env)
end

let add_name (name : str) typ =
  map_current_scope ~f:(FullScope.map_scopes ~f:(Scope.add_name name.txt typ))

let add_name_raw mode (name : str) typ id =
  map_current_scope
    ~f:(FullScope.map_scope mode ~f:(Scope.add_name_raw name.txt typ id))

let find_name_poly ~loc mode (lid : lid) env =
  match find_of_lident ~kind:"name" mode ~get_name:Scope.get_name lid env with
  | Some (typ, i) ->
      let vars, new_vars_map, typ =
        match typ.type_desc with
        | Tpoly (vars, typ) ->
            let vars, new_vars_map, _env =
              Type.refresh_vars mode ~loc vars Int.Map.empty env
            in
            (vars, new_vars_map, typ)
        | _ ->
            ([], Int.Map.empty, typ)
      in
      let typ = Type.copy ~loc mode typ new_vars_map env in
      let typ =
        match vars with [] -> typ | _ -> Type.mk mode (Tpoly (vars, typ)) env
      in
      (typ, i)
  | None ->
      raise (Error (lid.loc, Unbound_value lid.txt))

let find_name ~loc mode (lid : lid) env =
  match find_of_lident ~kind:"name" mode ~get_name:Scope.get_name lid env with
  | Some (typ, i) ->
      (Type.copy ~loc mode typ Int.Map.empty env, i)
  | None ->
      raise (Error (lid.loc, Unbound_value lid.txt))

(* Error handling *)

open Format

let pp_typ = Typeprint.type_expr

let pp_decl_typ ppf decl =
  pp_typ ppf
    { type_desc=
        Tctor
          { var_ident= mk_lid decl.tdec_ident
          ; var_params= decl.tdec_params
          ; var_implicit_params= decl.tdec_implicit_params
          ; var_decl= decl
          ; var_length= None (* TODO *) }
    ; type_id= -1
    ; type_depth= -1
    ; type_mode= OCaml }

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
  | Wrong_number_args (lid, given, expected) ->
      fprintf ppf
        "@[The type constructor @[<h>%a@] expects %d argument(s)@ but is here \
         applied to %d argument(s).@]"
        Longident.pp lid expected given
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
        (pp_print_list ~pp_sep:pp_print_space pp_print_string)
        types
  | Functor_in_module_sig ->
      fprintf ppf
        "Internal error: Bare functor found as part of a module signature."
  | Not_a_functor ->
      fprintf ppf "This module is not a functor."

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
    | _ ->
        None )
