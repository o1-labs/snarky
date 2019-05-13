open Core_kernel
open Parsetypes
open Longident

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

type 'a name_map = (string, 'a, String.comparator_witness) Map.t

type 'a int_map = (int, 'a, Int.comparator_witness) Map.t

type 'a lid_map = (Longident.t, 'a, Longident.comparator_witness) Map.t

module TypeEnvi = struct
  type t =
    { type_id: int
    ; type_decl_id: int
    ; instance_id: int
    ; variable_instances: type_expr int_map
    ; implicit_vars: expression list
    ; implicit_id: int
    ; type_decls: type_decl int_map
    ; instances: (int * type_expr) list
    ; predeclared_types:
        (int (* id *) * int option ref (* num. args *) * Location.t) name_map
    }

  let empty =
    { type_id= 1
    ; type_decl_id= 1
    ; instance_id= 1
    ; variable_instances= Map.empty (module Int)
    ; implicit_id= 1
    ; implicit_vars= []
    ; type_decls= Map.empty (module Int)
    ; instances= []
    ; predeclared_types= Map.empty (module String) }

  let instance env (typ : type_expr) =
    Map.find env.variable_instances typ.type_id

  let add_instance (typ : type_expr) typ' env =
    { env with
      variable_instances=
        Map.set env.variable_instances ~key:typ.type_id ~data:typ' }

  let clear_instance (typ : type_expr) env =
    {env with variable_instances= Map.remove env.variable_instances typ.type_id}

  let next_type_id env = (env.type_id, {env with type_id= env.type_id + 1})

  let next_decl_id env =
    (env.type_decl_id, {env with type_decl_id= env.type_decl_id + 1})

  let decl env (ctor : variant) = Map.find env.type_decls ctor.var_decl_id

  let add_decl (decl : type_decl) env =
    {env with type_decls= Map.set env.type_decls ~key:decl.tdec_id ~data:decl}

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
  ; mutable external_modules: 'a or_deferred name_map
  ; mutable predeclare_types: bool }

module Scope = struct
  type 't kind = Module | Expr | Open | Continue | Functor of ('t -> 't)

  type 'a or_path = Immediate of 'a | Deferred of Longident.t

  type t =
    { kind: t kind
    ; names: type_expr name_map
    ; type_variables: type_expr name_map
    ; type_decls: type_decl name_map
    ; fields: (type_decl * int) name_map
    ; ctors: (type_decl * int) name_map
    ; modules: t or_path name_map
    ; instances: Longident.t int_map }

  let load_module :
      (loc:Location.t -> name:string -> t resolve_env -> string -> t) ref =
    ref (fun ~loc ~name _env _filename ->
        raise (Error (loc, Unbound_module (Lident name))) )

  let empty kind =
    { kind
    ; names= Map.empty (module String)
    ; type_variables= Map.empty (module String)
    ; type_decls= Map.empty (module String)
    ; fields= Map.empty (module String)
    ; ctors= Map.empty (module String)
    ; modules= Map.empty (module String)
    ; instances= Map.empty (module Int) }

  let add_name key typ scope =
    {scope with names= Map.set scope.names ~key ~data:typ}

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

  let add_type_declaration decl scope =
    { scope with
      type_decls= Map.set scope.type_decls ~key:decl.tdec_ident.txt ~data:decl
    }

  let get_type_declaration name scope = Map.find scope.type_decls name

  let register_type_declaration decl scope =
    let {type_decls; _} = scope in
    let scope =
      { scope with
        type_decls= Map.set type_decls ~key:decl.tdec_ident.txt ~data:decl }
    in
    match decl.tdec_desc with
    | TAbstract | TAlias _ | TUnfold _ | TOpen | TForward _ ->
        scope
    | TRecord fields ->
        List.foldi ~f:(add_field decl) ~init:scope fields
    | TVariant ctors ->
        List.foldi ~f:(add_ctor decl) ~init:scope ctors
    | TExtend (_, _, ctors) ->
        (* Don't add the identifier to the scope *)
        let scope = {scope with type_decls} in
        List.foldi ~f:(add_ctor decl) ~init:scope ctors

  let fold_over ~init:acc ~names ~type_variables ~type_decls ~fields ~ctors
      ~modules ~instances
      { kind= _
      ; names= names1
      ; type_variables= type_variables1
      ; type_decls= type_decls1
      ; fields= fields1
      ; ctors= ctors1
      ; modules= modules1
      ; instances= instances1 }
      { kind= _
      ; names= names2
      ; type_variables= type_variables2
      ; type_decls= type_decls2
      ; fields= fields2
      ; ctors= ctors2
      ; modules= modules2
      ; instances= instances2 } =
    let acc =
      Map.fold2 type_variables1 type_variables2 ~init:acc ~f:type_variables
    in
    let acc = Map.fold2 type_decls1 type_decls2 ~init:acc ~f:type_decls in
    let acc = Map.fold2 ctors1 ctors2 ~init:acc ~f:ctors in
    let acc = Map.fold2 fields1 fields2 ~init:acc ~f:fields in
    let acc = Map.fold2 modules1 modules2 ~init:acc ~f:modules in
    let acc = Map.fold2 instances1 instances2 ~init:acc ~f:instances in
    let acc = Map.fold2 names1 names2 ~init:acc ~f:names in
    acc

  let join ~loc
      { kind
      ; names= names1
      ; type_variables= type_variables1
      ; type_decls= type_decls1
      ; fields= fields1
      ; ctors= ctors1
      ; modules= modules1
      ; instances= instances1 }
      { kind= _
      ; names= names2
      ; type_variables= type_variables2
      ; type_decls= type_decls2
      ; fields= fields2
      ; ctors= ctors2
      ; modules= modules2
      ; instances= instances2 } =
    { kind
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
    ; instances=
        Map.merge_skewed instances1 instances2 ~combine:(fun ~key:_ _ v -> v)
    }

  let add_module name m scope =
    {scope with modules= Map.set scope.modules ~key:name ~data:m}

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

  let rec find_module_ ~loc ~scopes resolve_env lid scope =
    match lid with
    | Lident name ->
        get_module ~loc ~scopes resolve_env name scope
    | Ldot (path, name) ->
        Option.bind
          (find_module_ ~loc ~scopes resolve_env path scope)
          ~f:(get_module ~loc ~scopes resolve_env name)
    | Lapply (fpath, path) ->
        Option.map
          (find_module_ ~loc ~scopes resolve_env fpath scope)
          ~f:(apply_functor ~loc ~scopes resolve_env path)

  and apply_functor ~loc ~scopes resolve_env lid scope =
    let f =
      match scope.kind with
      | Functor f ->
          f
      | _ ->
          raise (Error (loc, Not_a_functor))
    in
    let m = find_module ~loc lid resolve_env scopes in
    f m

  and get_module ~loc ~scopes resolve_env name scope =
    match Map.find scope.modules name with
    | Some (Immediate m) ->
        Some m
    | Some (Deferred lid) ->
        get_global_module ~loc ~scopes resolve_env lid
    | None ->
        None

  and get_global_module ~loc ~scopes resolve_env lid =
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
    match (m, lid) with
    | Some m, Some lid ->
        find_module_ ~loc ~scopes resolve_env lid m
    | Some m, None ->
        Some m
    | None, _ ->
        None

  and find_module ~loc lid resolve_env scopes =
    match
      List.find_map ~f:(find_module_ ~loc ~scopes resolve_env lid) scopes
    with
    | Some m ->
        m
    | None -> (
      match get_global_module ~loc ~scopes resolve_env lid with
      | Some m ->
          m
      | None ->
          raise (Error (loc, Unbound_module lid)) )

  let rec find_module_deferred ~loc ~scopes resolve_env lid scope =
    match lid with
    | Lident name ->
        Map.find scope.modules name
    | Ldot (path, name) -> (
      match find_module_deferred ~loc ~scopes resolve_env path scope with
      | Some (Immediate m) ->
          Map.find m.modules name
      | Some (Deferred lid) ->
          Some (Deferred (Ldot (lid, name)))
      | None ->
          None )
    | Lapply (lid1, lid2) ->
        (* Don't defer functor applications *)
        let m1 = find_module ~loc lid1 resolve_env scopes in
        let f =
          match m1.kind with
          | Functor f ->
              f
          | _ ->
              raise (Error (loc, Not_a_functor))
        in
        Some (Immediate (f (find_module ~loc lid2 resolve_env scopes)))
end

let empty_resolve_env : Scope.t resolve_env =
  { type_env= TypeEnvi.empty
  ; external_modules= Map.empty (module String)
  ; predeclare_types= false }

type t =
  {scope_stack: Scope.t list; depth: int; resolve_env: Scope.t resolve_env}

let empty resolve_env =
  {scope_stack= [Scope.empty Scope.Module]; depth= 0; resolve_env}

let current_scope {scope_stack; _} =
  match List.hd scope_stack with
  | Some scope ->
      scope
  | None ->
      raise (Error (Location.none, No_open_scopes))

let push_scope scope env =
  {env with scope_stack= scope :: env.scope_stack; depth= env.depth + 1}

let make_functor f = Scope.empty (Functor f)

let open_expr_scope = push_scope Scope.(empty Expr)

let open_module = push_scope Scope.(empty Module)

let open_namespace_scope scope env =
  env
  |> push_scope {scope with kind= Scope.Open}
  |> push_scope Scope.(empty Continue)

let pop_scope env =
  match env.scope_stack with
  | [] ->
      raise (Error (Location.none, No_open_scopes))
  | scope :: scope_stack ->
      (scope, {env with scope_stack; depth= env.depth - 1})

let pop_expr_scope env =
  let scope, env = pop_scope env in
  match scope.Scope.kind with
  | Scope.Expr ->
      (scope, env)
  | _ ->
      raise (Error (Location.none, Wrong_scope_kind "expression"))

let pop_module ~loc env =
  let rec all_scopes scopes env =
    let scope, env = pop_scope env in
    match scope.kind with
    | Scope.Module ->
        (scope :: scopes, env)
    | Expr ->
        raise (Error (Location.none, Wrong_scope_kind "module"))
    | Open ->
        all_scopes scopes env
    | Continue ->
        all_scopes (scope :: scopes) env
    | Functor _ ->
        if List.is_empty scopes then ([scope], env)
        else raise (Error (Location.none, Functor_in_module_sig))
  in
  let scopes, env = all_scopes [] env in
  let m =
    List.fold_left ~init:Scope.(empty Module) scopes ~f:(Scope.join ~loc)
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
      raise (Error (Location.none, No_open_scopes))

let add_type_variable name typ =
  map_current_scope ~f:(Scope.add_type_variable name typ)

let find_type_variable name env =
  List.find_map ~f:(Scope.find_type_variable name) env.scope_stack

let add_module (name : str) m =
  map_current_scope ~f:(fun scope ->
      let scope = Scope.add_module name.txt (Scope.Immediate m) scope in
      { scope with
        instances=
          Map.merge scope.instances m.instances ~f:(fun ~key:_ data ->
              match data with
              | `Left x ->
                  Some x
              | `Both (_, x) | `Right x ->
                  Some (Longident.add_outer_module name.txt x) ) } )

let add_deferred_module (name : str) lid =
  map_current_scope ~f:(Scope.add_module name.txt (Scope.Deferred lid))

let register_external_module name x env =
  env.resolve_env.external_modules
  <- Map.set ~key:name ~data:x env.resolve_env.external_modules

let find_module ~loc (lid : lid) env =
  Scope.find_module ~loc lid.txt env.resolve_env env.scope_stack

let find_module_deferred ~loc (lid : lid) env =
  List.find_map
    ~f:
      (Scope.find_module_deferred ~loc ~scopes:env.scope_stack env.resolve_env
         lid.txt)
    env.scope_stack

let add_implicit_instance name typ env =
  let path = Lident name in
  let id, type_env = TypeEnvi.next_instance_id env.resolve_env.type_env in
  let env =
    map_current_scope env ~f:(fun scope ->
        {scope with instances= Map.set ~key:id ~data:path scope.instances} )
  in
  env.resolve_env.type_env <- TypeEnvi.add_implicit_instance id typ type_env ;
  env

let find_of_lident ~kind ~get_name (lid : lid) env =
  let loc = lid.loc in
  let full_get_name =
    match lid.txt with
    | Lident name ->
        fun scope -> get_name name scope
    | Ldot (path, name) ->
        fun scope ->
          Option.bind ~f:(get_name name)
            (Scope.find_module_ ~loc ~scopes:env.scope_stack env.resolve_env
               path scope)
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
          Scope.get_global_module ~loc ~scopes:env.scope_stack env.resolve_env
            path
        in
        Option.bind m ~f:(get_name name)
    | _ ->
        None )

let raw_find_type_declaration (lid : lid) env =
  match
    find_of_lident ~kind:"type" ~get_name:Scope.get_type_declaration lid env
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
        ; tdec_id= id
        ; tdec_loc= lid.loc }
    | _ ->
        raise (Error (lid.loc, Unbound_type lid.txt)) )

module Type = struct
  type env = t

  let mk ~loc type_desc env =
    let type_id, type_env = TypeEnvi.next_type_id env.resolve_env.type_env in
    env.resolve_env.type_env <- type_env ;
    {type_desc; type_id; type_loc= loc}

  let mkvar ~loc ?(explicitness = Explicit) name env =
    mk ~loc (Tvar (name, env.depth, explicitness)) env

  let instance env typ = TypeEnvi.instance env.resolve_env.type_env typ

  let map_env ~f env = env.resolve_env.type_env <- f env.resolve_env.type_env

  let add_instance typ typ' = map_env ~f:(TypeEnvi.add_instance typ typ')

  let clear_instance typ = map_env ~f:(TypeEnvi.clear_instance typ)

  let rec import ?must_find typ env =
    let import' = import in
    let import = import ?must_find in
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar (None, _, explicitness) -> (
      match must_find with
      | Some true ->
          raise (Error (typ.type_loc, Unbound_type_var typ))
      | _ ->
          (mkvar ~loc ~explicitness None env, env) )
    | Tvar ((Some {txt= x; _} as name), _, explicitness) -> (
        let var =
          match must_find with
          | Some true ->
              let var = find_type_variable x env in
              if not (Option.is_some var) then
                raise (Error (typ.type_loc, Unbound_type_var typ)) ;
              var
          | Some false ->
              None
          | None ->
              find_type_variable x env
        in
        match var with
        | Some var ->
            (var, env)
        | None ->
            let var = mkvar ~loc ~explicitness name env in
            (var, add_type_variable x var env) )
    | Tpoly (vars, typ) ->
        let env = open_expr_scope env in
        let env, vars =
          List.fold_map vars ~init:env ~f:(fun e t ->
              let t, e = import' ~must_find:false t e in
              (e, t) )
        in
        let typ, env = import typ env in
        let env = close_expr_scope env in
        (mk ~loc (Tpoly (vars, typ)) env, env)
    | Tctor variant -> (
        let {var_ident; var_params; _} = variant in
        match raw_find_type_declaration var_ident env with
        | {tdec_desc= TUnfold typ; _} ->
            (typ, env)
        | decl ->
            let variant =
              { variant with
                var_decl_id= decl.tdec_id
              ; var_implicit_params= decl.tdec_implicit_params }
            in
            let given_args_length = List.length var_params in
            let expected_args_length =
              match decl.tdec_desc with
              | TForward num_args -> (
                match !num_args with
                | Some l ->
                    l
                | None ->
                    num_args := Some given_args_length ;
                    given_args_length )
              | _ ->
                  List.length decl.tdec_params
            in
            if not (Int.equal given_args_length expected_args_length) then
              raise
                (Error
                   ( typ.type_loc
                   , Wrong_number_args
                       (var_ident.txt, given_args_length, expected_args_length)
                   )) ;
            let env, var_params =
              List.fold_map ~init:env var_params ~f:(fun env param ->
                  let param, env = import param env in
                  (env, param) )
            in
            (mk ~loc (Tctor {variant with var_params}) env, env) )
    | Ttuple typs ->
        let env, typs =
          List.fold_map typs ~init:env ~f:(fun e t ->
              let t, e = import t e in
              (e, t) )
        in
        (mk ~loc (Ttuple typs) env, env)
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1, env = import typ1 env in
        let typ2, env = import typ2 env in
        (mk ~loc (Tarrow (typ1, typ2, explicit, label)) env, env)

  let refresh_vars vars new_vars_map env =
    let env, new_vars =
      List.fold_map vars ~init:env ~f:(fun e t ->
          let t, e = import ~must_find:false t e in
          (e, t) )
    in
    let new_vars_map =
      List.fold2_exn ~init:new_vars_map vars new_vars
        ~f:(fun map var new_var -> Map.set map ~key:var.type_id ~data:new_var
      )
    in
    (new_vars, new_vars_map, env)

  let rec copy typ new_vars_map env =
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar _ -> (
      match Map.find new_vars_map typ.type_id with
      | Some var ->
          var
      | None ->
          typ )
    | Tpoly (vars, typ) ->
        let _vars, new_vars_map, env = refresh_vars vars new_vars_map env in
        copy typ new_vars_map env
    | Tctor ({var_params; _} as variant) ->
        let var_params =
          List.map var_params ~f:(fun t -> copy t new_vars_map env)
        in
        mk ~loc (Tctor {variant with var_params}) env
    | Ttuple typs ->
        let typs = List.map typs ~f:(fun t -> copy t new_vars_map env) in
        mk ~loc (Ttuple typs) env
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1 = copy typ1 new_vars_map env in
        let typ2 = copy typ2 new_vars_map env in
        mk ~loc (Tarrow (typ1, typ2, explicit, label)) env

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
    | Tvar (_, var_depth, _) when deep_enough var_depth ->
        Set.singleton (module Comparator) typ
    | Tvar _ ->
        Set.empty (module Comparator)
    | Tpoly (vars, typ) ->
        let poly_vars =
          Set.union_list (module Comparator) (List.map ~f:type_vars' vars)
        in
        Set.diff (type_vars typ) poly_vars
    | Tctor {var_params; _} ->
        Set.union_list (module Comparator) (List.map ~f:type_vars var_params)
    | Ttuple typs ->
        Set.union_list (module Comparator) (List.map ~f:type_vars typs)
    | Tarrow (typ1, typ2, _, _) ->
        Set.union (type_vars typ1) (type_vars typ2)

  let rec flatten typ env =
    let loc = typ.type_loc in
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
        mk ~loc (Tpoly (Set.to_list var_set, typ)) env
    | Tctor variant ->
        let var_params =
          List.map variant.var_params ~f:(fun typ -> flatten typ env)
        in
        mk ~loc (Tctor {variant with var_params}) env
    | Ttuple typs ->
        let typs = List.map typs ~f:(fun typ -> flatten typ env) in
        mk ~loc (Ttuple typs) env
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1 = flatten typ1 env in
        let typ2 = flatten typ2 env in
        mk ~loc (Tarrow (typ1, typ2, explicit, label)) env

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
      | ( Tctor {var_decl_id= id1; var_params= params1; _}
        , Tctor {var_decl_id= id2; var_params= params2; _} ) ->
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

  let rec get_implicits acc typ =
    match typ.type_desc with
    | Tarrow (typ1, typ2, Implicit, label) ->
        get_implicits ((label, typ1) :: acc) typ2
    | _ ->
        (List.rev acc, typ)

  let new_implicit_var ~loc typ env =
    let {TypeEnvi.implicit_vars; implicit_id; _} = env.resolve_env.type_env in
    let mk exp_loc exp_desc = {exp_loc; exp_desc; exp_type= typ} in
    let name = Location.mkloc (sprintf "__implicit%i__" implicit_id) loc in
    let new_exp =
      mk loc (Unifiable {expression= None; name; id= implicit_id})
    in
    env.resolve_env.type_env
    <- { env.resolve_env.type_env with
         implicit_vars= new_exp :: implicit_vars
       ; implicit_id= implicit_id + 1 } ;
    new_exp

  let implicit_instances ~(unify : env -> type_expr -> type_expr -> 'a)
      (typ : type_expr) env =
    List.filter_map env.resolve_env.type_env.instances
      ~f:(fun (id, instance_typ) ->
        let instance_typ = copy instance_typ (Map.empty (module Int)) env in
        match unify env typ instance_typ with
        | _ ->
            List.find_map env.scope_stack ~f:(fun {instances; _} ->
                Option.map (Map.find instances id) ~f:(fun path ->
                    (path, instance_typ) ) )
        | exception _ ->
            None )

  let generate_implicits e env =
    let loc = e.exp_loc in
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

  let rec instantiate_implicits ~unify implicit_vars env =
    let implicit_vars =
      List.map implicit_vars ~f:(fun e ->
          {e with exp_type= flatten e.exp_type env} )
    in
    let env_implicits = env.resolve_env.type_env.implicit_vars in
    env.resolve_env.type_env
    <- {env.resolve_env.type_env with implicit_vars= []} ;
    let implicit_vars =
      List.filter implicit_vars ~f:(fun ({exp_loc; exp_type; _} as exp) ->
          match implicit_instances ~unify exp_type env with
          | [(name, instance_typ)] ->
              let instance_typ =
                copy instance_typ (Map.empty (module Int)) env
              in
              let name = Location.mkloc name exp_loc in
              unify env exp_type instance_typ ;
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
        instantiate_implicits ~unify (new_implicits @ implicit_vars) env

  let flattened_implicit_vars ~toplevel ~unify typ_vars env =
    let unify env typ ctyp = unify env typ (snd (get_implicits [] ctyp)) in
    let {TypeEnvi.implicit_vars; _} = env.resolve_env.type_env in
    let implicit_vars = instantiate_implicits ~unify implicit_vars env in
    let implicit_vars =
      List.dedup_and_sort implicit_vars ~compare:(fun exp1 exp2 ->
          let cmp = compare exp1.exp_type exp2.exp_type in
          ( if Int.equal cmp 0 then
            match (exp1.exp_desc, exp2.exp_desc) with
            | Unifiable desc1, Unifiable desc2 ->
                if desc1.id < desc2.id then desc2.expression <- Some exp1
                else desc1.expression <- Some exp2
            | _ ->
                raise (Error (exp2.exp_loc, No_unifiable_implicit)) ) ;
          cmp )
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

  let rec implicit_params env typ =
    let implicit_params = implicit_params env in
    match typ.type_desc with
    | Tvar (_, _, Explicit) ->
        Set.empty (module Comparator)
    | Tvar (_, _, Implicit) ->
        Set.singleton (module Comparator) typ
    | Ttuple typs ->
        Set.union_list (module Comparator) (List.map ~f:implicit_params typs)
    | Tarrow (typ1, typ2, _, _) ->
        Set.union (implicit_params typ1) (implicit_params typ2)
    | Tctor variant ->
        let {predeclare_types; _} = env.resolve_env in
        env.resolve_env.predeclare_types <- false ;
        let ctor_params =
          try
            let decl = raw_find_type_declaration variant.var_ident env in
            Set.of_list (module Comparator) decl.tdec_implicit_params
          with Error (_, Unbound_type _) -> Set.empty (module Comparator)
        in
        env.resolve_env.predeclare_types <- predeclare_types ;
        Set.union_list
          (module Comparator)
          (ctor_params :: List.map ~f:implicit_params variant.var_params)
    | Tpoly (_, typ) ->
        implicit_params typ

  let rec constr_map env ~f typ =
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar _ ->
        typ
    | Ttuple typs ->
        let typs = List.map ~f:(constr_map env ~f) typs in
        mk ~loc (Ttuple typs) env
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1 = constr_map env ~f typ1 in
        let typ2 = constr_map env ~f typ2 in
        mk ~loc (Tarrow (typ1, typ2, explicit, label)) env
    | Tctor variant ->
        mk ~loc (f variant) env
    | Tpoly (typs, typ) ->
        mk ~loc (Tpoly (typs, constr_map env ~f typ)) env

  let rec bubble_label_aux env label typ =
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
      match bubble_label_aux env label typ2 with
      | None, _ ->
          (None, typ)
      | res, typ2 ->
          ( res
          , mk ~loc:typ.type_loc (Tarrow (typ1, typ2, explicit, arr_label)) env
          ) )
    | _ ->
        (None, typ)

  let bubble_label env label typ =
    match bubble_label_aux env label typ with
    | Some (typ1, explicit, arr_label), typ2 ->
        mk ~loc:typ.type_loc (Tarrow (typ1, typ2, explicit, arr_label)) env
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
end

module TypeDecl = struct
  let next_id env =
    let tdec_id, type_env = TypeEnvi.next_decl_id env.resolve_env.type_env in
    env.resolve_env.type_env <- type_env ;
    tdec_id

  let mk ?(loc = Location.none) ~name ~params ?(implicit_params = []) desc env
      =
    let tdec_id = next_id env in
    { tdec_ident= name
    ; tdec_params= params
    ; tdec_implicit_params= implicit_params
    ; tdec_desc= desc
    ; tdec_id
    ; tdec_loc= loc }

  let import decl env =
    let tdec_id =
      match
        Map.find env.resolve_env.type_env.predeclared_types decl.tdec_ident.txt
      with
      | Some (id, num_args, loc) ->
          ( match !num_args with
          | Some num_args ->
              let given = List.length decl.tdec_params in
              if not (Int.equal given num_args) then
                raise
                  (Error
                     ( loc
                     , Wrong_number_args
                         (Lident decl.tdec_ident.txt, given, num_args) ))
          | None ->
              () ) ;
          let {type_env; _} = env.resolve_env in
          env.resolve_env.type_env
          <- { type_env with
               predeclared_types=
                 Map.remove type_env.predeclared_types decl.tdec_ident.txt } ;
          id
      | None ->
          next_id env
    in
    let env = open_expr_scope env in
    let env, tdec_params =
      List.fold_map ~init:env decl.tdec_params ~f:(fun env param ->
          match param.type_desc with
          | Tvar _ ->
              let var, env = Type.import ~must_find:false param env in
              (env, var)
          | _ ->
              raise (Error (param.type_loc, Expected_type_var param)) )
    in
    let tdec_implicit_params =
      match decl.tdec_desc with
      | TAbstract | TOpen | TExtend _ ->
          Set.empty (module Type)
      | TAlias typ | TUnfold typ ->
          Type.implicit_params env typ
      | TRecord fields ->
          Set.union_list
            (module Type)
            (List.map fields ~f:(fun {fld_type; _} ->
                 Type.implicit_params env fld_type ))
      | TVariant ctors ->
          Set.union_list
            (module Type)
            (List.map ctors ~f:(fun ctor ->
                 let typs =
                   match ctor.ctor_args with
                   | Ctor_tuple typs ->
                       typs
                   | Ctor_record (_, fields) ->
                       List.map ~f:(fun {fld_type; _} -> fld_type) fields
                 in
                 let typs =
                   match ctor.ctor_ret with
                   | Some ctor_ret ->
                       ctor_ret :: typs
                   | None ->
                       typs
                 in
                 Set.union_list
                   (module Type)
                   (List.map typs ~f:(Type.implicit_params env)) ))
      | TForward _ ->
          failwith "Cannot import a forward type declaration"
    in
    let tdec_implicit_params = Set.to_list tdec_implicit_params in
    (* Make sure the declaration is available to lookup for recursive types. *)
    let decl = {decl with tdec_id; tdec_params; tdec_implicit_params} in
    let scope, env = pop_expr_scope env in
    let env =
      match decl.tdec_desc with
      | TExtend _ ->
          env
      | _ ->
          map_current_scope ~f:(Scope.add_type_declaration decl) env
    in
    let env = push_scope scope env in
    let tdec_desc, env =
      match decl.tdec_desc with
      | (TAbstract | TOpen | TForward _) as desc ->
          (desc, env)
      | TAlias typ ->
          let typ, env = Type.import ~must_find:true typ env in
          (TAlias typ, env)
      | TUnfold typ ->
          let typ, env = Type.import ~must_find:false typ env in
          (TUnfold typ, env)
      | TRecord fields ->
          let env, fields =
            List.fold_map ~init:env fields ~f:(fun env field ->
                let fld_type, env =
                  Type.import ~must_find:true field.fld_type env
                in
                (env, {field with fld_type}) )
          in
          (TRecord fields, env)
      | TVariant ctors | TExtend (_, _, ctors) -> (
          let env, ctors =
            List.fold_map ~init:env ctors ~f:(fun env ctor ->
                let scope, env = pop_expr_scope env in
                let ctor_ret, env, must_find, ctor_ret_params =
                  match ctor.ctor_ret with
                  | Some ret ->
                      let env = open_expr_scope env in
                      let name =
                        match decl.tdec_desc with
                        | TVariant _ ->
                            Lident decl.tdec_ident.txt
                        | TExtend (lid, _, _) ->
                            lid.txt
                        | _ ->
                            failwith
                              "Could not find name for TVariant/TExtend."
                      in
                      ( match ret.type_desc with
                      | Tctor {var_ident= {txt= lid; _}; _}
                        when Longident.compare lid name = 0 ->
                          ()
                      | _ ->
                          raise
                            (Error
                               ( ret.type_loc
                               , Constraints_not_satisfied (ret, decl) )) ) ;
                      let ret, env = Type.import ~must_find:false ret env in
                      let ctor_ret_params =
                        match ret.type_desc with
                        | Tctor {var_params; _} ->
                            var_params
                        | _ ->
                            []
                      in
                      (Some ret, env, None, ctor_ret_params)
                  | None ->
                      (None, push_scope scope env, Some true, decl.tdec_params)
                in
                let env, ctor_args =
                  match ctor.ctor_args with
                  | Ctor_tuple args ->
                      let env, args =
                        List.fold_map ~init:env args ~f:(fun env arg ->
                            let arg, env = Type.import ?must_find arg env in
                            (env, arg) )
                      in
                      (env, Ctor_tuple args)
                  | Ctor_record (_, fields) ->
                      let env, fields =
                        List.fold_map ~init:env fields ~f:(fun env field ->
                            let fld_type, env =
                              Type.import ?must_find field.fld_type env
                            in
                            (env, {field with fld_type}) )
                      in
                      let decl =
                        mk ~loc:ctor.ctor_loc ~name:ctor.ctor_ident
                          ~params:ctor_ret_params (TRecord fields) env
                      in
                      Type.map_env env ~f:(TypeEnvi.add_decl decl) ;
                      (env, Ctor_record (decl.tdec_id, fields))
                in
                let env = push_scope scope (close_expr_scope env) in
                (env, {ctor with ctor_args; ctor_ret}) )
          in
          match decl.tdec_desc with
          | TVariant _ ->
              (TVariant ctors, env)
          | TExtend (id, decl, _) ->
              (TExtend (id, decl, ctors), env)
          | _ ->
              failwith "Expected a TVariant or a TExtend" )
    in
    let env = close_expr_scope env in
    let decl = {decl with tdec_desc} in
    let env =
      map_current_scope ~f:(Scope.register_type_declaration decl) env
    in
    Type.map_env env ~f:(TypeEnvi.add_decl decl) ;
    (decl, env)

  let mk_typ ?(loc = Location.none) ~params ?ident decl =
    let ident = Option.value ident ~default:(mk_lid decl.tdec_ident) in
    Type.mk ~loc
      (Tctor
         { var_ident= ident
         ; var_params= params
         ; var_implicit_params= []
         ; var_decl_id= decl.tdec_id })

  let find ident env =
    let decl = raw_find_type_declaration ident env in
    import decl env

  let find_of_type typ env =
    let open Option.Let_syntax in
    let%map variant =
      match typ.type_desc with Tctor variant -> Some variant | _ -> None
    in
    let decl =
      match TypeEnvi.decl env.resolve_env.type_env variant with
      | Some decl ->
          decl
      | None ->
          raise (Error (typ.type_loc, Unbound_type variant.var_ident.txt))
    in
    let bound_vars =
      match
        List.fold2
          ~init:(Map.empty (module Int))
          variant.var_params decl.tdec_params
          ~f:(fun bound_vars param var ->
            Map.set bound_vars ~key:var.type_id ~data:param )
      with
      | Ok bound_vars ->
          bound_vars
      | Unequal_lengths ->
          raise
            (Error
               ( typ.type_loc
               , Wrong_number_args
                   ( variant.var_ident.txt
                   , List.length decl.tdec_params
                   , List.length variant.var_params ) ))
    in
    (decl, bound_vars, env)

  let find_of_field (field : lid) env =
    find_of_lident ~kind:"field" ~get_name:Scope.get_field field env

  let find_of_constructor (ctor : lid) env =
    find_of_lident ~kind:"constructor" ~get_name:Scope.get_ctor ctor env

  let unfold_alias typ env =
    match find_of_type typ env with
    | Some ({tdec_desc= TAlias alias_typ; _}, bound_vars, env) ->
        Some (Type.copy alias_typ bound_vars env)
    | _ ->
        None

  let rec find_unaliased_of_type typ env =
    match find_of_type typ env with
    | Some ({tdec_desc= TAlias alias_typ; _}, bound_vars, env) ->
        let typ = Type.copy alias_typ bound_vars env in
        find_unaliased_of_type typ env
    | ret ->
        ret
end

let add_name (name : str) typ =
  map_current_scope ~f:(Scope.add_name name.txt typ)

let get_name (name : str) env =
  let loc = name.loc in
  match List.find_map ~f:(Scope.get_name name.txt) env.scope_stack with
  | Some typ ->
      Type.copy typ (Map.empty (module Int)) env
  | None ->
      raise (Error (loc, Unbound_value (Lident name.txt)))

let find_name (lid : lid) env =
  match find_of_lident ~kind:"name" ~get_name:Scope.get_name lid env with
  | Some typ ->
      Type.copy typ (Map.empty (module Int)) env
  | None ->
      raise (Error (lid.loc, Unbound_value lid.txt))

(* Error handling *)

open Format

let pp_typ = Pprint.type_expr

let pp_decl_typ ppf decl =
  pp_typ ppf
    { type_desc=
        Tctor
          { var_ident= mk_lid decl.tdec_ident
          ; var_params= decl.tdec_params
          ; var_implicit_params= decl.tdec_implicit_params
          ; var_decl_id= decl.tdec_id }
    ; type_id= -1
    ; type_loc= Location.none }

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
