open Core_kernel
open Parsetypes
open Longident

type error =
  | No_open_scopes
  | Unbound_type_var of type_expr
  | Unbound_type of Longident.t
  | Unbound_module of Longident.t
  | Unbound_value of Longident.t
  | Wrong_number_args of Longident.t * int * int
  | Expected_type_var of type_expr
  | Lident_unhandled of string * Longident.t
  | Constraints_not_satisfied of type_expr * type_decl

exception Error of Location.t * error

type 'a name_map = (string, 'a, String.comparator_witness) Map.t

type 'a int_map = (int, 'a, Int.comparator_witness) Map.t

type 'a lid_map = (Longident.t, 'a, Longident.comparator_witness) Map.t

module Scope = struct
  type t =
    { names: type_expr name_map
    ; type_variables: type_expr name_map
    ; type_decls: type_decl name_map
    ; fields: (type_decl * int) name_map
    ; ctors: (type_decl * int) name_map
    ; modules: t name_map }

  let empty =
    { names= Map.empty (module String)
    ; type_variables= Map.empty (module String)
    ; type_decls= Map.empty (module String)
    ; fields= Map.empty (module String)
    ; ctors= Map.empty (module String)
    ; modules= Map.empty (module String) }

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
    let scope =
      { scope with
        type_decls=
          Map.set scope.type_decls ~key:decl.tdec_ident.txt ~data:decl }
    in
    match decl.tdec_desc with
    | TAbstract | TAlias _ -> scope
    | TRecord fields -> List.foldi ~f:(add_field decl) ~init:scope fields
    | TVariant ctors -> List.foldi ~f:(add_ctor decl) ~init:scope ctors

  let fold_over ~init:acc ~names ~type_variables ~type_decls ~fields ~ctors
      ~modules
      { names= names1
      ; type_variables= type_variables1
      ; type_decls= type_decls1
      ; fields= fields1
      ; ctors= ctors1
      ; modules= modules1 }
      { names= names2
      ; type_variables= type_variables2
      ; type_decls= type_decls2
      ; fields= fields2
      ; ctors= ctors2
      ; modules= modules2 } =
    let acc =
      Map.fold2 type_variables1 type_variables2 ~init:acc ~f:type_variables
    in
    let acc = Map.fold2 type_decls1 type_decls2 ~init:acc ~f:type_decls in
    let acc = Map.fold2 ctors1 ctors2 ~init:acc ~f:ctors in
    let acc = Map.fold2 fields1 fields2 ~init:acc ~f:fields in
    let acc = Map.fold2 modules1 modules2 ~init:acc ~f:modules in
    let acc = Map.fold2 names1 names2 ~init:acc ~f:names in
    acc

  let add_module name m scope =
    {scope with modules= Map.set scope.modules ~key:name ~data:m}

  let get_module name scope = Map.find scope.modules name

  let find_of_lident ~loc ~kind ~get_name ~find_module lid scope =
    match lid with
    | Lident name -> get_name name scope
    | Ldot (path, name) ->
        Option.bind (find_module ~loc path scope) ~f:(get_name name)
    | Lapply _ -> raise (Error (loc, Lident_unhandled (kind, lid)))

  let rec find_module =
    find_of_lident ~kind:"module" ~get_name:get_module ~find_module

  let find_name = find_of_lident ~kind:"identifier" ~get_name ~find_module

  let find_type_declaration =
    find_of_lident ~kind:"type" ~get_name:get_type_declaration ~find_module

  let find_field =
    find_of_lident ~kind:"field" ~get_name:get_field ~find_module

  let find_ctor =
    find_of_lident ~kind:"constructor" ~get_name:get_ctor ~find_module
end

module TypeEnvi = struct
  type t =
    { type_id: int
    ; type_decl_id: int
    ; variable_instances: type_expr int_map
    ; type_decls: type_decl int_map }

  let empty =
    { type_id= 1
    ; type_decl_id= 1
    ; variable_instances= Map.empty (module Int)
    ; type_decls= Map.empty (module Int) }

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
end

type t = {scope_stack: Scope.t list; type_env: TypeEnvi.t; depth: int}

let empty = {scope_stack= [Scope.empty]; type_env= TypeEnvi.empty; depth= 0}

let current_scope {scope_stack; _} =
  match List.hd scope_stack with
  | Some scope -> scope
  | None -> raise (Error (Location.none, No_open_scopes))

let push_scope scope env =
  {env with scope_stack= scope :: env.scope_stack; depth= env.depth + 1}

let open_scope = push_scope Scope.empty

let pop_scope env =
  match env.scope_stack with
  | [] -> raise (Error (Location.none, No_open_scopes))
  | scope :: scope_stack ->
      (scope, {env with scope_stack; depth= env.depth + 1})

let close_scope env = snd (pop_scope env)

let map_current_scope ~f env =
  match env.scope_stack with
  | current_scope :: scope_stack ->
      {env with scope_stack= f current_scope :: scope_stack}
  | [] -> raise (Error (Location.none, No_open_scopes))

let add_type_variable name typ =
  map_current_scope ~f:(Scope.add_type_variable name typ)

let find_type_variable name env =
  List.find_map ~f:(Scope.find_type_variable name) env.scope_stack

let raw_find_type_declaration (lid : lid) env =
  let loc = lid.loc in
  match
    List.find_map ~f:(Scope.find_type_declaration ~loc lid.txt) env.scope_stack
  with
  | Some decl -> decl
  | None -> raise (Error (loc, Unbound_type lid.txt))

let add_module (name : str) m =
  map_current_scope ~f:(Scope.add_module name.txt m)

let find_module ~loc (lid : lid) env =
  match List.find_map ~f:(Scope.find_module ~loc lid.txt) env.scope_stack with
  | Some m -> m
  | None -> raise (Error (loc, Unbound_module lid.txt))

module Type = struct
  let mk ~loc type_desc env =
    let type_id, type_env = TypeEnvi.next_type_id env.type_env in
    let env = {env with type_env} in
    ({type_desc; type_id; type_loc= loc}, env)

  let mkvar ~loc name env = mk ~loc (Tvar (name, env.depth)) env

  let instance env typ = TypeEnvi.instance env.type_env typ

  let map_env ~f env = {env with type_env= f env.type_env}

  let add_instance typ typ' = map_env ~f:(TypeEnvi.add_instance typ typ')

  let clear_instance typ = map_env ~f:(TypeEnvi.clear_instance typ)

  let rec import ?must_find typ env =
    let import' = import in
    let import = import ?must_find in
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar (None, _) -> (
      match must_find with
      | Some true -> raise (Error (typ.type_loc, Unbound_type_var typ))
      | _ -> mkvar ~loc None env )
    | Tvar ((Some {txt= x; _} as name), _) -> (
        let var =
          match must_find with
          | Some true ->
              let var = find_type_variable x env in
              if not (Option.is_some var) then
                raise (Error (typ.type_loc, Unbound_type_var typ)) ;
              var
          | Some false -> None
          | None -> find_type_variable x env
        in
        match var with
        | Some var -> (var, env)
        | None ->
            let var, env = mkvar ~loc name env in
            (var, add_type_variable x var env) )
    | Tpoly (vars, typ) ->
        let env = open_scope env in
        let env, vars =
          List.fold_map vars ~init:env ~f:(fun e t ->
              let t, e = import' ~must_find:false t e in
              (e, t) )
        in
        let typ, env = import typ env in
        let env = close_scope env in
        mk ~loc (Tpoly (vars, typ)) env
    | Tctor variant ->
        let {var_ident; var_params; _} = variant in
        let decl = raw_find_type_declaration var_ident env in
        let variant = {variant with var_decl_id= decl.tdec_id} in
        let given_args_length = List.length var_params in
        let expected_args_length = List.length decl.tdec_params in
        if not (Int.equal given_args_length expected_args_length) then
          raise
            (Error
               ( typ.type_loc
               , Wrong_number_args
                   (var_ident.txt, given_args_length, expected_args_length) )) ;
        let env, var_params =
          List.fold_map ~init:env var_params ~f:(fun env param ->
              let param, env = import param env in
              (env, param) )
        in
        mk ~loc (Tctor {variant with var_params}) env
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
      | Some var -> (var, env)
      | None -> (typ, env) )
    | Tpoly (vars, typ) ->
        let vars, new_vars_map, env = refresh_vars vars new_vars_map env in
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

module TypeDecl = struct
  let mk ?(loc = Location.none) ~name ~params desc env =
    let tdec_id, type_env = TypeEnvi.next_decl_id env.type_env in
    let env = {env with type_env} in
    ( { tdec_ident= name
      ; tdec_params= params
      ; tdec_desc= desc
      ; tdec_id
      ; tdec_loc= loc }
    , env )

  let import decl env =
    let tdec_id, type_env = TypeEnvi.next_decl_id env.type_env in
    let env = {env with type_env} in
    let env = open_scope env in
    let env, tdec_params =
      List.fold_map ~init:env decl.tdec_params ~f:(fun env param ->
          match param.type_desc with
          | Tvar _ ->
              let var, env = Type.import ~must_find:false param env in
              (env, var)
          | _ -> raise (Error (param.type_loc, Expected_type_var param)) )
    in
    (* Make sure the declaration is available to lookup for recursive types. *)
    let decl = {decl with tdec_id; tdec_params} in
    let scope, env = pop_scope env in
    let env = map_current_scope ~f:(Scope.add_type_declaration decl) env in
    let env = push_scope scope env in
    let tdec_desc, env =
      match decl.tdec_desc with
      | TAbstract -> (TAbstract, env)
      | TAlias typ ->
          let typ, env = Type.import ~must_find:true typ env in
          (TAlias typ, env)
      | TRecord fields ->
          let env, fields =
            List.fold_map ~init:env fields ~f:(fun env field ->
                let fld_type, env =
                  Type.import ~must_find:true field.fld_type env
                in
                (env, {field with fld_type}) )
          in
          (TRecord fields, env)
      | TVariant ctors ->
          let env, ctors =
            List.fold_map ~init:env ctors ~f:(fun env ctor ->
                let scope, env = pop_scope env in
                let ctor_ret, env, must_find, ctor_ret_params =
                  match ctor.ctor_ret with
                  | Some ret ->
                      let env = open_scope env in
                      ( match ret.type_desc with
                      | Tctor {var_ident= {txt= Lident str; _}; _}
                        when String.equal str decl.tdec_ident.txt ->
                          ()
                      | _ ->
                          raise
                            (Error
                               ( ret.type_loc
                               , Constraints_not_satisfied (ret, decl) )) ) ;
                      let ret, env = Type.import ~must_find:false ret env in
                      let ctor_ret_params =
                        match ret.type_desc with
                        | Tctor {var_params; _} -> var_params
                        | _ -> []
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
                      let decl, env =
                        mk ~loc:ctor.ctor_loc ~name:ctor.ctor_ident
                          ~params:ctor_ret_params (TRecord fields) env
                      in
                      let env =
                        {env with type_env= TypeEnvi.add_decl decl env.type_env}
                      in
                      (env, Ctor_record (tdec_id, fields))
                in
                let env = push_scope scope (close_scope env) in
                (env, {ctor with ctor_args; ctor_ret}) )
          in
          (TVariant ctors, env)
    in
    let env = close_scope env in
    let decl = {decl with tdec_desc} in
    let env =
      map_current_scope ~f:(Scope.register_type_declaration decl) env
    in
    let env = {env with type_env= TypeEnvi.add_decl decl env.type_env} in
    (decl, env)

  let mk_typ ?(loc = Location.none) ~params ?ident decl =
    let ident = Option.value ident ~default:(mk_lid decl.tdec_ident) in
    Type.mk ~loc
      (Tctor {var_ident= ident; var_params= params; var_decl_id= decl.tdec_id})

  let find ident env =
    let decl = raw_find_type_declaration ident env in
    import decl env

  let find_of_type typ env =
    let open Option.Let_syntax in
    let%bind variant =
      match typ.type_desc with Tctor variant -> Some variant | _ -> None
    in
    let%map decl = TypeEnvi.decl env.type_env variant in
    let bound_vars =
      match
        List.fold2
          ~init:(Map.empty (module Int))
          variant.var_params decl.tdec_params
          ~f:(fun bound_vars param var ->
            Map.set bound_vars ~key:var.type_id ~data:param )
      with
      | Ok bound_vars -> bound_vars
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
    List.find_map
      ~f:(Scope.find_field ~loc:field.loc field.txt)
      env.scope_stack

  let find_of_constructor (ctor : lid) env =
    List.find_map ~f:(Scope.find_ctor ~loc:ctor.loc ctor.txt) env.scope_stack

  let unfold_alias typ env =
    match find_of_type typ env with
    | Some ({tdec_desc= TAlias alias_typ; _}, bound_vars, env) ->
        Some (Type.copy alias_typ bound_vars env)
    | _ -> None

  let rec find_unaliased_of_type typ env =
    match find_of_type typ env with
    | Some ({tdec_desc= TAlias alias_typ; _}, bound_vars, env) ->
        let typ, env = Type.copy alias_typ bound_vars env in
        find_unaliased_of_type typ env
    | ret -> ret
end

let add_name (name : str) typ =
  map_current_scope ~f:(Scope.add_name name.txt typ)

let get_name (name : str) env =
  let loc = name.loc in
  match List.find_map ~f:(Scope.get_name name.txt) env.scope_stack with
  | Some typ -> Type.copy typ (Map.empty (module Int)) env
  | None -> raise (Error (loc, Unbound_value (Lident name.txt)))

let find_name (lid : lid) env =
  let loc = lid.loc in
  match List.find_map ~f:(Scope.find_name ~loc lid.txt) env.scope_stack with
  | Some typ -> Type.copy typ (Map.empty (module Int)) env
  | None -> raise (Error (loc, Unbound_value lid.txt))

module Core = struct
  let env = ref empty

  module Mk = struct
    let mkloc s = Location.(mkloc s none)

    let constructor name =
      { ctor_ident= mkloc name
      ; ctor_args= Ctor_tuple []
      ; ctor_ret= None
      ; ctor_loc= Location.none }

    let type_decl name desc =
      let decl =
        { tdec_ident= mkloc name
        ; tdec_params= []
        ; tdec_desc= desc
        ; tdec_id= 0
        ; tdec_loc= Location.none }
      in
      let decl, env' = TypeDecl.import decl !env in
      env := env' ;
      decl

    let type_decl_typ decl ~params =
      let typ, env' = TypeDecl.mk_typ decl ~params !env in
      env := env' ;
      typ

    let typ type_desc =
      let typ, env' = Type.mk ~loc:Location.none type_desc !env in
      env := env' ;
      typ

    let arrow x y = typ (Tarrow (x, y))
  end

  let add_name name typ =
    let typ, env' = Type.import ~must_find:false typ !env in
    env := add_name {txt= name; loc= Location.none} typ env'

  module type Mod = functor (X :sig  end) -> sig end

  module Mod (X : sig
    val name : string
  end)
  (Mod : Mod) =
  struct
    let () = env := open_scope !env

    include Mod ()

    let () =
      let m_env, env' = pop_scope !env in
      env := add_module {txt= X.name; loc= Location.none} m_env env'
  end

  module Decl = struct
    let int = Mk.type_decl "int" TAbstract

    let unit = Mk.type_decl "unit" (TVariant [Mk.constructor "()"])

    let bool =
      Mk.type_decl "bool"
        (TVariant [Mk.constructor "true"; Mk.constructor "false"])

    let char = Mk.type_decl "char" TAbstract

    let string = Mk.type_decl "string" TAbstract

    let float = Mk.type_decl "float" TAbstract
  end

  let int = Mk.type_decl_typ Decl.int ~params:[]

  let unit = Mk.type_decl_typ Decl.unit ~params:[]

  let bool = Mk.type_decl_typ Decl.bool ~params:[]

  let char = Mk.type_decl_typ Decl.char ~params:[]

  let string = Mk.type_decl_typ Decl.string ~params:[]

  let float = Mk.type_decl_typ Decl.float ~params:[]

  module Field_intf (Env : sig end) = struct
    module Decl = struct
      let t = Mk.type_decl "t" TAbstract
    end

    let t = Mk.type_decl_typ Decl.t ~params:[]

    let () =
      add_name "of_int" (Mk.arrow int t) ;
      add_name "one" t ;
      add_name "zero" t ;
      add_name "add" (Mk.arrow t (Mk.arrow t t)) ;
      add_name "sub" (Mk.arrow t (Mk.arrow t t)) ;
      add_name "mul" (Mk.arrow t (Mk.arrow t t)) ;
      add_name "inv" (Mk.arrow t t) ;
      add_name "square" (Mk.arrow t t) ;
      add_name "sqrt" (Mk.arrow t t) ;
      add_name "is_square" (Mk.arrow t bool) ;
      add_name "equal" (Mk.arrow t (Mk.arrow t bool)) ;
      add_name "size_in_bits" int ;
      add_name "print" (Mk.arrow t unit) ;
      add_name "random" (Mk.arrow unit t)
  end

  module Backend_intf (Env : sig end) = struct
    module Field =
      Mod (struct
          let name = "Field"
        end)
        (Field_intf)
  end

  module Backends_intf (Env : sig end) = struct
    module Mnt4 =
      Mod (struct
          let name = "Mnt4"
        end)
        (Backend_intf)

    module Mnt6 =
      Mod (struct
          let name = "Mnt6"
        end)
        (Backend_intf)

    module Bn128 =
      Mod (struct
          let name = "Bn128"
        end)
        (Backend_intf)
  end

  module Snarky_intf (Env : sig end) = struct
    module Backends =
      Mod (struct
          let name = "Backends"
        end)
        (Backends_intf)
  end

  module Snarky = struct
    include Mod (struct
                let name = "Snarky"
              end)
              (Snarky_intf)
  end

  let env = !env
end

(* Error handling *)

open Format

let pp_typ ppf typ = Pprintast.core_type ppf (To_ocaml.of_type_expr typ)

let pp_decl_typ ppf decl =
  pp_typ ppf
    { type_desc=
        Tctor
          { var_ident= mk_lid decl.tdec_ident
          ; var_params= decl.tdec_params
          ; var_decl_id= decl.tdec_id }
    ; type_id= -1
    ; type_loc= Location.none }

let report_error ppf = function
  | No_open_scopes ->
      fprintf ppf "Internal error: There is no current open scope."
  | Unbound_type_var var -> fprintf ppf "Unbound type parameter %a." pp_typ var
  | Unbound_type lid ->
      fprintf ppf "Unbound type constructor %a." Longident.pp lid
  | Unbound_module lid -> fprintf ppf "Unbound module %a." Longident.pp lid
  | Unbound_value lid -> fprintf ppf "Unbound value %a." Longident.pp lid
  | Wrong_number_args (lid, given, expected) ->
      fprintf ppf
        "@[The type constructor %a expects %d argument(s)@ but is here \
         applied to %d argument(s).@]"
        Longident.pp lid expected given
  | Expected_type_var typ ->
      fprintf ppf "Syntax error: Expected a type parameter, but got %a." pp_typ
        typ
  | Lident_unhandled (kind, lid) ->
      fprintf ppf "Don't know how to find %s %a" kind Longident.pp lid
  | Constraints_not_satisfied (typ, decl) ->
      fprintf ppf
        "@[Constraints are not satisfied in this type.@ Type %a should be an \
         instance of %a"
        pp_typ typ pp_decl_typ decl

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None )
