open Core_kernel
open Parsetypes
open Longident

type error =
  | No_open_scopes
  | Unbound_type_var of type_expr
  | Unbound_type of string
  | Wrong_number_args of string * int * int
  | Expected_type_var of type_expr

exception Error of Location.t * error

type 'a name_map = (string, 'a, String.comparator_witness) Map.t

type 'a int_map = (int, 'a, Int.comparator_witness) Map.t

type 'a lid_map = (Longident.t, 'a, Longident.comparator_witness) Map.t

module Scope = struct
  type t =
    { names: type_expr name_map
    ; type_variables: type_expr name_map
    ; type_decls: type_decl name_map
    ; type_decls_ids: type_decl int_map
    ; fields: (type_decl * int) name_map
    ; modules: t name_map }

  let empty =
    { names= Map.empty (module String)
    ; type_variables= Map.empty (module String)
    ; type_decls= Map.empty (module String)
    ; type_decls_ids= Map.empty (module Int)
    ; fields= Map.empty (module String)
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

  let add_type_declaration decl scope =
    { scope with
      type_decls= Map.set scope.type_decls ~key:decl.tdec_ident.txt ~data:decl
    }

  let add_type_declaration_id decl scope =
    { scope with
      type_decls_ids= Map.set scope.type_decls_ids ~key:decl.tdec_id ~data:decl
    }

  let find_type_declaration (name : str) scope =
    Map.find scope.type_decls name.txt

  let register_type_declaration decl scope =
    let scope =
      { scope with
        type_decls=
          Map.set scope.type_decls ~key:decl.tdec_ident.txt ~data:decl
      ; type_decls_ids=
          Map.set scope.type_decls_ids ~key:decl.tdec_id ~data:decl }
    in
    match decl.tdec_desc with
    | TAbstract | TAlias _ -> scope
    | TRecord fields -> List.foldi ~f:(add_field decl) ~init:scope fields

  let add_module name m scope =
    {scope with modules= Map.set scope.modules ~key:name ~data:m}

  let get_module name scope = Map.find scope.modules name

  let rec find_module lid scope =
    match lid with
    | Lident name -> get_module name scope
    | Ldot (path, name) ->
        Option.bind (find_module path scope) ~f:(get_module name)
    | Lapply _ ->
        failwithf "Don't know how to find module %a" Longident.show lid ()

  let find_name lid scope =
    match lid with
    | Lident name -> get_name name scope
    | Ldot (path, name) ->
        Option.bind (find_module path scope) ~f:(get_name name)
    | Lapply _ ->
        failwithf "Don't know how to find identifier %a" Longident.show lid ()
end

module TypeEnvi = struct
  type t =
    {type_id: int; type_decl_id: int; variable_instances: type_expr int_map}

  let empty =
    {type_id= 1; type_decl_id= 1; variable_instances= Map.empty (module Int)}

  let instance env (typ : type_expr) =
    Map.find env.variable_instances typ.type_id

  let add_instance (typ : type_expr) typ' env =
    { env with
      variable_instances=
        Map.update env.variable_instances typ.type_id ~f:(fun _ -> typ') }

  let clear_instance (typ : type_expr) env =
    {env with variable_instances= Map.remove env.variable_instances typ.type_id}

  let next_type_id env = (env.type_id, {env with type_id= env.type_id + 1})

  let next_decl_id env =
    (env.type_decl_id, {env with type_decl_id= env.type_decl_id + 1})
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

let find_type_declaration name env =
  List.find_map ~f:(Scope.find_type_declaration name) env.scope_stack

let add_module (name : str) m =
  map_current_scope ~f:(Scope.add_module name.txt m)

let find_module (lid : lid) env =
  match List.find_map ~f:(Scope.find_module lid.txt) env.scope_stack with
  | Some m -> m
  | None -> failwithf "Could not find module %a." Longident.show lid.txt ()

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
        let decl =
          match find_type_declaration var_ident env with
          | Some decl -> decl
          | None -> raise (Error (var_ident.loc, Unbound_type var_ident.txt))
        in
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
              let t, e = import ~must_find:false t e in
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
    let decl = {decl with tdec_id} in
    let env = {env with type_env} in
    (* Make sure the declaration is available to lookup for recursive types. *)
    let env = map_current_scope ~f:(Scope.add_type_declaration decl) env in
    let env = open_scope env in
    let env, tdec_params =
      List.fold_map ~init:env decl.tdec_params ~f:(fun env param ->
          match param.type_desc with
          | Tvar _ ->
              let var, env = Type.import ~must_find:false param env in
              (env, var)
          | _ -> raise (Error (param.type_loc, Expected_type_var param)) )
    in
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
    in
    let env = close_scope env in
    let decl = {decl with tdec_id; tdec_params; tdec_desc} in
    let env = map_current_scope ~f:(Scope.add_type_declaration decl) env in
    (decl, env)

  let mk_typ ?(loc = Location.none) ~params decl =
    Type.mk ~loc
      (Tctor
         { var_ident= decl.tdec_ident
         ; var_params= params
         ; var_decl_id= decl.tdec_id })
end

let add_name (name : str) typ =
  map_current_scope ~f:(Scope.add_name name.txt typ)

let get_name (name : str) env =
  match List.find_map ~f:(Scope.get_name name.txt) env.scope_stack with
  | Some typ -> Type.copy typ (Map.empty (module Int)) env
  | None -> failwithf "Could not find name %s." name.txt ()

let find_name (lid : lid) env =
  match List.find_map ~f:(Scope.find_name lid.txt) env.scope_stack with
  | Some typ -> Type.copy typ (Map.empty (module Int)) env
  | None -> failwithf "Could not find name %a." Longident.show lid.txt ()

module Core = struct
  let mkloc s = Location.(mkloc s none)

  let mk_type_decl name desc =
    { tdec_ident= mkloc name
    ; tdec_params= []
    ; tdec_desc= desc
    ; tdec_id= 0
    ; tdec_loc= Location.none }

  let env = empty

  let int, env = TypeDecl.import (mk_type_decl "int" TAbstract) env

  let unit, env = TypeDecl.import (mk_type_decl "unit" TAbstract) env

  let bool, env = TypeDecl.import (mk_type_decl "bool" TAbstract) env

  let char, env = TypeDecl.import (mk_type_decl "char" TAbstract) env

  let string, env = TypeDecl.import (mk_type_decl "string" TAbstract) env

  let float, env = TypeDecl.import (mk_type_decl "float" TAbstract) env

  module Type = struct
    let int, env = TypeDecl.mk_typ int ~params:[] env

    let unit, env = TypeDecl.mk_typ unit ~params:[] env

    let bool, env = TypeDecl.mk_typ bool ~params:[] env

    let char, env = TypeDecl.mk_typ char ~params:[] env

    let string, env = TypeDecl.mk_typ string ~params:[] env

    let float, env = TypeDecl.mk_typ float ~params:[] env
  end

  let env = Type.env
end

(* Error handling *)

open Format

let pp_typ ppf typ = Pprintast.core_type ppf (To_ocaml.of_type_expr typ)

let report_error ppf = function
  | No_open_scopes ->
      fprintf ppf "Internal error: There is no current open scope."
  | Unbound_type_var var -> fprintf ppf "Unbound type parameter %a." pp_typ var
  | Unbound_type typename ->
      fprintf ppf "Unbound type constructor %s." typename
  | Wrong_number_args (typename, given, expected) ->
      fprintf ppf
        "@[The type constructor %s expects %d argument(s)@ but is here \
         applied to %d argument(s).@]"
        typename expected given
  | Expected_type_var typ ->
      fprintf ppf "Syntax error: Expected a type parameter, but got %a." pp_typ
        typ

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None )
