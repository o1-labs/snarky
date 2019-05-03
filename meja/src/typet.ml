open Core_kernel
open Ast_types
open Parsetypes
open Envi
open Longident

type error =
  | Unbound_type_var of type_expr
  | Wrong_number_args of Longident.t * int * int
  | Expected_type_var of type_expr
  | Constraints_not_satisfied of type_expr * type_decl

exception Error of Location.t * error

module Type = struct
  open Type

  let rec import ?must_find (typ : type_expr) env : Type0.type_expr * env =
    let import' = import in
    let import = import ?must_find in
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar (None, explicitness) -> (
      match must_find with
      | Some true ->
          raise (Error (loc, Unbound_type_var typ))
      | _ ->
          (mkvar ~explicitness None env, env) )
    | Tvar ((Some {txt= x; _} as name), explicitness) -> (
        let var =
          match must_find with
          | Some true ->
              let var = find_type_variable x env in
              if not (Option.is_some var) then
                raise (Error (loc, Unbound_type_var typ)) ;
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
            let var = mkvar ~explicitness name env in
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
        (mk (Tpoly (vars, typ)) env, env)
    | Tctor variant -> (
        let {var_ident; var_params; _} = variant in
        match raw_find_type_declaration var_ident env with
        | {tdec_desc= TUnfold typ; _} ->
            (typ, env)
        | decl ->
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
                   ( loc
                   , Wrong_number_args
                       (var_ident.txt, given_args_length, expected_args_length)
                   )) ;
            let env, var_params =
              List.fold_map ~init:env var_params ~f:(fun env param ->
                  let param, env = import param env in
                  (env, param) )
            in
            let variant =
              { Type0.var_params
              ; var_ident
              ; var_decl_id= decl.tdec_id
              ; var_implicit_params= decl.tdec_implicit_params }
            in
            (mk (Tctor variant) env, env) )
    | Ttuple typs ->
        let env, typs =
          List.fold_map typs ~init:env ~f:(fun e t ->
              let t, e = import t e in
              (e, t) )
        in
        (mk (Ttuple typs) env, env)
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1, env = import typ1 env in
        let typ2, env = import typ2 env in
        (mk (Tarrow (typ1, typ2, explicit, label)) env, env)
end

module TypeDecl = struct
  open TypeDecl

  let import_field ?must_find env {fld_ident; fld_type; fld_loc= _} =
    let fld_type, env = Type.import ?must_find fld_type env in
    (env, {Type0.fld_ident; fld_type; fld_id= -1})

  let import decl' env =
    let {tdec_ident; tdec_params; tdec_implicit_params; tdec_desc; tdec_loc= _}
        =
      decl'
    in
    let tdec_id =
      match
        Map.find env.resolve_env.type_env.predeclared_types tdec_ident.txt
      with
      | Some (id, num_args, loc) ->
          ( match !num_args with
          | Some num_args ->
              let given = List.length tdec_params in
              if not (Int.equal given num_args) then
                raise
                  (Error
                     ( loc
                     , Wrong_number_args
                         (Lident tdec_ident.txt, given, num_args) ))
          | None ->
              () ) ;
          let {type_env; _} = env.resolve_env in
          env.resolve_env.type_env
          <- { type_env with
               predeclared_types=
                 Map.remove type_env.predeclared_types tdec_ident.txt } ;
          id
      | None ->
          next_id env
    in
    let env = open_expr_scope env in
    let import_params env =
      List.fold_map ~init:env ~f:(fun env param ->
          match param.type_desc with
          | Tvar _ ->
              let var, env = Type.import ~must_find:false param env in
              (env, var)
          | _ ->
              raise (Error (param.type_loc, Expected_type_var param)) )
    in
    let env, tdec_params = import_params env tdec_params in
    let env, tdec_implicit_params = import_params env tdec_implicit_params in
    let decl =
      Type0.
        { tdec_ident
        ; tdec_params
        ; tdec_implicit_params= []
        ; tdec_desc= TAbstract
        ; tdec_id }
    in
    let add_implicits implicit_params =
      if Set.is_empty implicit_params then tdec_implicit_params
      else
        tdec_implicit_params
        |> Set.of_list (module Envi.Type.Comparator)
        |> Set.union implicit_params |> Set.to_list
    in
    (* Make sure the declaration is available to lookup for recursive types. *)
    let env =
      let scope, env = Envi.pop_expr_scope env in
      let env =
        match tdec_desc with
        | TExtend _ ->
            env
        | _ ->
            map_current_scope ~f:(Scope.add_type_declaration decl) env
      in
      Envi.push_scope scope env
    in
    let decl, env =
      match tdec_desc with
      | TAbstract ->
          ({decl with tdec_implicit_params}, env)
      | TAlias typ ->
          let typ, env = Type.import ~must_find:true typ env in
          let tdec_implicit_params =
            add_implicits (Envi.Type.implicit_params env typ)
          in
          ({decl with tdec_desc= TAlias typ; tdec_implicit_params}, env)
      | TUnfold typ ->
          let typ, env = Type.import ~must_find:false typ env in
          let tdec_implicit_params =
            add_implicits (Envi.Type.implicit_params env typ)
          in
          ({decl with tdec_desc= TUnfold typ; tdec_implicit_params}, env)
      | TOpen ->
          ({decl with tdec_desc= TOpen}, env)
      | TRecord fields ->
          let env, fields =
            List.fold_map ~init:env fields ~f:(import_field ~must_find:true)
          in
          let tdec_implicit_params =
            add_implicits
              (Set.union_list
                 (module Envi.Type)
                 (List.map fields ~f:(fun {fld_type; _} ->
                      Envi.Type.implicit_params env fld_type )))
          in
          ({decl with tdec_desc= TRecord fields; tdec_implicit_params}, env)
      | TVariant ctors | TExtend (_, _, ctors) ->
          let env, ctors =
            List.fold_map ~init:env ctors ~f:(fun env ctor ->
                let scope, env = pop_expr_scope env in
                let ctor_ret, env, must_find, ctor_ret_params =
                  match ctor.ctor_ret with
                  | Some ret ->
                      let env = open_expr_scope env in
                      let name =
                        match tdec_desc with
                        | TVariant _ ->
                            Lident tdec_ident.txt
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
                               , Constraints_not_satisfied (ret, decl') )) ) ;
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
                      (env, Type0.Ctor_tuple args)
                  | Ctor_record (_, fields) ->
                      let env, fields =
                        List.fold_map ~init:env fields
                          ~f:(import_field ?must_find)
                      in
                      let decl =
                        mk ~name:ctor.ctor_ident ~params:ctor_ret_params
                          (TRecord fields) env
                      in
                      Envi.Type.map_env env ~f:(TypeEnvi.add_decl decl) ;
                      (env, Type0.Ctor_record (decl.tdec_id, fields))
                in
                let env = push_scope scope (close_expr_scope env) in
                (env, {Type0.ctor_ident= ctor.ctor_ident; ctor_args; ctor_ret})
            )
          in
          let tdec_desc =
            match tdec_desc with
            | TVariant _ ->
                Type0.TVariant ctors
            | TExtend (id, decl, _) ->
                Type0.TExtend (id, decl, ctors)
            | _ ->
                failwith "Expected a TVariant or a TExtend"
          in
          let tdec_implicit_params =
            add_implicits
              (Set.union_list
                 (module Envi.Type)
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
                        (module Envi.Type)
                        (List.map typs ~f:(Envi.Type.implicit_params env)) )))
          in
          ({decl with tdec_desc; tdec_implicit_params}, env)
      | TForward _ ->
          failwith "Cannot import a forward type declaration"
    in
    let env = close_expr_scope env in
    let decl =
      let open Envi in
      let open Type0 in
      (* Insert the implicit arguments in all nested references to this type. *)
      if List.is_empty decl.tdec_implicit_params then decl
      else
        let map_type typ =
          Type.constr_map env typ ~f:(fun variant ->
              let variant =
                if Int.equal variant.var_decl_id decl.tdec_id then
                  {variant with var_implicit_params= decl.tdec_implicit_params}
                else variant
              in
              Tctor variant )
        in
        let map_field field = {field with fld_type= map_type field.fld_type} in
        let map_ctor_args = function
          | Ctor_tuple typs ->
              Ctor_tuple (List.map ~f:map_type typs)
          | Ctor_record (i, fields) ->
              Ctor_record (i, List.map ~f:map_field fields)
        in
        let map_ctor ctor =
          { ctor with
            ctor_args= map_ctor_args ctor.ctor_args
          ; ctor_ret= Option.map ~f:map_type ctor.ctor_ret }
        in
        match decl.tdec_desc with
        | TAbstract | TOpen ->
            decl
        | TAlias typ ->
            {decl with tdec_desc= TAlias (map_type typ)}
        | TUnfold typ ->
            {decl with tdec_desc= TUnfold (map_type typ)}
        | TRecord fields ->
            {decl with tdec_desc= TRecord (List.map ~f:map_field fields)}
        | TVariant ctors ->
            {decl with tdec_desc= TVariant (List.map ~f:map_ctor ctors)}
        | TExtend (lid, base_decl, ctors) ->
            { decl with
              tdec_desc= TExtend (lid, base_decl, List.map ~f:map_ctor ctors)
            }
        | TForward _ ->
            failwith "Cannot import a forward type declaration"
    in
    let env =
      map_current_scope ~f:(Scope.register_type_declaration decl) env
    in
    Envi.Type.map_env env ~f:(TypeEnvi.add_decl decl) ;
    (decl, env)
end

(* Error handling *)

open Format

let pp_typ = Pprint.type_expr

let pp_decl_typ ppf decl =
  pp_typ ppf
    { type_desc=
        Tctor
          { var_ident= mk_lid decl.tdec_ident
          ; var_params= decl.tdec_params
          ; var_implicit_params= decl.tdec_implicit_params }
    ; type_id= -1
    ; type_loc= Location.none }

let report_error ppf = function
  | Unbound_type_var var ->
      fprintf ppf "@[<hov>Unbound type parameter@ @[<h>%a@].@]" pp_typ var
  | Wrong_number_args (lid, given, expected) ->
      fprintf ppf
        "@[The type constructor @[<h>%a@] expects %d argument(s)@ but is here \
         applied to %d argument(s).@]"
        Longident.pp lid expected given
  | Expected_type_var typ ->
      fprintf ppf
        "@[<hov>Syntax error: Expected a type parameter, but got @[<h>%a@].@]"
        pp_typ typ
  | Constraints_not_satisfied (typ, decl) ->
      fprintf ppf
        "@[<hov>Constraints are not satisfied in this type.@ Type @[<h>%a@] \
         should be an instance of @[<h>%a@].@]"
        pp_typ typ pp_decl_typ decl

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
    | _ ->
        None )
