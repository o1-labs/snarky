open Core_kernel
open Ast_types
open Parsetypes
open Envi
open Longident

type error =
  | Unbound_type_var of type_expr
  | Wrong_number_args of Longident.t * int * int
  | Wrong_number_implicit_args of Longident.t * int * int
  | Expected_type_var of type_expr
  | Constraints_not_satisfied of type_expr * type_decl
  | Length_on_non_list_type of Longident.t

exception Error of Location.t * error

(* TODO: undo hack *)
let list = ref None

module Type = struct
  open Type

  let rec import mode ?must_find (typ : type_expr) env : Type0.type_expr * env
      =
    let import' = import mode in
    let import = import mode ?must_find in
    let loc = typ.type_loc in
    match typ.type_desc with
    | Tvar (None, explicitness) -> (
      match (must_find, explicitness) with
      | Some true, Explicit ->
          raise (Error (loc, Unbound_type_var typ))
      | _ ->
          (mkvar mode ~explicitness None env, env) )
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
            (var, env)
        | None ->
            let var = mkvar mode ~explicitness name env in
            (var, add_type_variable x var env) )
    | Tpoly (vars, typ) ->
        let env = open_expr_scope mode env in
        let env, vars =
          List.fold_map vars ~init:env ~f:(fun e t ->
              let t, e = import' ~must_find:false t e in
              (e, t) )
        in
        let typ, env = import typ env in
        let env = close_expr_scope env in
        (mk mode (Tpoly (vars, typ)) env, env)
    | Tctor variant -> (
        let {var_ident; var_params; var_implicit_params; var_length; _} =
          variant
        in
        let decl = raw_find_type_declaration mode var_ident env in
        let import_implicits () =
          List.fold_map ~init:env decl.tdec_implicit_params
            ~f:(Envi.Type.refresh_var mode ~loc ?must_find)
        in
        match decl with
        | {tdec_desc= TUnfold typ; tdec_implicit_params; _} ->
            let env, implicit_params = import_implicits () in
            let new_vars_map =
              List.fold2_exn
                ~init:(Map.empty (module Int))
                tdec_implicit_params implicit_params
                ~f:(fun new_vars_map var param ->
                  Map.set new_vars_map ~key:var.type_id ~data:param )
            in
            let typ = Envi.Type.copy mode ~loc typ new_vars_map env in
            (typ, env)
        | _ ->
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
            let env, var_implicit_params =
              if List.is_empty var_implicit_params then import_implicits ()
              else
                let expected = List.length decl.tdec_implicit_params in
                let given = List.length var_implicit_params in
                if Int.equal expected given then
                  List.fold_map ~init:env var_implicit_params
                    ~f:(fun env param ->
                      let param, env = import' ~must_find:false param env in
                      (env, param) )
                else import_implicits ()
            in
            ( match !list with
            | None ->
                ()
            | Some (list : Type0.type_decl) ->
                if
                  Option.is_some var_length
                  && not (Int.equal decl.tdec_id list.tdec_id)
                then raise (Error (loc, Length_on_non_list_type var_ident.txt))
            ) ;
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
              ; var_decl= decl
              ; var_implicit_params
              ; var_length }
            in
            (mk mode (Tctor variant) env, env) )
    | Ttuple typs ->
        let env, typs =
          List.fold_map typs ~init:env ~f:(fun e t ->
              let t, e = import t e in
              (e, t) )
        in
        (mk mode (Ttuple typs) env, env)
    | Tarrow (typ1, typ2, explicit, label) ->
        let typ1, env = import typ1 env in
        let typ2, env = import typ2 env in
        (mk mode (Tarrow (typ1, typ2, explicit, label)) env, env)

  let fold ~init ~f typ =
    match typ.type_desc with
    | Tvar _ ->
        init
    | Ttuple typs ->
        List.fold ~init ~f typs
    | Tarrow (typ1, typ2, _, _) ->
        let acc = f init typ1 in
        f acc typ2
    | Tctor variant ->
        let acc = List.fold ~init ~f variant.var_params in
        List.fold ~init:acc ~f variant.var_implicit_params
    | Tpoly (typs, typ) ->
        let acc = List.fold ~init ~f typs in
        f acc typ

  let iter ~f = fold ~init:() ~f:(fun () -> f)

  let map ~loc ~f typ =
    match typ.type_desc with
    | Tvar _ ->
        {typ with type_loc= loc}
    | Ttuple typs ->
        let typs = List.map ~f typs in
        {typ with type_desc= Ttuple typs; type_loc= loc}
    | Tarrow (typ1, typ2, explicit, label) ->
        { typ with
          type_desc= Tarrow (f typ1, f typ2, explicit, label)
        ; type_loc= loc }
    | Tctor variant ->
        let variant =
          { variant with
            var_params= List.map ~f variant.var_params
          ; var_implicit_params= List.map ~f variant.var_implicit_params }
        in
        {typ with type_desc= Tctor variant; type_loc= loc}
    | Tpoly (typs, typ) ->
        let typs = List.map ~f typs in
        {typ with type_desc= Tpoly (typs, f typ); type_loc= loc}
end

module TypeDecl = struct
  open TypeDecl

  let import_field mode ?must_find env {fld_ident; fld_type; fld_loc= _} =
    let fld_type, env = Type.import mode ?must_find fld_type env in
    (env, {Type0.fld_ident; fld_type; fld_id= -1})

  let import mode decl' env =
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
    let env = open_expr_scope mode env in
    let import_params env =
      List.fold_map ~init:env ~f:(fun env param ->
          match param.type_desc with
          | Tvar _ ->
              let var, env = Type.import mode ~must_find:false param env in
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
            map_current_scope
              ~f:(FullScope.map_scopes ~f:(Scope.add_type_declaration decl))
              env
      in
      Envi.push_scope scope env
    in
    let decl, env =
      match tdec_desc with
      | TAbstract ->
          ({decl with tdec_implicit_params}, env)
      | TAlias typ ->
          let typ, env = Type.import mode ~must_find:true typ env in
          let tdec_implicit_params =
            add_implicits (Envi.Type.implicit_params env typ)
          in
          ({decl with tdec_desc= TAlias typ; tdec_implicit_params}, env)
      | TUnfold typ ->
          let typ, env = Type.import mode ~must_find:false typ env in
          let tdec_implicit_params =
            add_implicits (Envi.Type.implicit_params env typ)
          in
          ({decl with tdec_desc= TUnfold typ; tdec_implicit_params}, env)
      | TOpen ->
          ({decl with tdec_desc= TOpen}, env)
      | TRecord fields ->
          let env, fields =
            List.fold_map ~init:env fields
              ~f:(import_field mode ~must_find:true)
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
                let ctor_ret, env, must_find =
                  match ctor.ctor_ret with
                  | Some ret ->
                      let env = open_expr_scope mode env in
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
                      let ret, env =
                        Type.import mode ~must_find:false ret env
                      in
                      (Some ret, env, None)
                  | None ->
                      (None, push_scope scope env, Some true)
                in
                let env, ctor_args =
                  match ctor.ctor_args with
                  | Ctor_tuple args ->
                      let env, args =
                        List.fold_map ~init:env args ~f:(fun env arg ->
                            let arg, env =
                              Type.import mode ?must_find arg env
                            in
                            (env, arg) )
                      in
                      (env, Type0.Ctor_tuple args)
                  | Ctor_record (_, fields) ->
                      let env, fields =
                        List.fold_map ~init:env fields
                          ~f:(import_field mode ?must_find)
                      in
                      (* Extract the type variables from the fields' types, use
                         them as effective type parameters.
                      *)
                      let params =
                        List.fold
                          ~init:(Set.empty (module Envi.Type))
                          fields
                          ~f:(fun set {fld_type; _} ->
                            Set.union set (Envi.Type.type_vars fld_type) )
                        |> Set.to_list
                      in
                      let decl =
                        mk ~name:ctor.ctor_ident ~params (TRecord fields) env
                      in
                      (env, Type0.Ctor_record decl)
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
                        | Ctor_record {tdec_desc= TRecord fields; _} ->
                            List.map ~f:(fun {fld_type; _} -> fld_type) fields
                        | Ctor_record _ ->
                            assert false
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
    let () =
      let open Type0 in
      (* Insert the implicit arguments in all nested references to this type. *)
      if List.is_empty decl.tdec_implicit_params then ()
      else
        let rec iter_type typ =
          ( match typ.type_desc with
          | Tctor variant when Int.equal variant.var_decl.tdec_id decl.tdec_id
            ->
              typ.type_desc
              <- Tctor
                   { variant with
                     var_implicit_params= decl.tdec_implicit_params
                   ; var_decl= decl }
          | _ ->
              () ) ;
          Type0.iter ~f:iter_type typ
        in
        let iter_field field = iter_type field.fld_type in
        let iter_ctor_args = function
          | Ctor_tuple typs ->
              List.iter ~f:iter_type typs
          | Ctor_record {tdec_desc= TRecord fields; _} ->
              List.iter ~f:iter_field fields
          | Ctor_record _ ->
              assert false
        in
        let iter_ctor ctor =
          iter_ctor_args ctor.ctor_args ;
          Option.iter ~f:iter_type ctor.ctor_ret
        in
        match decl.tdec_desc with
        | TAbstract | TOpen ->
            ()
        | TAlias typ ->
            iter_type typ
        | TUnfold typ ->
            iter_type typ
        | TRecord fields ->
            List.iter ~f:iter_field fields
        | TVariant ctors ->
            List.iter ~f:iter_ctor ctors
        | TExtend (_lid, _base_decl, ctors) ->
            List.iter ~f:iter_ctor ctors
        | TForward _ ->
            failwith "Cannot import a forward type declaration"
    in
    let env =
      map_current_scope
        ~f:(FullScope.map_scope mode ~f:(Scope.register_type_declaration decl))
        env
    in
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
          ; var_implicit_params= decl.tdec_implicit_params
          ; var_length= None (* TODO *) }
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
  | Wrong_number_implicit_args (lid, given, expected) ->
      fprintf ppf
        "@[The type constructor @[<h>%a@] expects %d implicit argument(s)@ \
         but is here applied to %d implicit argument(s).@]"
        Longident.pp lid expected given
  | Length_on_non_list_type lid ->
      fprintf ppf "@[The type constructor @[<h>%a@] cannot take a length.@]"
        Longident.pp lid
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
