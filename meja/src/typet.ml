open Compiler_internals
open Core_kernel
open Ast_types
open Parsetypes
open Envi

type error =
  | Unbound_type_var of type_expr
  | Wrong_number_args of Path.t * int * int
  | Expected_type_var of type_expr
  | Constraints_not_satisfied of type_expr * type_decl

exception Error of Location.t * error

let type0 {Typedast.type_type; _} = type_type

module Type = struct
  open Type

  let rec import ?must_find (typ : type_expr) env : Typedast.type_expr * env =
    let mode = Envi.current_mode env in
    let import' = import in
    let import = import ?must_find in
    let loc = typ.type_loc in
    match typ.type_desc with
    | Ptyp_var None -> (
      match must_find with
      | Some true ->
          raise (Error (loc, Unbound_type_var typ))
      | _ ->
          ( {type_desc= Ttyp_var None; type_loc= loc; type_type= mkvar None env}
          , env ) )
    | Ptyp_var (Some {txt= x; _} as name) ->
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
        let type_type, env =
          match var with
          | Some var ->
              (var, env)
          | None ->
              let var = mkvar (Some x) env in
              (var, add_type_variable x var env)
        in
        ({type_desc= Ttyp_var name; type_loc= loc; type_type}, env)
    | Ptyp_poly (vars, typ) ->
        let env = open_expr_scope env in
        let env, vars =
          List.fold_map vars ~init:env ~f:(fun e t ->
              let t, e = import' ~must_find:false t e in
              (e, t) )
        in
        let typ, env = import typ env in
        let env = close_expr_scope env in
        ( { type_desc= Ttyp_poly (vars, typ)
          ; type_loc= loc
          ; type_type= mk (Tpoly (List.map ~f:type0 vars, type0 typ)) env }
        , env )
    | Ptyp_ctor variant ->
        let {var_ident; var_params} = variant in
        let var_ident, decl = raw_find_type_declaration ~mode var_ident env in
        let var_ident = Location.mkloc var_ident variant.var_ident.loc in
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
                   (var_ident.txt, given_args_length, expected_args_length) )) ;
        let env, var_params =
          List.fold_map ~init:env var_params ~f:(fun env param ->
              let param, env = import param env in
              (env, param) )
        in
        let typ =
          let variant =
            { Type0.var_params= List.map ~f:type0 var_params
            ; var_ident= var_ident.txt
            ; var_decl= decl }
          in
          mk (Tctor variant) env
        in
        ( { type_desc= Ttyp_ctor {var_params; var_ident}
          ; type_loc= loc
          ; type_type= typ }
        , env )
    | Ptyp_tuple typs ->
        let env, typs =
          List.fold_map typs ~init:env ~f:(fun e t ->
              let t, e = import t e in
              (e, t) )
        in
        let typ = mk (Ttuple (List.map ~f:type0 typs)) env in
        ({type_desc= Ttyp_tuple typs; type_loc= loc; type_type= typ}, env)
    | Ptyp_arrow (typ1, typ2, explicit, label) ->
        let typ1, env = import typ1 env in
        let typ2, env = import typ2 env in
        let typ = mk (Tarrow (type0 typ1, type0 typ2, explicit, label)) env in
        ( { type_desc= Ttyp_arrow (typ1, typ2, explicit, label)
          ; type_loc= loc
          ; type_type= typ }
        , env )
    | Ptyp_prover typ ->
        let env = open_expr_scope ~mode:Prover env in
        let typ, env = import typ env in
        let env =
          let scope, env = pop_expr_scope env in
          join_expr_scope env scope
        in
        (typ, env)

  let fold ~init ~f typ =
    match typ.type_desc with
    | Ptyp_var _ ->
        init
    | Ptyp_tuple typs ->
        List.fold ~init ~f typs
    | Ptyp_arrow (typ1, typ2, _, _) ->
        let acc = f init typ1 in
        f acc typ2
    | Ptyp_ctor variant ->
        List.fold ~init ~f variant.var_params
    | Ptyp_poly (typs, typ) ->
        let acc = List.fold ~init ~f typs in
        f acc typ
    | Ptyp_prover typ ->
        f init typ

  let iter ~f = fold ~init:() ~f:(fun () -> f)

  let map ~loc ~f typ =
    match typ.type_desc with
    | Ptyp_var _ ->
        {typ with type_loc= loc}
    | Ptyp_tuple typs ->
        let typs = List.map ~f typs in
        {type_desc= Ptyp_tuple typs; type_loc= loc}
    | Ptyp_arrow (typ1, typ2, explicit, label) ->
        {type_desc= Ptyp_arrow (f typ1, f typ2, explicit, label); type_loc= loc}
    | Ptyp_ctor variant ->
        let variant =
          {variant with var_params= List.map ~f variant.var_params}
        in
        {type_desc= Ptyp_ctor variant; type_loc= loc}
    | Ptyp_poly (typs, typ) ->
        let typs = List.map ~f typs in
        {type_desc= Ptyp_poly (typs, f typ); type_loc= loc}
    | Ptyp_prover typ ->
        {type_desc= Ptyp_prover (f typ); type_loc= loc}
end

module TypeDecl = struct
  open TypeDecl

  let generalise decl =
    let poly_name =
      map_loc decl.tdec_ident ~f:(fun name ->
          if name = "t" then "poly" else name ^ "_poly" )
    in
    match decl.tdec_desc with
    | Pdec_record fields ->
        let field_vars =
          List.map fields ~f:(fun {fld_ident; fld_type= _; fld_loc} ->
              {type_desc= Ptyp_var (Some fld_ident); type_loc= fld_loc} )
        in
        let poly_decl =
          { tdec_ident= poly_name
          ; tdec_params= field_vars
          ; tdec_desc=
              Pdec_record
                (List.map2_exn fields field_vars ~f:(fun fld fld_type ->
                     {fld with fld_type} ))
          ; tdec_loc= decl.tdec_loc }
        in
        let alias_typ =
          { type_desc=
              Ptyp_ctor
                { var_ident=
                    map_loc poly_name ~f:(fun name -> Longident.Lident name)
                ; var_params=
                    List.map fields ~f:(fun {fld_type; _} -> fld_type) }
          ; type_loc= decl.tdec_loc }
        in
        (poly_decl, {decl with tdec_desc= Pdec_alias alias_typ})
    | Pdec_variant _ ->
        (* Not sure what the right thing to do here is. GADTs make this
           complicated.
        *)
        assert false
    | _ ->
        (* We don't have enough information about the type to generalise it. *)
        assert false

  let import_field ?must_find env {fld_ident; fld_type; fld_loc} =
    let mode = Envi.current_mode env in
    let fld_type, env = Type.import ?must_find fld_type env in
    let fld_ident = map_loc ~f:(Ident.create ~mode) fld_ident in
    ( env
    , { Typedast.fld_ident
      ; fld_type
      ; fld_loc
      ; fld_fld= {Type0.fld_ident= fld_ident.txt; fld_type= fld_type.type_type}
      } )

  let import_ctor env ctor =
    let mode = current_mode env in
    let scope, env = pop_expr_scope env in
    let ctor_ret, env, must_find =
      match ctor.ctor_ret with
      | Some ret ->
          let env = open_expr_scope env in
          let ret, env = Type.import ~must_find:false ret env in
          (Some ret, env, None)
      | None ->
          (None, push_scope scope env, Some true)
    in
    let env, ctor_args =
      match ctor.ctor_args with
      | Ctor_tuple args ->
          let env, args =
            List.fold_map ~init:env args ~f:(fun env arg ->
                let arg, env = Type.import ?must_find arg env in
                (env, arg) )
          in
          (env, Typedast.Tctor_tuple args)
      | Ctor_record fields ->
          let env, fields =
            List.fold_map ~init:env fields ~f:(import_field ?must_find)
          in
          (env, Typedast.Tctor_record fields)
    in
    let type0_ctor_args =
      match ctor_args with
      | Tctor_tuple args ->
          Type0.Ctor_tuple (List.map ~f:type0 args)
      | Tctor_record fields ->
          (* Extract the type variables from the fields' types, use
             them as effective type parameters.
          *)
          let params =
            List.fold ~init:Typeset.empty fields
              ~f:(fun set {fld_fld= {fld_type; _}; _} ->
                Set.union set (Type1.type_vars fld_type) )
            |> Set.to_list
          in
          let decl =
            mk
              ~name:(Ident.create ~mode ctor.ctor_ident.txt)
              ~params
              (TRecord (List.map ~f:(fun {fld_fld= f; _} -> f) fields))
          in
          Type0.Ctor_record decl
    in
    let env = push_scope scope (close_expr_scope env) in
    let ctor_ident = map_loc ~f:(Ident.create ~mode) ctor.ctor_ident in
    ( env
    , { Typedast.ctor_ident
      ; ctor_args
      ; ctor_ret
      ; ctor_loc= ctor.ctor_loc
      ; ctor_ctor=
          { Type0.ctor_ident= ctor_ident.txt
          ; ctor_args= type0_ctor_args
          ; ctor_ret= Option.map ~f:type0 ctor_ret } } )

  let import decl' env =
    let mode = Envi.current_mode env in
    let {tdec_ident; tdec_params; tdec_desc; tdec_loc} = decl' in
    let tdec_ident, tdec_id =
      match
        IdTbl.find_name ~modes:(modes_of_mode mode) tdec_ident.txt
          env.resolve_env.type_env.predeclared_types
      with
      | Some (ident, (id, num_args, loc)) ->
          ( match !num_args with
          | Some num_args ->
              let given = List.length tdec_params in
              if not (Int.equal given num_args) then
                raise
                  (Error
                     (loc, Wrong_number_args (Pident ident, given, num_args)))
          | None ->
              () ) ;
          let {type_env; _} = env.resolve_env in
          env.resolve_env.type_env
          <- { type_env with
               predeclared_types= IdTbl.remove ident type_env.predeclared_types
             } ;
          (Location.mkloc ident tdec_ident.loc, id)
      | None ->
          (map_loc ~f:(Ident.create ~mode) tdec_ident, next_id env)
    in
    let env = open_expr_scope env in
    let import_params env =
      List.fold_map ~init:env ~f:(fun env param ->
          match param.type_desc with
          | Ptyp_var _ ->
              let var, env = Type.import ~must_find:false param env in
              (env, var)
          | _ ->
              raise (Error (param.type_loc, Expected_type_var param)) )
    in
    let env, tdec_params = import_params env tdec_params in
    let decl =
      Type0.
        { tdec_ident= tdec_ident.txt
        ; tdec_params= List.map ~f:type0 tdec_params
        ; tdec_desc= TAbstract
        ; tdec_id }
    in
    (* Make sure the declaration is available to lookup for recursive types. *)
    let env =
      let scope, env = Envi.pop_expr_scope env in
      let env =
        match tdec_desc with
        | Pdec_extend _ ->
            env
        | _ ->
            map_current_scope ~f:(Scope.add_type_declaration decl) env
      in
      Envi.push_scope scope env
    in
    let typedast_decl =
      { Typedast.tdec_ident
      ; tdec_params
      ; tdec_desc= Tdec_abstract
      ; tdec_loc
      ; tdec_tdec= decl }
    in
    let decl, env =
      match tdec_desc with
      | Pdec_abstract ->
          (typedast_decl, env)
      | Pdec_alias typ ->
          let typ, env = Type.import ~must_find:true typ env in
          let decl = {decl with tdec_desc= TAlias typ.type_type} in
          let typedast_decl =
            {typedast_decl with tdec_desc= Tdec_alias typ; tdec_tdec= decl}
          in
          (typedast_decl, env)
      | Pdec_open ->
          let decl = {decl with tdec_desc= TOpen} in
          let typedast_decl =
            {typedast_decl with tdec_desc= Tdec_open; tdec_tdec= decl}
          in
          (typedast_decl, env)
      | Pdec_record fields ->
          let env, fields =
            List.fold_map ~init:env fields ~f:(import_field ~must_find:true)
          in
          let decl =
            { decl with
              tdec_desc=
                TRecord (List.map ~f:(fun {fld_fld= f; _} -> f) fields) }
          in
          let typedast_decl =
            {typedast_decl with tdec_desc= Tdec_record fields; tdec_tdec= decl}
          in
          (typedast_decl, env)
      | Pdec_variant ctors | Pdec_extend (_, _, ctors) ->
          let name =
            match tdec_desc with
            | Pdec_variant _ ->
                Path.Pident tdec_ident.txt
            | Pdec_extend (lid, _, _) ->
                lid.txt
            | _ ->
                failwith "Could not find name for TVariant/TExtend."
          in
          let env, ctors =
            List.fold_map ~init:env ctors ~f:(fun env ctor ->
                let ret = ctor.ctor_ret in
                let env, ctor = import_ctor env ctor in
                ( match (ctor.ctor_ret, ret) with
                | Some {type_desc= Ttyp_ctor {var_ident= path; _}; _}, _
                  when Path.compare path.txt name = 0 ->
                    ()
                | Some _, Some ret ->
                    raise
                      (Error
                         (ret.type_loc, Constraints_not_satisfied (ret, decl')))
                | Some _, None ->
                    assert false
                | _ ->
                    () ) ;
                (env, ctor) )
          in
          let typedast_tdec_desc, tdec_desc =
            match tdec_desc with
            | Pdec_variant _ ->
                ( Typedast.Tdec_variant ctors
                , Type0.TVariant
                    (List.map ~f:(fun {ctor_ctor= c; _} -> c) ctors) )
            | Pdec_extend (id, decl, _) ->
                ( Typedast.Tdec_extend (id, decl, ctors)
                , Type0.TExtend
                    ( id.txt
                    , decl
                    , List.map ~f:(fun {ctor_ctor= c; _} -> c) ctors ) )
            | _ ->
                failwith "Expected a TVariant or a TExtend"
          in
          let decl = {decl with tdec_desc} in
          let typedast_decl =
            {typedast_decl with tdec_desc= typedast_tdec_desc; tdec_tdec= decl}
          in
          (typedast_decl, env)
    in
    let env = close_expr_scope env in
    let env =
      map_current_scope ~f:(Scope.register_type_declaration decl.tdec_tdec) env
    in
    (decl, env)
end

(* Error handling *)

open Format

let pp_typ = Pprint.type_expr

let pp_decl_typ ppf decl =
  pp_typ ppf
    { type_desc=
        Ptyp_ctor
          {var_ident= mk_lid decl.tdec_ident; var_params= decl.tdec_params}
    ; type_loc= Location.none }

let report_error ppf = function
  | Unbound_type_var var ->
      fprintf ppf "@[<hov>Unbound type parameter@ @[<h>%a@].@]" pp_typ var
  | Wrong_number_args (path, given, expected) ->
      fprintf ppf
        "@[The type constructor @[<h>%a@] expects %d argument(s)@ but is here \
         applied to %d argument(s).@]"
        Path.pp path expected given
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
        Some (Location.error_of_printer ~loc report_error err)
    | _ ->
        None )
