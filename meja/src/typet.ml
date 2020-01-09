open Compiler_internals
open Core_kernel
open Ast_types
open Parsetypes
open Envi

type error =
  | Unbound_type_var of string
  | Wrong_number_args of Path.t * int * int
  | Expected_type_var of type_expr
  | Constraints_not_satisfied of type_expr * type_decl
  | Opaque_type_in_prover_mode of type_expr
  | Convertible_arities_differ of string * int * string * int
  | GADT_in_nonrec_type
  | Repeated_row_label of Ident.t
  | Missing_row_label of Ident.t
  | Expected_row_type of type_expr

exception Error of Location.t * error

let type0 {Typedast.type_type; _} = type_type

let unify = ref (fun ~loc:_ _env _typ1 _typ2 -> failwith "Undefined")

module Type = struct
  open Type

  (* Unique identifier to refer to the fake `opaque` type constructor. *)
  let opaque = Ident.create ~mode:Checked "opaque"

  let mk_poly ~loc ~mode vars typ env =
    { Typedast.type_desc= Ttyp_poly (vars, typ)
    ; type_loc= loc
    ; type_type= Mk.poly ~mode (List.map ~f:type0 vars) (type0 typ) env }

  let mk_tuple ~loc ~mode typs env =
    { Typedast.type_desc= Ttyp_tuple typs
    ; type_loc= loc
    ; type_type= Mk.tuple ~mode (List.map ~f:type0 typs) env }

  let mk_arrow ~loc ~mode ?(explicit = Explicit) ?(label = Nolabel) typ1 typ2
      env =
    { Typedast.type_desc= Ttyp_arrow (typ1, typ2, explicit, label)
    ; type_loc= loc
    ; type_type= Mk.arrow ~mode ~explicit ~label (type0 typ1) (type0 typ2) env
    }

  let mk_prover ~loc ~mode typ =
    { Typedast.type_desc= Ttyp_prover typ
    ; type_loc= loc
    ; type_type= Type1.get_mode mode typ.type_type }

  let mk_conv ~loc ~mode typ1 typ2 env =
    { Typedast.type_desc= Ttyp_conv (typ1, typ2)
    ; type_loc= loc
    ; type_type= Envi.Type.Mk.conv ~mode (type0 typ1) (type0 typ2) env }

  let mk_opaque ~loc typ env =
    { Typedast.type_desc= Ttyp_opaque typ
    ; type_loc= loc
    ; type_type= Envi.Type.Mk.opaque ~mode:Checked (type0 typ) env }

  let rec import ?must_find (typ : type_expr) env : Typedast.type_expr * env =
    let mode = Envi.current_mode env in
    let import' = import in
    let import = import ?must_find in
    let loc = typ.type_loc in
    match typ.type_desc with
    | Ptyp_var None -> (
      match must_find with
      | Some true ->
          raise (Error (loc, Unbound_type_var "_"))
      | _ ->
          ( { type_desc= Ttyp_var None
            ; type_loc= loc
            ; type_type= mkvar ~mode None env }
          , env ) )
    | Ptyp_var (Some {txt= x; _} as name) ->
        let var =
          match must_find with
          | Some true ->
              let var = find_type_variable ~mode x env in
              if Option.is_none var then
                raise (Error (loc, Unbound_type_var x)) ;
              var
          | Some false ->
              None
          | None ->
              find_type_variable ~mode x env
        in
        let type_type, env =
          match var with
          | Some var ->
              (var, env)
          | None ->
              let var = mkvar ~mode (Some x) env in
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
        (mk_poly ~loc ~mode vars typ env, env)
    | Ptyp_ctor {var_ident= {txt= Lident "opaque"; _} as var_ident; var_params}
      when not (Envi.has_type_declaration ~mode var_ident env) -> (
      (* Special-case the [opaque] type constructor when it's not otherwise
           bound in the environment. This avoids the need for an extra keyword
           or any specific reserved syntax.
        *)
      match var_params with
      | [typ'] ->
          import {typ with type_desc= Ptyp_opaque typ'} env
      | _ ->
          raise
            (Error
               ( loc
               , Wrong_number_args
                   (Path.Pident opaque, List.length var_params, 1) )) )
    | Ptyp_ctor variant ->
        let {var_ident; var_params} = variant in
        let var_ident, decl = raw_find_type_declaration ~mode var_ident env in
        let var_ident = Location.mkloc var_ident variant.var_ident.loc in
        let given_args_length = List.length var_params in
        let expected_args_length = List.length decl.tdec_params in
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
          Envi.Type.instantiate decl.tdec_params
            (List.map ~f:type0 var_params)
            (Type1.get_mode mode decl.tdec_ret)
            env
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
        (mk_tuple ~loc ~mode typs env, env)
    | Ptyp_arrow (typ1, typ2, explicit, label) ->
        let typ1, env = import typ1 env in
        let typ2, env = import typ2 env in
        (mk_arrow ~loc ~mode ~explicit ~label typ1 typ2 env, env)
    | Ptyp_prover typ ->
        let env = open_expr_scope ~mode:Prover env in
        let typ, env = import typ env in
        let env =
          let scope, env = pop_expr_scope env in
          join_expr_scope env scope
        in
        (mk_prover ~loc ~mode typ, env)
    | Ptyp_conv (typ1, typ2) ->
        let env = open_expr_scope ~mode:Checked env in
        let typ1, env = import typ1 env in
        let env =
          let scope, env = pop_expr_scope env in
          join_expr_scope env scope
        in
        let env = open_expr_scope ~mode:Prover env in
        let typ2, env = import typ2 env in
        let env =
          let scope, env = pop_expr_scope env in
          join_expr_scope env scope
        in
        (mk_conv ~loc ~mode typ1 typ2 env, env)
    | Ptyp_opaque typ' ->
        if equal_mode Prover mode then
          raise (Error (loc, Opaque_type_in_prover_mode typ)) ;
        let env = open_expr_scope ~mode:Prover env in
        let typ, env = import typ' env in
        let env =
          let scope, env = pop_expr_scope env in
          join_expr_scope env scope
        in
        (mk_opaque ~loc typ env, env)
    | Ptyp_alias (typ, name) ->
        let var =
          match must_find with
          | Some true ->
              let var = find_type_variable ~mode name.txt env in
              if Option.is_none var then
                raise (Error (loc, Unbound_type_var name.txt)) ;
              var
          | Some false ->
              None
          | None ->
              find_type_variable ~mode name.txt env
        in
        let var, env =
          match var with
          | Some var ->
              (var, env)
          | None ->
              let var = mkvar ~mode (Some name.txt) env in
              (var, add_type_variable name.txt var env)
        in
        let typ, env = import typ env in
        !unify ~loc env typ.type_type var ;
        ( { Typedast.type_desc= Ttyp_alias (typ, name)
          ; type_loc= loc
          ; type_type= typ.type_type }
        , env )
    | Ptyp_row (tags, row_closed, min_tags) ->
        let pres =
          match (row_closed, min_tags) with
          | Closed, Some _ ->
              Type0.RpMaybe
          | Open, _ | Closed, None ->
              Type0.RpPresent
        in
        let (env, row_tags), tags =
          List.fold_map ~init:(env, Ident.Map.empty) tags
            ~f:(fun (env, row_tags) {rtag_ident; rtag_arg; rtag_loc} ->
              let env, rtag_arg =
                List.fold_map rtag_arg ~init:env ~f:(fun e t ->
                    let t, e = import t e in
                    (e, t) )
              in
              let rtag_ident = map_loc ~f:Ident.create_row rtag_ident in
              let args = List.map ~f:type0 rtag_arg in
              let row_tags =
                match
                  Map.add row_tags ~key:rtag_ident.txt
                    ~data:(Path.Pident rtag_ident.txt, Type1.mk_rp pres, args)
                with
                | `Duplicate ->
                    raise
                      (Error (rtag_ident.loc, Repeated_row_label rtag_ident.txt))
                | `Ok row_tags ->
                    row_tags
              in
              ((env, row_tags), {Typedast.rtag_ident; rtag_arg; rtag_loc}) )
        in
        let min_tags =
          Option.map ~f:(List.map ~f:(map_loc ~f:Ident.create_row)) min_tags
        in
        let set_present tag =
          match Map.find row_tags tag.Location.txt with
          | None ->
              raise (Error (tag.loc, Missing_row_label tag.txt))
          | Some (_path, pres, _args) ->
              pres.Type0.rp_desc <- RpPresent
        in
        Option.iter ~f:(List.iter ~f:set_present) min_tags ;
        let row_rest = Envi.Type.mkvar ~mode None env in
        let type_type =
          Envi.Type.Mk.row ~mode
            { row_tags
            ; row_closed
            ; row_rest
            ; row_presence_proxy= Type1.mk_rp RpPresent }
            env
        in
        ( { Typedast.type_desc= Ttyp_row (tags, row_closed, min_tags)
          ; type_loc= loc
          ; type_type }
        , env )
    | Ptyp_row_subtract (typ, tags) ->
        let typ', env = import typ env in
        let tags = List.map ~f:(map_loc ~f:Ident.create_row) tags in
        let type_type =
          let typ' = Type1.repr typ'.type_type in
          let (row_tags, row_rest, row_closed), row_presence_proxy =
            match typ'.type_desc with
            | Tvar name ->
                let row_tags = Ident.Map.empty in
                let row_closed = Open in
                let row_rest =
                  Type1.Mk.var ~mode:typ'.type_mode typ'.type_depth name
                in
                let row_presence_proxy = Type1.mk_rp RpPresent in
                let row =
                  {Type0.row_tags; row_closed; row_rest; row_presence_proxy}
                in
                let instance_typ =
                  Type1.Mk.row ~mode:typ'.type_mode typ'.type_depth row
                in
                let instance_typ =
                  if phys_equal typ' typ'.type_alternate.type_alternate then
                    instance_typ.type_alternate.type_alternate
                  else instance_typ
                in
                Type1.add_instance ~unify:(!unify ~loc env) typ' instance_typ ;
                ((row_tags, row_rest, row_closed), row_presence_proxy)
            | Trow row ->
                (Type1.row_repr row, row.row_presence_proxy)
            | _ ->
                raise (Error (typ.type_loc, Expected_row_type typ))
          in
          match row_closed with
          | Open ->
              let row_tags =
                List.fold ~init:row_tags tags ~f:(fun row_tags {txt= tag; _} ->
                    match Map.find row_tags tag with
                    | Some (path, pres, args) ->
                        Map.set row_tags ~key:tag
                          ~data:(path, Type1.mk_rp (RpSubtract pres), args)
                    | None ->
                        Map.set row_tags ~key:tag
                          ~data:
                            ( Path.Pident tag
                            , Type1.mk_rp (RpSubtract (Type1.mk_rp RpAny))
                            , [] ) )
              in
              Envi.Type.Mk.row ~mode
                {row_tags; row_closed; row_rest; row_presence_proxy}
                env
          | Closed ->
              let row_tags =
                List.fold ~init:row_tags tags
                  ~f:(fun row_tags {txt= tag; loc} ->
                    match Map.find row_tags tag with
                    | Some (path, pres, args) ->
                        Map.set row_tags ~key:tag
                          ~data:(path, Type1.mk_rp (RpSubtract pres), args)
                    | None ->
                        raise (Error (loc, Missing_row_label tag)) )
              in
              Envi.Type.Mk.row ~mode
                {row_tags; row_closed; row_rest; row_presence_proxy}
                env
        in
        ( { Typedast.type_desc= Ttyp_row_subtract (typ', tags)
          ; type_loc= loc
          ; type_type }
        , env )

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
    | Ptyp_conv (typ1, typ2) ->
        let acc = f init typ1 in
        f acc typ2
    | Ptyp_opaque typ ->
        f init typ
    | Ptyp_alias (typ, _) ->
        f init typ
    | Ptyp_row (tags, _closed, _min_tags) ->
        List.fold ~init tags ~f:(fun acc {rtag_arg; _} ->
            List.fold ~f ~init:acc rtag_arg )
    | Ptyp_row_subtract (typ, _tags) ->
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
    | Ptyp_conv (typ1, typ2) ->
        {type_desc= Ptyp_conv (f typ1, f typ2); type_loc= loc}
    | Ptyp_opaque typ ->
        {type_desc= Ptyp_opaque (f typ); type_loc= loc}
    | Ptyp_alias (typ, name) ->
        {type_desc= Ptyp_alias (f typ, name); type_loc= loc}
    | Ptyp_row (tags, closed, min_tags) ->
        { type_desc=
            Ptyp_row
              ( List.map tags ~f:(fun tag ->
                    {tag with rtag_arg= List.map ~f tag.rtag_arg} )
              , closed
              , min_tags )
        ; type_loc= loc }
    | Ptyp_row_subtract (typ, tags) ->
        {type_desc= Ptyp_row_subtract (f typ, tags); type_loc= loc}
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

  let predeclare env
      {Parsetypes.tdec_ident; tdec_params; tdec_desc; tdec_loc= loc} =
    match tdec_desc with
    | Pdec_extend _ ->
        env
    | _ ->
        let mode = Envi.current_mode env in
        let ident = Ident.create ~mode tdec_ident.txt in
        let params =
          List.map tdec_params ~f:(fun _ -> Envi.Type.mkvar ~mode None env)
        in
        let decl =
          { Type0.tdec_params= params
          ; tdec_desc= TAbstract
          ; tdec_id= next_id ()
          ; tdec_ret= Envi.Type.Mk.ctor ~mode (Path.Pident ident) params env }
        in
        Envi.TypeDecl.predeclare ident decl env ;
        map_current_scope ~f:(Scope.add_type_declaration ~loc ident decl) env

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
    let ctor_ident = map_loc ~f:(Ident.create ~mode) ctor.ctor_ident in
    let env, type0_ctor_args =
      match ctor_args with
      | Tctor_tuple args ->
          (env, Type0.Ctor_tuple (List.map ~f:type0 args))
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
            mk ~name:ctor_ident.txt ~params
              (TRecord (List.map ~f:(fun {fld_fld= f; _} -> f) fields))
          in
          (* Add the type declaration to the outer scope. *)
          let scope, env = Envi.pop_scope env in
          let env =
            map_current_scope
              ~f:
                (Scope.add_type_declaration ~loc:ctor.ctor_loc ~may_shadow:true
                   ctor_ident.txt decl)
              env
          in
          let env = Envi.push_scope scope env in
          (env, Type0.Ctor_record decl)
    in
    let env = push_scope scope (close_expr_scope env) in
    ( env
    , { Typedast.ctor_ident
      ; ctor_args
      ; ctor_ret
      ; ctor_loc= ctor.ctor_loc
      ; ctor_ctor=
          { Type0.ctor_ident= ctor_ident.txt
          ; ctor_args= type0_ctor_args
          ; ctor_ret= Option.map ~f:type0 ctor_ret } } )

  (* TODO: Make prover mode declarations stitch to opaque types. *)
  let import ?name ?other_name ?tri_stitched ?(newtype = false) ~recursive
      decl' env =
    let mode = Envi.current_mode env in
    let {tdec_ident; tdec_params; tdec_desc; tdec_loc} = decl' in
    let tdec_ident, path, tdec_id =
      match
        IdTbl.find_name ~modes:(modes_of_mode mode) tdec_ident.txt
          env.resolve_env.type_env.predeclared_types
      with
      | Some (ident, id) ->
          (map_loc ~f:(fun _ -> ident) tdec_ident, Path.Pident ident, id)
      | None -> (
        match tdec_desc with
        | Pdec_extend (path, _) ->
            let tdec_ident, _ =
              Envi.raw_get_type_declaration ~loc:path.loc path.txt env
            in
            (map_loc ~f:(fun _ -> tdec_ident) path, path.txt, next_id ())
        | _ ->
            let ident =
              match name with
              | Some name ->
                  map_loc ~f:(fun _ -> name) tdec_ident
              | None ->
                  map_loc ~f:(Ident.create ~mode) tdec_ident
            in
            (ident, Path.Pident ident.txt, next_id ()) )
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
    let params = List.map ~f:type0 tdec_params in
    let tdec_ret =
      match (other_name, tri_stitched) with
      | Some _, Some _ ->
          (* Disallowed. *)
          assert false
      | Some other_path, None ->
          (* Directly stitched types. *)
          Type1.Mk.ctor ~mode 10000 path ~other_path params
      | None, Some tri_typ ->
          (* Tri-stitched types. *)
          assert (equal_mode mode Checked) ;
          (* Evaluate [tri_typ] in the current environment so that it has
             access to the type parameters.
          *)
          let tri_typ = tri_typ env params in
          assert (equal_mode tri_typ.Type0.type_mode Prover) ;
          let typ =
            Type1.mk' ~mode:Checked 10000
              (Tctor {var_ident= path; var_params= params})
          in
          typ.type_alternate <- tri_typ ;
          typ
      | None, None -> (
          (* This type doesn't have a proper alternate, make it opaque. *)
          let opaque =
            let ctor = Type1.Mk.ctor ~mode 10000 path params in
            Type1.Mk.opaque ~mode 10000 (Type1.get_mode Prover ctor)
          in
          match mode with
          | Prover ->
              (* Prover-mode 'opaque' types aren't really opaque, so leave as
                 is.
              *)
              opaque
          | Checked ->
              (* Tri-stitch to ensure that [tdec_ret] isn't opaque itself. *)
              let typ =
                Type1.mk' ~mode:Checked 10000
                  (Tctor {var_ident= path; var_params= params})
              in
              typ.type_alternate <- Type1.get_mode Prover opaque ;
              typ )
    in
    let decl =
      Type0.{tdec_params= params; tdec_desc= TAbstract; tdec_id; tdec_ret}
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
      | Pdec_variant ctors | Pdec_extend (_, ctors) ->
          let name =
            match tdec_desc with
            | Pdec_variant _ ->
                Path.Pident tdec_ident.txt
            | Pdec_extend (lid, _) ->
                lid.txt
            | _ ->
                failwith "Could not find name for TVariant/TExtend."
          in
          let env, ctors =
            List.fold_map ~init:env ctors ~f:(fun env ctor ->
                let ret = ctor.ctor_ret in
                if (not recursive) && Option.is_some ctor.ctor_ret then
                  raise (Error (ctor.ctor_loc, GADT_in_nonrec_type)) ;
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
            | Pdec_extend (id, _) ->
                ( Typedast.Tdec_extend (id, ctors)
                , Type0.TExtend
                    (id.txt, List.map ~f:(fun {ctor_ctor= c; _} -> c) ctors) )
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
      map_current_scope
        ~f:
          (Scope.register_type_declaration ~may_shadow:newtype
             ~loc:tdec_ident.loc tdec_ident.txt decl.tdec_tdec)
        env
    in
    (decl, env)

  let import_convertible decl type_conv env =
    let mode = current_mode env in
    assert (equal_mode mode Checked) ;
    match type_conv with
    | Ptconv_with (other_mode, conv_decl) -> (
        let decl_len = List.length decl.tdec_params in
        let conv_len = List.length conv_decl.tdec_params in
        if not (Int.equal decl_len conv_len) then
          raise
            (Error
               ( conv_decl.tdec_loc
               , Convertible_arities_differ
                   ( decl.tdec_ident.txt
                   , decl_len
                   , conv_decl.tdec_ident.txt
                   , conv_len ) )) ;
        let name =
          match other_mode with
          | Prover when decl.tdec_ident.txt = conv_decl.tdec_ident.txt ->
              let name = Ident.create ~mode ~ocaml:true decl.tdec_ident.txt in
              let name' = decl.tdec_ident.txt in
              Option.iter (Ident.ocaml_name_ref name) ~f:(fun name ->
                  name := if name' = "t" then "var" else name' ^ "_var" ) ;
              name
          | _ ->
              Ident.create ~mode decl.tdec_ident.txt
        in
        let other_name =
          Ident.create ~mode:other_mode conv_decl.tdec_ident.txt
        in
        let decl, env =
          import ~name ~other_name:(Path.Pident other_name) ~recursive:false
            decl env
        in
        match other_mode with
        | Checked ->
            (* Tri-stitch. The types are related as
               [other_name@{C} -> other_name@{P} <-> name@{C}].
            *)
            let tri_stitched env params =
              Envi.Type.instantiate decl.tdec_tdec.tdec_params params
                (Type1.get_mode Prover decl.tdec_tdec.tdec_ret)
                env
            in
            let conv_decl, env =
              import ~name:other_name ~tri_stitched ~recursive:false conv_decl
                env
            in
            (decl, Typedast.Ttconv_with (other_mode, conv_decl), env)
        | Prover ->
            (* Stitch. The types are related as
               [name@{C} <-> other_name@{P}].
            *)
            let env = Envi.open_mode_module_scope other_mode env in
            let conv_decl, env =
              import ~name:other_name ~other_name:(Path.Pident name)
                ~recursive:false conv_decl env
            in
            let env = Envi.open_mode_module_scope mode env in
            (decl, Typedast.Ttconv_with (other_mode, conv_decl), env) )
    | Ptconv_to typ ->
        (* Tri-stitch to an existing stitching as
           [decl@{C} -> typ@{P} <-> typ@{C}].
        *)
        (* Sad hack, but we need to evaluate in the declaration environment to
           find the type variables, and also be able to return the type.
        *)
        let typ' = ref None in
        let tri_stitched env _params =
          (* Interpret in prover mode. *)
          let env = Envi.open_expr_scope ~mode:Prover env in
          let typ, _env = Type.import ~must_find:true typ env in
          typ' := Some typ ;
          typ.type_type
        in
        let decl, env = import ~tri_stitched ~recursive:false decl env in
        let typ = Option.value_exn !typ' in
        (decl, Ttconv_to typ, env)

  let import_rec decls env =
    let env = List.fold ~f:predeclare ~init:env decls in
    let env, decls =
      List.fold_map ~init:env decls ~f:(fun env decl ->
          let decl, env = import ~recursive:true decl env in
          (env, decl) )
    in
    Envi.TypeDecl.clear_predeclared env ;
    (decls, env)

  let import ?name ?other_name ?tri_stitched =
    import ?name ?other_name ?tri_stitched ~recursive:false
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
  | Unbound_type_var name ->
      let quot = match name with "_" -> "" | _ -> "'" in
      fprintf ppf "@[<hov>Unbound type parameter@ %s%s.@]" quot name
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
  | Opaque_type_in_prover_mode typ ->
      fprintf ppf
        "@[<hov>The type @[<h>%a@] is not valid in this mode:@ opaque types \
         cannot be created in Prover mode.@]"
        pp_typ typ
  | Convertible_arities_differ (name, len, conv_name, conv_len) ->
      fprintf ppf
        "@[<hov>Cannot associate type %s of arity %i@ with type %s of arity \
         %i:@ their arities must be equal.@]"
        name len conv_name conv_len
  | GADT_in_nonrec_type ->
      fprintf ppf
        "@[<hov>GADT case syntax cannot be used in a non-recursive type.@ To \
         use this syntax, use 'type rec' for this type definition.@]@."
  | Repeated_row_label label ->
      fprintf ppf
        "@[<hov>The constructor %a has already appeared in this row.@]@."
        Ident.pprint label
  | Missing_row_label label ->
      fprintf ppf "@[<hov>The constructor %a is not present in this row.@]@."
        Ident.pprint label
  | Expected_row_type typ ->
      fprintf ppf "@[<hov>The type %a was expected to be a row.@]@." pp_typ typ

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
    | _ ->
        None )
