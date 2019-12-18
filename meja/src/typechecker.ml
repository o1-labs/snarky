open Compiler_internals
open Core_kernel
open Ast_types
open Parsetypes
open Type0
open Type1

type error =
  | Check_failed of type_expr * type_expr * error
  | Cannot_unify of type_expr * type_expr
  | Recursive_variable of type_expr
  | Unbound of string * lid
  | Unbound_value of str
  | Repeated_pattern_variable of string
  | Variable_on_one_side of string
  | Pattern_declaration of string * string
  | Empty_record
  | Wrong_record_field of Longident.t * type_expr
  | Repeated_field of Ident.t
  | Missing_fields of Ident.t list
  | Wrong_type_description of string * str
  | Unifiable_expr
  | No_unifiable_expr
  | No_instance of type_expr
  | Argument_expected of Longident.t
  | Not_extensible of Longident.t
  | Extension_different_arity of Longident.t
  | Convert_failed of type_expr * error
  | Cannot_create_conversion of type_expr
  | Convertible_not_in_checked
  | Type_modes_mismatch of type_expr * type_expr
  | Missing_row_constructor of Path.t * type_expr * type_expr
  | Empty_resulting_row of type_expr * type_expr
  | Row_different_arity of Path.t * type_expr * type_expr

exception Error of Location.t * Envi.t * error

let map_none x f = match x with Some x -> x | None -> f ()

let bind_none x f = match x with Some x -> Some x | None -> f ()

let unpack_decls ~loc typ ctyp env =
  if Int.equal typ.type_id ctyp.type_id then Some (typ, ctyp)
  else
    let unfold_typ () =
      Option.map (Envi.TypeDecl.unfold_alias ~loc typ env) ~f:(fun typ ->
          (typ, ctyp) )
    in
    let unfold_ctyp () =
      Option.map (Envi.TypeDecl.unfold_alias ~loc ctyp env) ~f:(fun ctyp ->
          (typ, ctyp) )
    in
    match (typ.type_desc, ctyp.type_desc) with
    | Tctor variant, Tctor cvariant ->
        let decl_id =
          (snd (Envi.raw_get_type_declaration ~loc variant.var_ident env))
            .tdec_id
        in
        let cdecl_id =
          (snd (Envi.raw_get_type_declaration ~loc cvariant.var_ident env))
            .tdec_id
        in
        (* Try to unfold the oldest type definition first. *)
        if decl_id < cdecl_id then bind_none (unfold_ctyp ()) unfold_typ
        else bind_none (unfold_typ ()) unfold_ctyp
    | Tctor _, _ ->
        unfold_typ ()
    | _, Tctor _ ->
        unfold_ctyp ()
    | _ ->
        None

let rec check_type_aux ~loc typ ctyp env =
  if Type1.contains typ ~in_:ctyp then
    raise (Error (loc, env, Recursive_variable typ)) ;
  if Type1.contains ctyp ~in_:typ then
    raise (Error (loc, env, Recursive_variable ctyp)) ;
  let check_type_aux = check_type_aux ~loc in
  Type1.unify_depths typ ctyp ;
  (* Unfold any [Tother_mode] types so that [typ] and [ctyp] have the same
     mode, if possible.
  *)
  let typ, ctyp = Type1.get_same_mode typ ctyp in
  (* Unpack checked mode [Tprover]s when unifying with a prover mode type,
     raising an exception if there is any other kind of mismatch.
  *)
  if not (equal_mode typ.type_mode ctyp.type_mode) then
    raise (Error (loc, env, Type_modes_mismatch (typ, ctyp))) ;
  (* Reject tri-stitchings with bad modes. *)
  assert (equal_mode typ.type_mode typ.type_alternate.type_alternate.type_mode) ;
  assert (
    equal_mode ctyp.type_mode ctyp.type_alternate.type_alternate.type_mode ) ;
  (* If the type stitchings differ, lower the tri-stitched one to a stitched
     one.
  *)
  let typ, ctyp =
    if
      phys_equal typ.type_alternate.type_alternate ctyp
      || phys_equal ctyp.type_alternate.type_alternate typ
    then
      (* Recursion breaking: these types differ in stitching, but are
         tri-stitched together.
      *)
      (typ, ctyp)
    else
      try
        match
          ( phys_equal typ typ.type_alternate.type_alternate
          , phys_equal ctyp ctyp.type_alternate.type_alternate )
        with
        | true, false ->
            stitch_tri_stitched ~loc ctyp env ;
            (typ, repr ctyp)
        | false, true ->
            stitch_tri_stitched ~loc typ env ;
            (repr typ, ctyp)
        | _ ->
            (typ, ctyp)
      with Error (_, _, Cannot_unify _) ->
        (* The stitching error tells us that the type will not be compatible,
           but the incompatibility between the stitched types is not useful to
           the user. Instead, throw an error about the types that triggered the
           unification.
        *)
        raise (Error (loc, env, Cannot_unify (typ, ctyp)))
  in
  match (typ.type_desc, ctyp.type_desc) with
  | Tref _, _ | _, Tref _ ->
      assert false
  | Treplace _, _ | _, Treplace _ ->
      assert false
  | _, _ when Int.equal typ.type_id ctyp.type_id ->
      ()
  | Tpoly _, _ | _, Tpoly _ ->
      (* We don't (yet) have a unification algorithm for [Tpoly], and unifying
         naively is clearly wrong: we don't want to instantiate the variables
         that [Tpoly] is over, otherwise we fundamentally change the meaning of
         the type!
      *)
      assert false
  | Tvar _, Tvar _ ->
      (* Add the outermost (in terms of lexical scope) of the variables as
         the instance for the other. We do this by chosing the type of lowest
         ID, to ensure strict ordering and thus no cycles.

         If the types are related by a tri-stitching, we instead collapse it.
      *)
      if phys_equal typ.type_alternate.type_alternate ctyp then
        stitch_tri_stitched ~loc typ env
      else if phys_equal ctyp.type_alternate.type_alternate typ then
        stitch_tri_stitched ~loc ctyp env
      else if ctyp.type_id < typ.type_id then
        Type1.add_instance
          ~unify:(fun typ1 typ2 -> check_type_aux typ1 typ2 env)
          typ ctyp
      else
        Type1.add_instance
          ~unify:(fun typ1 typ2 -> check_type_aux typ1 typ2 env)
          ctyp typ
  | Tvar _, _ ->
      Type1.add_instance
        ~unify:(fun typ1 typ2 -> check_type_aux typ1 typ2 env)
        typ ctyp
  | _, Tvar _ ->
      Type1.add_instance
        ~unify:(fun typ1 typ2 -> check_type_aux typ1 typ2 env)
        ctyp typ
  | Ttuple typs, Ttuple ctyps -> (
    match
      List.iter2 typs ctyps ~f:(fun typ ctyp -> check_type_aux typ ctyp env)
    with
    | Ok () ->
        ()
    | Unequal_lengths ->
        raise (Error (loc, env, Cannot_unify (typ, ctyp))) )
  | ( Tarrow (typ1, typ2, Explicit, label1)
    , Tarrow (ctyp1, ctyp2, Explicit, label2) )
  | ( Tarrow (typ1, typ2, Implicit, label1)
    , Tarrow (ctyp1, ctyp2, Implicit, label2) ) -> (
    match (label1, label2) with
    | Nolabel, Nolabel ->
        check_type_aux typ1 ctyp1 env ;
        check_type_aux typ2 ctyp2 env
    | Labelled x, Labelled y when String.equal x y ->
        check_type_aux typ1 ctyp1 env ;
        check_type_aux typ2 ctyp2 env
    | Optional x, Optional y when String.equal x y ->
        check_type_aux typ1 ctyp1 env ;
        check_type_aux typ2 ctyp2 env
    | Labelled x, Optional y when String.equal x y ->
        check_type_aux (Initial_env.Type.option typ1) ctyp1 env ;
        check_type_aux typ2 ctyp2 env
    | Optional x, Labelled y when String.equal x y ->
        check_type_aux typ1 (Initial_env.Type.option ctyp1) env ;
        check_type_aux typ2 ctyp2 env
    | _ ->
        raise (Error (loc, env, Cannot_unify (typ, ctyp))) )
  | Tctor variant, Tctor constr_variant -> (
    (* Always try to unfold first, so that type aliases with phantom
         parameters can unify, as in OCaml.
      *)
    match unpack_decls ~loc typ ctyp env with
    | Some (typ, ctyp) ->
        check_type_aux typ ctyp env
    | None ->
        let decl_id =
          (snd (Envi.raw_get_type_declaration ~loc variant.var_ident env))
            .tdec_id
        in
        let cdecl_id =
          (snd
             (Envi.raw_get_type_declaration ~loc constr_variant.var_ident env))
            .tdec_id
        in
        if Int.equal decl_id cdecl_id then
          match
            List.iter2 variant.var_params constr_variant.var_params
              ~f:(fun param constr_param ->
                check_type_aux param constr_param env )
          with
          | Ok env ->
              env
          | Unequal_lengths ->
              raise (Error (loc, env, Cannot_unify (typ, ctyp)))
        else raise (Error (loc, env, Cannot_unify (typ, ctyp))) )
  | Topaque typ, Topaque ctyp ->
      check_type_aux typ ctyp env
  | Topaque typ', _ when typ.type_mode = Prover ->
      (* Viral opacity, make sure that if one type is opaque then they both
         are.
      *)
      let ctyp' =
        (* New type with identical contents to [typ]. *)
        Mk.stitch ~mode:ctyp.type_mode ctyp.type_depth ctyp.type_desc
          ctyp.type_alternate.type_desc
      in
      set_desc ctyp (Topaque ctyp') ;
      set_desc ctyp.type_alternate (Topaque ctyp') ;
      check_type_aux typ' ctyp' env
  | _, Topaque ctyp' when ctyp.type_mode = Prover ->
      (* Viral opacity, make sure that if one type is opaque then they both
         are.
      *)
      let typ' =
        (* New type with identical contents to [typ]. *)
        Mk.stitch ~mode:typ.type_mode typ.type_depth typ.type_desc
          typ.type_alternate.type_desc
      in
      set_desc typ (Topaque typ') ;
      set_desc typ.type_alternate (Topaque typ') ;
      check_type_aux typ' ctyp' env
  | Tother_mode typ, Tother_mode ctyp ->
      check_type_aux typ ctyp env
  | Tctor _, _ | _, Tctor _ ->
      (* Unfold an alias and compare again *)
      let typ, ctyp =
        match unpack_decls ~loc typ ctyp env with
        | Some (typ, ctyp) ->
            (typ, ctyp)
        | None ->
            raise (Error (loc, env, Cannot_unify (typ, ctyp)))
      in
      check_type_aux typ ctyp env
  | Tconv typ, Tconv ctyp ->
      check_type_aux typ ctyp env ;
      check_type_aux typ.type_alternate ctyp.type_alternate env
  | Trow row1, Trow row2 ->
      let row_tags1, subtract_tags1, row_rest1, row_closed1 = row_repr row1 in
      let row_tags2, subtract_tags2, row_rest2, row_closed2 = row_repr row2 in
      let subtract_tags = subtract_tags1 @ subtract_tags2 in
      let is_empty = ref true in
      let row_extra1 = ref Ident.Map.empty in
      let row_extra2 = ref Ident.Map.empty in
      let row_presence_proxy = mk_rp RpPresent in
      set_rp_desc row1.row_presence_proxy (RpRef row_presence_proxy) ;
      set_rp_desc row2.row_presence_proxy (RpRef row_presence_proxy) ;
      let row_tags =
        Map.merge row_tags1 row_tags2 ~f:(fun ~key data ->
            match (data, row_closed1, row_extra1, row_closed2, row_extra2) with
            | `Left (path, pres, args), _, _, row_closed, row_extra
            | `Right (path, pres, args), row_closed, row_extra, _, _ ->
                let pres = rp_repr pres in
                ( match (pres.rp_desc, row_closed) with
                | (RpRef _ | RpReplace _), _ ->
                    assert false
                | RpPresent, Closed ->
                    raise
                      (Error
                         (loc, env, Missing_row_constructor (path, typ, ctyp)))
                | RpMaybe, Closed ->
                    set_rp_desc pres RpAbsent
                | (RpPresent | RpMaybe), Open ->
                    is_empty := false
                | RpAbsent, _ ->
                    () ) ;
                let data = (path, pres, args) in
                row_extra := Map.set !row_extra ~key ~data ;
                Some data
            | `Both ((path1, pres1, args1), (path2, pres2, args2)), _, _, _, _
              ->
                let pres1 = rp_repr pres1 in
                let pres2 = rp_repr pres2 in
                let pres_desc =
                  match (pres1.rp_desc, pres2.rp_desc) with
                  | (RpRef _ | RpReplace _), _ | _, (RpRef _ | RpReplace _) ->
                      assert false
                  | RpMaybe, RpMaybe ->
                      is_empty := false ;
                      RpMaybe
                  | (RpPresent | RpMaybe), (RpPresent | RpMaybe) ->
                      is_empty := false ;
                      RpPresent
                  | (RpAbsent | RpMaybe), (RpAbsent | RpMaybe) ->
                      RpAbsent
                  | RpPresent, RpAbsent ->
                      raise
                        (Error
                           ( loc
                           , env
                           , Missing_row_constructor (path1, typ, ctyp) ))
                  | RpAbsent, RpPresent ->
                      raise
                        (Error
                           ( loc
                           , env
                           , Missing_row_constructor (path1, typ, ctyp) ))
                in
                let pres = mk_rp pres_desc in
                set_rp_desc pres1 (RpRef pres) ;
                set_rp_desc pres2 (RpRef pres) ;
                ( match
                    List.iter2 args1 args2 ~f:(fun typ ctyp ->
                        check_type_aux typ ctyp env )
                  with
                | Ok () ->
                    ()
                | Unequal_lengths ->
                    raise
                      (Error (loc, env, Row_different_arity (path1, typ, ctyp)))
                ) ;
                (* TODO: Decide on how to choose between paths. *)
                ignore path2 ;
                Some (path1, pres, args1) )
      in
      let row_closed =
        match (row_closed1, row_closed2) with
        | Open, Open ->
            Open
        | _ ->
            if !is_empty then
              raise (Error (loc, env, Empty_resulting_row (typ, ctyp))) ;
            Closed
      in
      let row_rest =
        let row_rest = Type1.Mk.var ~mode:typ.type_mode typ.type_depth None in
        (* Tweak errors so that they talk about the row types instead of just
           their respective [row_rest] parameters.
        *)
        let rescope_rest_error exn =
          match exn with
          | Error (loc, env, Cannot_unify _) ->
              Error (loc, env, Cannot_unify (typ, ctyp))
          | Error (loc, env, Recursive_variable recvar)
            when phys_equal recvar row_rest1 ->
              Error (loc, env, Cannot_unify (typ, ctyp))
          | Error (loc, env, Recursive_variable recvar)
            when phys_equal recvar row_rest2 ->
              Error (loc, env, Cannot_unify (typ, ctyp))
          | Error (loc, env, Recursive_variable recvar)
            when phys_equal recvar row_rest ->
              Error (loc, env, Cannot_unify (typ, ctyp))
          | _ ->
              exn
        in
        let expand_row row_rest1 row_extra =
          match row_rest1.type_desc with
          | Tvar _ when Map.is_empty row_extra ->
              row_rest1
          | Tvar _ | Tref _ ->
              let new_row =
                Type1.Mk.row ~mode:row_rest1.type_mode row_rest1.type_depth
                  { row_tags= row_extra
                  ; row_closed
                  ; row_rest
                  ; row_presence_proxy }
              in
              let new_row =
                if phys_equal row_rest1.type_alternate.type_alternate row_rest1
                then new_row.type_alternate.type_alternate
                else new_row
              in
              ( match row_rest1.type_desc with
              | Tvar _ ->
                  choose_variable_name row_rest1 row_rest ;
                  add_instance
                    ~unify:(fun typ1 typ2 -> check_type_aux typ1 typ2 env)
                    row_rest1 new_row
              | Tref _ -> (
                try
                  (* This type could not have been a reference at definition
                   time, so it must be equal to the other [row_rest], or must
                   contain itself.
                *)
                  check_type_aux row_rest1 new_row env
                with exn ->
                  (* Return a type error that will make sense to the user. *)
                  raise (rescope_rest_error exn) )
              | _ ->
                  assert false ) ;
              row_rest
          | _ ->
              assert false
        in
        let row_rest1 = expand_row row_rest1 !row_extra1 in
        let row_rest2 = expand_row row_rest2 !row_extra2 in
        ( try check_type_aux row_rest1 row_rest2 env
          with exn ->
            (* Return a type error that will make sense to the user. *)
            raise (rescope_rest_error exn) ) ;
        let row_rest = repr row_rest in
        let ret =
          if List.is_empty subtract_tags then row_rest
          else
            let row_rest' =
              Type1.Mk.var ~mode:row_rest.type_mode row_rest.type_depth None
            in
            choose_variable_name row_rest' row_rest ;
            let ret =
              if
                equal_mode row_rest.type_mode Checked
                && phys_equal row_rest.type_alternate.type_alternate row_rest
              then
                (Type1.Mk.row_subtract ~mode:Prover row_rest.type_depth
                   row_rest'.type_alternate subtract_tags)
                  .type_alternate
              else
                Type1.Mk.row_subtract ~mode:row_rest.type_mode
                  row_rest.type_depth row_rest' subtract_tags
            in
            add_instance
              ~unify:(fun typ1 typ2 -> check_type_aux typ1 typ2 env)
              row_rest ret ;
            ret
        in
        ret
      in
      let new_row = {row_tags; row_closed; row_rest; row_presence_proxy} in
      let alt_row = Type1.row_alternate new_row in
      let res_type =
        if ctyp.type_id < typ.type_id then ( set_repr typ ctyp ; ctyp )
        else ( set_repr ctyp typ ; typ )
      in
      set_desc res_type (Trow new_row) ;
      set_desc res_type.type_alternate (Trow alt_row) ;
      if not (phys_equal typ.type_alternate.type_alternate typ) then
        set_desc res_type.type_alternate.type_alternate
          (Trow (Type1.row_alternate alt_row))
  | _, _ ->
      raise (Error (loc, env, Cannot_unify (typ, ctyp)))

and stitch_tri_stitched ~loc typ env =
  ( match typ.type_desc with
  | Tvar _ ->
      (* Ensure that we properly lift any user-provided type variable names
         before erasing the description of [typ].
      *)
      Type1.choose_variable_name typ typ.type_alternate.type_alternate
  | _ ->
      (* Check that [typ] is properly compatible with its stitched conterpart
         before erasing its description.
      *)
      check_type_aux ~loc typ typ.type_alternate.type_alternate env ) ;
  (* Set the representative of the tri-stitched type to the corresponding
     stitched type.
  *)
  Type1.set_desc typ (Tref typ.type_alternate.type_alternate)

let check_type ~loc env typ constr_typ =
  Format.eprintf "%a@.%a@.%a@." Location.print loc Typeprint.type_expr typ
    Typeprint.type_expr constr_typ ;
  let snapshot = Snapshot.create () in
  match check_type_aux ~loc typ constr_typ env with
  | exception Error (_, _, err) ->
      let typ = Type1.flatten typ in
      let constr_typ = Type1.flatten constr_typ in
      (* Backtrack to restore types to their pre-unification states. *)
      backtrack snapshot ;
      (*Format.eprintf "%s@." (Printexc.get_backtrace ()) ;*)
      raise (Error (loc, env, Check_failed (typ, constr_typ, err)))
  (*| exception err ->
      Format.(
        fprintf err_formatter "checking:%a@.%a@." typ_debug_print typ
          typ_debug_print constr_typ) ;
      raise err*)
  | () ->
      Format.eprintf "%a@.%a@.%a@." Location.print loc Typeprint.type_expr typ
        Typeprint.type_expr constr_typ ;
      ()

let () = Typet.unify := check_type

let unifies env typ constr_typ =
  match check_type ~loc:Location.none env typ constr_typ with
  | () ->
      true
  | exception (Error _ as _exn) ->
      (*Format.(
        fprintf err_formatter "Does not unify:@.%a@." Location.report_exception
          exn) ;*)
      false

let rec add_implicits ~loc implicits typ env =
  match implicits with
  | [] ->
      typ
  | typ' :: implicits ->
      let typ = add_implicits ~loc implicits typ env in
      Envi.Type.Mk.arrow ~mode:typ.type_mode ~explicit:Implicit typ' typ env

let free_type_vars ?depth typ =
  let empty = Typeset.empty in
  let rec free_type_vars set typ =
    match typ.type_desc with
    | Tpoly (vars, typ) ->
        let poly_vars =
          Typeset.union_list (List.map ~f:(Type1.type_vars ?depth) vars)
        in
        Set.union set (Set.diff (free_type_vars empty typ) poly_vars)
    | Tarrow (typ1, typ2, _, _) ->
        Set.union (Type1.type_vars ?depth typ1) (Type1.type_vars ?depth typ2)
    | _ ->
        fold ~init:set typ ~f:free_type_vars
  in
  free_type_vars empty typ

let polymorphise typ env =
  let typ_vars = Set.to_list (free_type_vars ~depth:env.Envi.depth typ) in
  match typ_vars with
  | [] ->
      typ
  | _ ->
      (* TODO: Capture type variables for each mode seperately. *)
      Envi.Type.Mk.poly ~mode:typ.type_mode typ_vars typ env

let add_polymorphised name typ env =
  let typ = Type1.flatten typ in
  let typ = polymorphise typ env in
  Envi.add_name name typ env

let get_field (field : lid) env =
  let mode = Envi.current_mode env in
  let loc = field.loc in
  match Envi.TypeDecl.find_of_field ~mode field env with
  | Some
      (ident, ({tdec_desc= TRecord field_decls; tdec_ret; tdec_params; _}, i))
    ->
      let snap = Snapshot.create () in
      ignore (Envi.Type.refresh_vars tdec_params env) ;
      let {fld_type; _} = List.nth_exn field_decls i in
      let rcd_type = Envi.Type.copy tdec_ret env in
      let fld_type = Type1.get_mode mode fld_type in
      let fld_type = Envi.Type.copy fld_type env in
      backtrack snap ;
      (ident, i, fld_type, rcd_type)
  | _ ->
      raise (Error (loc, env, Unbound ("record field", field)))

let get_field_of_decl typ decl_vars params field_decls (field : lid) env =
  match field with
  | {txt= Longident.Lident name; _} -> (
    match
      List.findi field_decls ~f:(fun _ {fld_ident; _} ->
          String.equal (Ident.name fld_ident) name )
    with
    | Some (i, {fld_type; fld_ident; _}) ->
        let mode = Envi.current_mode env in
        let fld_type = Type1.get_mode mode fld_type in
        let fld_type = Envi.Type.instantiate decl_vars params fld_type env in
        (Path.Pident fld_ident, i, fld_type, typ)
    | None ->
        get_field field env )
  | _ ->
      get_field field env

let get_ctor (name : lid) env =
  let mode = Envi.current_mode env in
  let loc = name.loc in
  let name, (decl, index) =
    match Envi.TypeDecl.find_of_constructor ~mode name env with
    | Some x ->
        x
    | None ->
        raise (Error (loc, env, Unbound ("constructor", name)))
  in
  let ctors =
    match decl.tdec_desc with
    | TVariant ctors ->
        ctors
    | TExtend (_, ctors) ->
        ctors
    | _ ->
        assert false
  in
  let ctor = List.nth_exn ctors index in
  let make_name name tdec_ident =
    match name with
    | Path.Pdot (m, _, _) ->
        Path.dot m tdec_ident
    | _ ->
        Path.Pident tdec_ident
  in
  let typ = match ctor.ctor_ret with Some typ -> typ | _ -> decl.tdec_ret in
  let typ = get_mode mode typ in
  let args_typ =
    match ctor.ctor_args with
    | Ctor_record decl ->
        Envi.Type.Mk.ctor ~mode
          (make_name name ctor.ctor_ident)
          decl.tdec_params env
    | Ctor_tuple [typ] ->
        get_mode mode typ
    | Ctor_tuple typs ->
        Envi.Type.Mk.tuple ~mode typs env
  in
  let bound_vars =
    Set.to_list (Set.union (Type1.type_vars typ) (Type1.type_vars args_typ))
  in
  let snap = Snapshot.create () in
  ignore (Envi.Type.refresh_vars bound_vars env) ;
  let args_typ = Envi.Type.copy args_typ env in
  let typ = Envi.Type.copy typ env in
  backtrack snap ; (name, typ, args_typ)

let rec check_pattern env typ pat =
  let mode = Envi.current_mode env in
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | Ppat_any ->
      ({Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_any}, [], env)
  | Ppat_variable str ->
      let name = map_loc ~f:(Ident.create ~mode) str in
      ( {Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_variable name}
      , [(name, typ)]
      , env )
  | Ppat_constraint (p, constr_typ) ->
      let ctyp, env = Typet.Type.import constr_typ env in
      check_type ~loc env typ ctyp.type_type ;
      let p, names, env = check_pattern env ctyp.type_type p in
      ( { Typedast.pat_loc= loc
        ; pat_type= typ
        ; pat_desc= Tpat_constraint (p, ctyp) }
      , names
      , env )
  | Ppat_tuple ps ->
      let vars = List.map ps ~f:(fun _ -> Envi.Type.mkvar ~mode None env) in
      let tuple_typ = Envi.Type.Mk.tuple ~mode vars env in
      check_type ~loc env typ tuple_typ ;
      let ps, names, env = check_patterns env vars ps in
      ( {Typedast.pat_loc= loc; pat_type= tuple_typ; pat_desc= Tpat_tuple ps}
      , names
      , env )
  | Ppat_or (p1, p2) ->
      let p1, names1, env = check_pattern env typ p1 in
      let p2, names2, env = check_pattern env typ p2 in
      let () =
        (* Check that the assignments in each scope match. *)
        let names_map1 =
          String.Map.of_alist_exn
            (List.map names1 ~f:(fun (name, typ) ->
                 (Ident.name name.Location.txt, typ) ))
        in
        let names_map2 =
          String.Map.of_alist_exn
            (List.map names2 ~f:(fun (name, typ) ->
                 (Ident.name name.Location.txt, typ) ))
        in
        Map.iter2 names_map1 names_map2 ~f:(fun ~key ~data ->
            match data with
            | `Both (typ1, typ2) ->
                check_type ~loc env typ1 typ2
            | _ ->
                raise (Error (loc, env, Variable_on_one_side key)) )
      in
      ( {Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_or (p1, p2)}
      , names1
      , env )
  | Ppat_int i ->
      check_type ~loc env typ (get_mode mode Initial_env.Type.int) ;
      ({Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_int i}, [], env)
  | Ppat_record [] ->
      raise (Error (loc, env, Empty_record))
  | Ppat_record ((field, _) :: _ as fields) ->
      let typ, field_decls, decl_vars, type_params =
        match Envi.TypeDecl.find_unaliased_of_type ~loc typ env with
        | Some
            ( {tdec_desc= TRecord field_decls; tdec_params; _}
            , ({type_desc= Tctor {var_params; _}; _} as typ) ) ->
            (typ, field_decls, tdec_params, var_params)
        | _ -> (
          match Envi.TypeDecl.find_of_field ~mode field env with
          | Some
              ( _fld_ident
              , ({tdec_desc= TRecord field_decls; tdec_params; tdec_ret; _}, _)
              ) ->
              let vars =
                List.map
                  ~f:(fun _ -> Envi.Type.mkvar ~mode None env)
                  tdec_params
              in
              let decl_type =
                Envi.Type.instantiate tdec_params vars tdec_ret env
              in
              check_type ~loc env typ decl_type ;
              (decl_type, field_decls, tdec_params, vars)
          | _ ->
              raise (Error (loc, env, Unbound ("record field", field))) )
      in
      let field_infos =
        List.map fields ~f:(fun (field, _p) ->
            let path, _index, field_typ, record_typ =
              get_field_of_decl typ decl_vars type_params field_decls field env
            in
            ( try check_type ~loc:field.loc env record_typ typ
              with
              | Error (_, env, Check_failed (_, _, Cannot_unify (typ, _))) ->
                raise
                  (Error (field.loc, env, Wrong_record_field (field.txt, typ)))
            ) ;
            (field_typ, Location.mkloc path field.loc) )
      in
      let ps, names, env =
        check_patterns env
          (List.map ~f:fst field_infos)
          (List.map ~f:snd fields)
      in
      let fields =
        List.map2_exn field_infos ps ~f:(fun (_, field) p -> (field, p))
      in
      ( {Typedast.pat_loc= loc; pat_type= typ; pat_desc= Tpat_record fields}
      , names
      , env )
  | Ppat_ctor (name, arg) ->
      let name', typ', args_typ = get_ctor name env in
      let name = Location.mkloc name' name.loc in
      check_type ~loc env typ typ' ;
      let arg, names, env =
        match arg with
        | Some arg ->
            let arg, names, env = check_pattern env args_typ arg in
            (Some arg, names, env)
        | None ->
            let typ = Envi.Type.Mk.tuple ~mode [] env in
            check_type ~loc env args_typ typ ;
            (None, [], env)
      in
      ( {Typedast.pat_loc= loc; pat_type= typ'; pat_desc= Tpat_ctor (name, arg)}
      , names
      , env )
  | Ppat_row_ctor (name, args) ->
      let name = map_loc ~f:Ident.create_row name in
      let arg_types =
        List.map args ~f:(fun _ -> Envi.Type.Mk.var ~mode None env)
      in
      let typ' = Envi.Type.Mk.row_of_ctor ~mode name.txt arg_types env in
      check_type ~loc env typ typ' ;
      let args, names, env = check_patterns env arg_types args in
      ( { Typedast.pat_loc= loc
        ; pat_type= typ
        ; pat_desc= Tpat_row_ctor (name, args) }
      , names
      , env )

and check_patterns env typs pats =
  let names_table = String.Table.create () in
  let rev_pats, rev_names, env =
    List.fold2_exn ~init:([], [], env) typs pats
      ~f:(fun (rev_pats, rev_names, env) typ pat ->
        let pat, names, env = check_pattern env typ pat in
        (* Check that a variable name hasn't been multiply assigned. *)
        List.iter names ~f:(fun ({loc; txt= name}, _typ) ->
            let name = Ident.name name in
            String.Table.update names_table name ~f:(function
              | Some _ ->
                  raise (Error (loc, env, Repeated_pattern_variable name))
              | None ->
                  () ) ) ;
        (pat :: rev_pats, List.rev_append names rev_names, env) )
  in
  (List.rev rev_pats, List.rev rev_names, env)

let rec get_conversion_body ~may_identity ~can_add_args ~loc env free_vars typ
    =
  let get_conversion_body =
    get_conversion_body ~may_identity ~can_add_args ~loc env
  in
  let get_conversion_bodies =
    get_conversion_bodies ~may_identity ~can_add_args ~loc env
  in
  let typ = repr typ in
  (* Sanity check. *)
  assert (are_stitched typ typ.type_alternate) ;
  let mode = Envi.current_mode env in
  let conv_body_type = Envi.Type.Mk.conv ~mode typ typ.type_alternate env in
  let found_conversion =
    match (typ.type_desc, typ.type_alternate.type_desc) with
    | Tvar _, Tvar _ ->
        (* This search will never succeed, avoid doing it. *)
        None
    | _ ->
        Envi.find_conversion ~unifies typ env
  in
  match found_conversion with
  | Some (path, conv_args) ->
      let labels, args = List.unzip conv_args in
      let free_vars, args = get_conversion_bodies free_vars args in
      let conv_args = List.zip_exn labels args in
      ( free_vars
      , { Typedast.conv_body_desc=
            Tconv_ctor (Location.mkloc path loc, conv_args)
        ; conv_body_loc= loc
        ; conv_body_type } )
  | None -> (
    match (typ.type_desc, typ.type_alternate.type_desc) with
    | Tconv typ, Tconv alt when phys_equal typ alt ->
        get_conversion_body free_vars typ
    | Ttuple typs, Ttuple alts ->
        (* Sanity check: The types within stitched tuples should always be
             stitched.
          *)
        ( match
            List.iter2 typs alts ~f:(fun typ1 typ2 ->
                assert (Type1.are_stitched typ1 typ2) )
          with
        | Unequal_lengths ->
            assert false
        | Ok () ->
            () ) ;
        let free_vars, convs = get_conversion_bodies free_vars typs in
        if
          may_identity
          && List.for_all convs ~f:(function
               | {conv_body_desc= Tconv_identity; _} ->
                   true
               | _ ->
                   false )
        then
          ( free_vars
          , {conv_body_desc= Tconv_identity; conv_body_loc= loc; conv_body_type}
          )
        else
          ( free_vars
          , { conv_body_desc= Tconv_tuple convs
            ; conv_body_loc= loc
            ; conv_body_type } )
    | Tvar _, _ ->
        let free_vars, ident =
          match List.Assoc.find ~equal:Type1.equal free_vars typ with
          | Some ident ->
              (free_vars, ident)
          | None when can_add_args ->
              (* TODO: Better unique identifiers. *)
              let ident = Ident.fresh mode in
              ((typ, ident) :: free_vars, ident)
          | None ->
              raise (Error (loc, env, Cannot_create_conversion typ))
        in
        ( free_vars
        , { conv_body_desc=
              Tconv_ctor (Location.mkloc (Path.Pident ident) loc, [])
          ; conv_body_loc= loc
          ; conv_body_type } )
    | Tctor variant1, Tctor variant2 -> (
        if
          may_identity
          && Type1.equal_at_depth
               ~get_decl:(fun path ->
                 snd (Envi.raw_get_type_declaration ~loc path env) )
               ~depth:10001 typ typ.type_alternate
        then
          ( free_vars
          , {conv_body_desc= Tconv_identity; conv_body_loc= loc; conv_body_type}
          )
        else
          match Envi.TypeDecl.unfold_alias_aux ~loc typ env with
          | Some (_desc, Some typ') ->
              (* Type declaration is an alias. *)
              if unifies env typ.type_alternate typ'.type_alternate then
                get_conversion_body free_vars typ'
              else raise (Error (loc, env, Cannot_create_conversion typ))
          | Some ({tdec_desc= TRecord fields1; tdec_params= params1; _}, None)
            -> (
            match
              Envi.TypeDecl.unfold_alias_aux ~loc typ.type_alternate env
            with
            | Some ({tdec_desc= TRecord fields2; tdec_params= params2; _}, None)
              ->
                let free_vars = ref free_vars in
                let conv_field {fld_ident= ident1; fld_type= typ1}
                    {fld_ident= ident2; fld_type= typ2} =
                  if not (String.equal (Ident.name ident1) (Ident.name ident2))
                  then raise (Error (loc, env, Cannot_create_conversion typ)) ;
                  let typ1 =
                    Envi.Type.instantiate params1 variant1.var_params typ1 env
                  in
                  let typ2 =
                    Envi.Type.instantiate params2 variant2.var_params typ2 env
                  in
                  if not (unifies env typ1.type_alternate typ2) then
                    (* The types of the fields don't match. *)
                    raise (Error (loc, env, Cannot_create_conversion typ)) ;
                  let free_vars', conv = get_conversion_body !free_vars typ1 in
                  free_vars := free_vars' ;
                  (Location.mkloc (Path.Pident ident1) loc, conv)
                in
                let fields =
                  match List.map2 ~f:conv_field fields1 fields2 with
                  | Ok x ->
                      x
                  | Unequal_lengths ->
                      raise (Error (loc, env, Cannot_create_conversion typ))
                in
                if
                  may_identity
                  && List.for_all fields ~f:(function
                       | _, {conv_body_desc= Tconv_identity; _} ->
                           true
                       | _ ->
                           false )
                then
                  ( !free_vars
                  , { conv_body_desc= Tconv_identity
                    ; conv_body_loc= loc
                    ; conv_body_type } )
                else
                  ( !free_vars
                  , { conv_body_desc= Tconv_record fields
                    ; conv_body_loc= loc
                    ; conv_body_type } )
            | _ ->
                assert false )
          | _ ->
              raise (Error (loc, env, Cannot_create_conversion typ)) )
    | ( Tarrow (typ1a, typ1b, Explicit, Nolabel)
      , Tarrow (typ2a, typ2b, Explicit, Nolabel) ) -> (
        if
          not
            ( unifies env typ1a.type_alternate typ2a
            && unifies env typ1b.type_alternate typ2b )
        then
          (* The types of the arguments don't match. *)
          raise (Error (loc, env, Cannot_create_conversion typ)) ;
        let free_vars, conv1 = get_conversion_body free_vars typ1a in
        let free_vars, conv2 = get_conversion_body free_vars typ1b in
        match (conv1.conv_body_desc, conv2.conv_body_desc) with
        | Tconv_identity, Tconv_identity when may_identity ->
            ( free_vars
            , { conv_body_desc= Tconv_identity
              ; conv_body_loc= loc
              ; conv_body_type } )
        | _ ->
            ( free_vars
            , { conv_body_desc= Tconv_arrow (conv1, conv2)
              ; conv_body_loc= loc
              ; conv_body_type } ) )
    | Topaque typ1, _ ->
        (* An opaque conversion. The underlying types should be the same. *)
        let typ1 = Type1.remove_opaques typ1 in
        let typ2 = Type1.remove_opaques typ.type_alternate in
        if
          phys_equal typ1 typ2
          || Type1.equal_at_depth
               ~get_decl:(fun path ->
                 snd (Envi.raw_get_type_declaration ~loc path env) )
               ~depth:10001 typ typ2
        then
          ( free_vars
          , {conv_body_desc= Tconv_opaque; conv_body_loc= loc; conv_body_type}
          )
        else raise (Error (loc, env, Cannot_create_conversion typ))
    | _, Topaque typ2 ->
        (* A tri-stitching where the stitching is an opaque conversion.
           The underlying types should be the same.
        *)
        let typ2 = Type1.remove_opaques typ2 in
        if
          may_identity
          && ( phys_equal typ typ2
             || Type1.equal_at_depth
                  ~get_decl:(fun path ->
                    snd (Envi.raw_get_type_declaration ~loc path env) )
                  ~depth:10001 typ typ2 )
        then
          ( free_vars
          , {conv_body_desc= Tconv_identity; conv_body_loc= loc; conv_body_type}
          )
        else raise (Error (loc, env, Cannot_create_conversion typ))
    | _ ->
        raise (Error (loc, env, Cannot_create_conversion typ)) )

and get_conversion_bodies ~may_identity ~can_add_args ~loc env free_vars typs =
  List.fold_map
    ~f:(get_conversion_body ~may_identity ~can_add_args ~loc env)
    ~init:free_vars typs

let get_conversion ~may_identity ~can_add_args ~loc env typ =
  let mode = Envi.current_mode env in
  let typ = Type1.flatten typ in
  let rev_arguments, typ = get_rev_implicits typ in
  let rev_arguments =
    List.map rev_arguments ~f:(fun (_, typ') ->
        match typ'.type_desc with
        | Tconv typ' ->
            (typ', Ident.fresh mode)
        | Tvar _ ->
            (* Free type variables. *)
            (typ', Ident.fresh mode)
        | _ ->
            raise (Error (loc, env, Cannot_create_conversion typ)) )
  in
  let typ = match typ.type_desc with Tconv typ -> typ | _ -> typ in
  match
    get_conversion_body ~may_identity ~can_add_args ~loc env rev_arguments typ
  with
  | free_vars, conv ->
      let conv =
        { Typedast.conv_desc= Tconv_body conv
        ; conv_loc= loc
        ; conv_type= conv.conv_body_type }
      in
      List.fold ~init:conv free_vars ~f:(fun conv (typ, ident) ->
          let typ = Envi.Type.Mk.conv ~mode typ typ.type_alternate env in
          let conv_type =
            Envi.Type.Mk.arrow ~mode ~explicit:Implicit typ conv.conv_type env
          in
          { conv_desc= Tconv_fun (Location.mkloc ident loc, conv)
          ; conv_loc= loc
          ; conv_type } )
  | exception (Error (_, _, err) as _exn) ->
      (*Format.eprintf "%s@." (Printexc.get_backtrace ()) ;
      Location.report_exception Format.err_formatter _exn ;*)
      let typ = Envi.Type.Mk.conv ~mode typ typ.type_alternate env in
      raise (Error (loc, env, Convert_failed (typ, err)))

let conversion_body_is_identity = function
  | {Typedast.conv_body_desc= Tconv_identity; _} ->
      true
  | _ ->
      false

let rec conversion_is_identity = function
  | {Typedast.conv_desc= Tconv_body conv; _} ->
      conversion_body_is_identity conv
  | {conv_desc= Tconv_fun (_, conv); _} ->
      conversion_is_identity conv

let rec get_expression env expected exp =
  let mode = Envi.current_mode env in
  assert (equal_mode expected.type_mode mode) ;
  let loc = exp.exp_loc in
  Format.eprintf "%a@.%a@." Location.print loc Typeprint.type_expr expected ;
  match exp.exp_desc with
  | Pexp_apply (f, es) ->
      let f_typ = Envi.Type.mkvar ~mode None env in
      let f, env = get_expression env f_typ f in
      let (typ, env), es =
        List.fold_map ~init:(f.Typedast.exp_type, env) es
          ~f:(fun (f_typ, env) (label, e) ->
            let f_typ = Type1.bubble_label label f_typ in
            let e_typ = Envi.Type.mkvar ~mode None env in
            let res_typ = Envi.Type.mkvar ~mode None env in
            let arrow = Envi.Type.Mk.arrow ~mode ~label e_typ res_typ env in
            check_type ~loc:e.exp_loc env f_typ arrow ;
            let e_typ =
              match label with
              | Optional _ ->
                  Initial_env.Type.option e_typ
              | _ ->
                  e_typ
            in
            let e, env = get_expression env e_typ e in
            ((Type1.flatten res_typ, env), (Explicit, label, e)) )
      in
      let typ = Type1.discard_optional_labels @@ Type1.flatten typ in
      (* Squash nested applies from implicit arguments. *)
      let f, es =
        match f.exp_desc with
        | Texp_apply (f', args) ->
            if
              List.for_all args ~f:(function
                | Implicit, _, _ ->
                    true
                | _ ->
                    false )
            then (f', args @ es)
            else (f, es)
        | _ ->
            (f, es)
      in
      check_type ~loc env expected typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_apply (f, es)}, env)
  | Pexp_variable name ->
      let path, typ = Envi.find_name ~mode name env in
      let path = Location.mkloc path name.loc in
      let implicits, result_typ = get_implicits typ in
      check_type ~loc env expected (get_mode mode result_typ) ;
      let implicits =
        List.map implicits ~f:(fun (label, typ) ->
            (Implicit, label, Envi.Type.new_implicit_var ~loc typ env) )
      in
      let e =
        {Typedast.exp_loc= loc; exp_type= typ; exp_desc= Texp_variable path}
      in
      let e =
        if List.is_empty implicits then e
        else
          { Typedast.exp_loc= loc
          ; exp_type= result_typ
          ; exp_desc= Texp_apply (e, implicits) }
      in
      let e =
        match (mode, result_typ.type_mode) with
        | Checked, Checked | Prover, Prover ->
            e
        | Prover, Checked ->
            let conv =
              get_conversion ~may_identity:true ~can_add_args:true ~loc env
                result_typ
            in
            if conversion_is_identity conv then {e with exp_type= expected}
            else
              let implicits =
                (* Instantiate unfilled implicit arguments in [conv]. *)
                let implicits, _ = get_implicits conv.conv_type in
                List.map implicits ~f:(fun (label, typ) ->
                    (label, Envi.Type.new_implicit_var ~loc typ env) )
              in
              { exp_loc= loc
              ; exp_type= expected
              ; exp_desc= Texp_read (conv, implicits, e) }
        | Checked, Prover ->
            assert false
      in
      (e, env)
  | Pexp_literal (Int i) ->
      let typ = Type1.get_mode mode Initial_env.Type.int in
      check_type ~loc env expected typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_literal (Int i)}, env)
  | Pexp_literal (Bool _b) ->
      failwith "Unhandled boolean literal"
  | Pexp_literal (Field _f) ->
      failwith "Unhandled field literal"
  | Pexp_literal (String s) ->
      let typ = get_mode mode Initial_env.Type.string in
      check_type ~loc env expected typ ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_literal (String s)}, env)
  | Pexp_fun (label, p, body, explicit) ->
      let env = Envi.open_expr_scope env in
      let p_typ = Envi.Type.mkvar ~mode None env in
      let body_typ = Envi.Type.mkvar ~mode None env in
      let typ = Envi.Type.Mk.arrow ~mode ~explicit ~label p_typ body_typ env in
      check_type ~loc env expected typ ;
      let add_name =
        match label with
        | Optional _ ->
            fun name typ -> Envi.add_name name (Initial_env.Type.option typ)
        | _ ->
            Envi.add_name
      in
      (* In OCaml, function arguments can't be polymorphic, so we use
         [add_name] to ensure that each check refines them rather than
         instantiating the parameters.
      *)
      let p, names, env = check_pattern env p_typ p in
      let env =
        List.fold ~init:env names ~f:(fun env (name, typ) ->
            add_name name.Location.txt typ env )
      in
      let body, env = get_expression env body_typ body in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env typ ;
      ( { exp_loc= loc
        ; exp_type= typ
        ; exp_desc= Texp_fun (label, p, body, explicit) }
      , env )
  | Pexp_newtype (name, body) ->
      let env = Envi.open_expr_scope env in
      let decl =
        { tdec_ident= name
        ; tdec_params= []
        ; tdec_desc= Pdec_abstract
        ; tdec_loc= loc }
      in
      let ident = map_loc ~f:(Ident.create ~mode) name in
      let decl, env =
        let name = ident.txt in
        let other_name = Path.Pident name in
        match mode with
        | Checked ->
            (* Tri-stitch with itself. *)
            let tri_stitched =
              Type1.Mk.ctor ~mode:Prover 10000 other_name []
            in
            let tri_stitched _ _ = tri_stitched in
            Typet.TypeDecl.import ~name ~tri_stitched decl env
        | Prover ->
            (* Normal stitching. *)
            Typet.TypeDecl.import ~name ~other_name decl env
      in
      let res = Envi.Type.mkvar ~mode None env in
      let body, env = get_expression env res body in
      let env = Envi.close_expr_scope env in
      let free_var = Envi.Type.mkvar ~mode None env in
      let res =
        (* Substitute instances of the type for [free_var]. *)
        let mapper =
          { Type0_map.default_mapper with
            type_expr=
              (fun mapper typ ->
                match (typ.type_desc, typ.type_mode) with
                | Tctor {var_ident= Pident ident'; _}, Checked
                  when Ident.compare ident.txt ident' = 0 ->
                    if phys_equal typ typ.type_alternate.type_alternate then
                      (Type1.get_mode Checked free_var).type_alternate
                        .type_alternate
                    else Type1.get_mode Checked free_var
                | Tctor {var_ident= Pident ident'; _}, Prover
                  when Ident.compare ident.txt ident' = 0 ->
                    Type1.get_mode Prover free_var
                | _ ->
                    Type0_map.default_mapper.type_expr mapper typ ) }
        in
        let snap = Snapshot.create () in
        let res = mapper.type_expr mapper (Type1.flatten res) in
        backtrack_replace snap ; res
      in
      check_type ~loc env expected res ;
      Envi.Type.update_depths env res ;
      ( { exp_loc= loc
        ; exp_type= res
        ; exp_desc= Texp_newtype (decl.tdec_ident, body) }
      , env )
  | Pexp_seq (e1, e2) ->
      let e1, env = get_expression env Initial_env.Type.unit e1 in
      let e2, env = get_expression env expected e2 in
      ({exp_loc= loc; exp_type= e2.exp_type; exp_desc= Texp_seq (e1, e2)}, env)
  | Pexp_let (p, e1, e2) ->
      let env = Envi.open_expr_scope env in
      let p, e1, env = check_binding env p e1 in
      let e2, env = get_expression env expected e2 in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env e2.exp_type ;
      ( {exp_loc= loc; exp_type= e2.exp_type; exp_desc= Texp_let (p, e1, e2)}
      , env )
  | Pexp_instance (name, e1, e2) ->
      let env = Envi.open_expr_scope env in
      let p = Ast_build.Pat.var ~loc:name.loc name.txt in
      let p, e1, env = check_binding env p e1 in
      let ident, typ =
        let ret = ref None in
        let pattern iter pat =
          match pat.Typedast.pat_desc with
          | Tpat_variable name ->
              ret := Some (name, pat.Typedast.pat_type)
          | _ ->
              Typedast_iter.default_iterator.pattern iter pat
        in
        pattern {Typedast_iter.default_iterator with pattern} p ;
        Option.value_exn !ret
      in
      let env = Envi.add_implicit_instance ident.txt typ env in
      let e2, env = get_expression env expected e2 in
      let implicits_instantiated =
        (* Instantiate any implicits that we can within this scope. *)
        Envi.Type.flattened_implicit_vars ~loc ~toplevel:false ~unifies
          Typeset.empty env
      in
      assert (List.is_empty implicits_instantiated) ;
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env e2.exp_type ;
      ( { exp_loc= loc
        ; exp_type= e2.exp_type
        ; exp_desc= Texp_instance (ident, e1, e2) }
      , env )
  | Pexp_constraint (e, typ') ->
      let typ, env = Typet.Type.import typ' env in
      check_type ~loc env expected typ.type_type ;
      let e, env = get_expression env typ.type_type e in
      check_type ~loc env e.exp_type typ.type_type ;
      ( { exp_loc= loc
        ; exp_type= typ.type_type
        ; exp_desc= Texp_constraint (e, typ) }
      , env )
  | Pexp_tuple es ->
      let typs = List.map es ~f:(fun _ -> Envi.Type.mkvar ~mode None env) in
      let typ = Envi.Type.Mk.tuple ~mode typs env in
      check_type ~loc env expected typ ;
      let env = ref env in
      let es =
        List.map2_exn es typs ~f:(fun e expected ->
            let e, env' = get_expression !env expected e in
            env := env' ;
            e )
      in
      let typ =
        Envi.Type.Mk.tuple ~mode
          (List.map es ~f:(fun {exp_type= t; _} -> t))
          !env
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_tuple es}, !env)
  | Pexp_match (e, cases) ->
      let e_typ = Envi.Type.mkvar ~mode None env in
      let e, env = get_expression env e_typ e in
      let typ = e.exp_type in
      let env, cases =
        List.fold_map ~init:env cases ~f:(fun env (p, e) ->
            let env = Envi.open_expr_scope env in
            let p, names, env = check_pattern env typ p in
            let env =
              List.fold ~init:env names ~f:(fun env (name, typ) ->
                  add_polymorphised name.Location.txt typ env )
            in
            let e, env = get_expression env expected e in
            let env = Envi.close_expr_scope env in
            (env, (p, e)) )
      in
      Envi.Type.update_depths env expected ;
      ({exp_loc= loc; exp_type= expected; exp_desc= Texp_match (e, cases)}, env)
  | Pexp_field (e, field) ->
      let field_info =
        match field.txt with
        | Lident _ ->
            None
        | Ldot _ -> (
          match Envi.TypeDecl.find_of_field ~mode field env with
          | Some
              ( fld_ident
              , ({tdec_desc= TRecord field_decls; tdec_params; tdec_ret; _}, i)
              ) ->
              let vars =
                List.map
                  ~f:(fun _ -> Envi.Type.mkvar ~mode None env)
                  tdec_params
              in
              let decl_type =
                Envi.Type.instantiate tdec_params vars tdec_ret env
              in
              let {fld_type; _} = List.nth_exn field_decls i in
              let fld_type =
                Envi.Type.instantiate tdec_params vars fld_type env
              in
              check_type ~loc env expected fld_type ;
              Some (fld_type, decl_type, fld_ident)
          | _ ->
              None )
        | Lapply _ ->
            failwith "Unhandled Lapply in field name"
      in
      let typ, decl_type, fld_ident, resolved =
        match field_info with
        | Some (fld_type, decl_type, fld_ident) ->
            (fld_type, decl_type, Some fld_ident, true)
        | None ->
            let fld_type = expected in
            let decl_type = Envi.Type.mkvar ~mode None env in
            (fld_type, decl_type, None, false)
      in
      let e, env = get_expression env decl_type e in
      let fld_ident, typ =
        if resolved then (Option.value_exn fld_ident, typ)
        else
          match Envi.TypeDecl.find_unaliased_of_type ~loc e.exp_type env with
          | Some
              ( {tdec_desc= TRecord field_decls; tdec_params; _}
              , {type_desc= Tctor {var_params; _}; _} ) -> (
            match
              List.find field_decls ~f:(fun {fld_ident; _} ->
                  match field.txt with
                  | Lident field ->
                      String.equal (Ident.name fld_ident) field
                  | _ ->
                      assert false )
            with
            | Some {fld_type; fld_ident; _} ->
                let fld_type =
                  Envi.Type.instantiate tdec_params var_params fld_type env
                in
                check_type ~loc env typ fld_type ;
                (Path.Pident fld_ident, fld_type)
            | None ->
                raise
                  (Error (loc, env, Wrong_record_field (field.txt, e.exp_type)))
            )
          | _ -> (
            match Envi.TypeDecl.find_of_field ~mode field env with
            | Some
                ( fld_ident
                , ( {tdec_desc= TRecord field_decls; tdec_params; tdec_ret; _}
                  , i ) ) ->
                let vars =
                  List.map
                    ~f:(fun _ -> Envi.Type.mkvar ~mode None env)
                    tdec_params
                in
                let e_typ =
                  Envi.Type.instantiate tdec_params vars tdec_ret env
                in
                check_type ~loc env e.exp_type e_typ ;
                let {fld_type; _} = List.nth_exn field_decls i in
                let fld_type =
                  Envi.Type.instantiate tdec_params vars fld_type env
                in
                (fld_ident, fld_type)
            | _ ->
                raise (Error (loc, env, Unbound ("record field", field))) )
      in
      ( { exp_loc= loc
        ; exp_type= typ
        ; exp_desc= Texp_field (e, Location.mkloc fld_ident field.loc) }
      , env )
  | Pexp_record ([], _) ->
      raise (Error (loc, env, Empty_record))
  | Pexp_record (((field, _) :: _ as fields), ext) ->
      let typ, ext, env =
        match ext with
        | Some ext ->
            let ext, env = get_expression env expected ext in
            (ext.exp_type, Some ext, env)
        | None ->
            (expected, None, env)
      in
      let typ = Type1.flatten typ in
      let typ, field_decls, type_vars, bound_vars =
        match Envi.TypeDecl.find_unaliased_of_type ~loc typ env with
        | Some
            ( {tdec_desc= TRecord field_decls; tdec_params; _}
            , {type_desc= Tctor {var_params; _}; _} ) ->
            (typ, field_decls, tdec_params, var_params)
        | _ -> (
          match Envi.TypeDecl.find_of_field ~mode field env with
          | Some
              ( _fld_ident
              , ({tdec_desc= TRecord field_decls; tdec_params; tdec_ret; _}, _)
              ) ->
              let vars =
                List.map
                  ~f:(fun _ -> Envi.Type.mkvar ~mode None env)
                  tdec_params
              in
              let tdec_ret = get_mode mode tdec_ret in
              let decl_type =
                Envi.Type.instantiate tdec_params vars tdec_ret env
              in
              check_type ~loc env typ decl_type ;
              (decl_type, field_decls, tdec_params, vars)
          | _ ->
              raise (Error (loc, env, Unbound ("record field", field))) )
      in
      let type_vars = List.map ~f:(Type1.get_mode mode) type_vars in
      let bound_vars = List.map ~f:(Type1.get_mode mode) bound_vars in
      let env = ref env in
      let fields_filled = Array.create ~len:(List.length field_decls) false in
      let fields =
        List.map fields ~f:(fun (field, e) ->
            let path, i, field_typ, record_typ =
              get_field_of_decl typ type_vars bound_vars field_decls field !env
            in
            ( try check_type ~loc:field.loc !env record_typ typ
              with
              | Error (_, env, Check_failed (_, _, Cannot_unify (typ, _))) ->
                raise
                  (Error (field.loc, env, Wrong_record_field (field.txt, typ)))
            ) ;
            let e, env' = get_expression !env field_typ e in
            ( if fields_filled.(i) then
              let name = (List.nth_exn field_decls i).fld_ident in
              raise (Error (field.loc, !env, Repeated_field name)) ) ;
            fields_filled.(i) <- true ;
            env := env' ;
            (Location.mkloc path field.loc, e) )
      in
      ( match ext with
      | Some _ ->
          () (* TODO: warn when all fields have been provided. *)
      | None ->
          let fields_filled = Array.to_list fields_filled in
          let names =
            List.fold2_exn ~init:[] fields_filled field_decls
              ~f:(fun names filled {fld_ident; _} ->
                if filled then names else fld_ident :: names )
          in
          if not (List.is_empty names) then
            raise (Error (loc, !env, Missing_fields names)) ) ;
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_record (fields, ext)}, !env)
  | Pexp_ctor (name, arg) ->
      let name', typ, arg_typ = get_ctor name env in
      let name' = Location.mkloc name' name.loc in
      check_type ~loc env expected typ ;
      let arg, env =
        match arg with
        | Some arg ->
            let arg, env = get_expression env arg_typ arg in
            (Some arg, env)
        | None ->
            let typ = Envi.Type.Mk.tuple ~mode [] env in
            ( try check_type ~loc env arg_typ typ
              with _ -> raise (Error (loc, env, Argument_expected name.txt)) ) ;
            (None, env)
      in
      ({exp_loc= loc; exp_type= typ; exp_desc= Texp_ctor (name', arg)}, env)
  | Pexp_row_ctor (name, args) ->
      let name = map_loc ~f:Ident.create_row name in
      let arg_types =
        List.map args ~f:(fun _ -> Envi.Type.Mk.var ~mode None env)
      in
      let typ = Envi.Type.Mk.row_of_ctor ~mode name.txt arg_types env in
      check_type ~loc env expected typ ;
      let env = ref env in
      let args =
        List.map2_exn args arg_types ~f:(fun arg expected ->
            let arg, env' = get_expression !env expected arg in
            env := env' ;
            arg )
      in
      ( {exp_loc= loc; exp_type= typ; exp_desc= Texp_row_ctor (name, args)}
      , !env )
  | Pexp_unifiable _ ->
      raise (Error (loc, env, Unifiable_expr))
  | Pexp_if (e1, e2, None) ->
      check_type ~loc env (get_mode mode Initial_env.Type.unit) expected ;
      let e1, env =
        get_expression env (get_mode mode Initial_env.Type.bool) e1
      in
      let e2, env =
        get_expression env (get_mode mode Initial_env.Type.unit) e2
      in
      ( { exp_loc= loc
        ; exp_type= get_mode mode Initial_env.Type.unit
        ; exp_desc= Texp_if (e1, e2, None) }
      , env )
  | Pexp_if (e1, e2, Some e3) ->
      let e1, env =
        get_expression env (get_mode mode Initial_env.Type.bool) e1
      in
      let e2, env = get_expression env expected e2 in
      let e3, env = get_expression env expected e3 in
      ( {exp_loc= loc; exp_type= expected; exp_desc= Texp_if (e1, e2, Some e3)}
      , env )
  | Pexp_prover e ->
      let env = Envi.open_expr_scope ~mode:Prover env in
      let e, env = get_expression env (Type1.get_mode Prover expected) e in
      check_type ~loc env expected (Type1.get_mode mode e.exp_type) ;
      let _, env = Envi.pop_expr_scope env in
      let conv =
        match mode with
        | Checked ->
            (* Convert all prover-mode implicits to checked-mode equivalents if
               necessary.
            *)
            Envi.wrap_prover_implicits env ;
            get_conversion ~loc ~may_identity:false ~can_add_args:true env
              expected
        | Prover ->
            let dummy_typ = Envi.Type.Mk.var ~mode:Prover None env in
            { conv_desc=
                Tconv_body
                  { conv_body_desc= Tconv_identity
                  ; conv_body_loc= loc
                  ; conv_body_type= dummy_typ }
            ; conv_loc= loc
            ; conv_type= dummy_typ }
      in
      let implicits =
        (* Instantiate unfilled implicit arguments in [conv]. *)
        let implicits, _ = get_implicits conv.conv_type in
        List.map implicits ~f:(fun (label, typ) ->
            (label, Envi.Type.new_implicit_var ~loc typ env) )
      in
      ( { exp_loc= loc
        ; exp_type= expected
        ; exp_desc= Texp_prover (conv, implicits, e) }
      , env )

and check_binding ?(toplevel = false) (env : Envi.t) p e : 's =
  let loc = e.exp_loc in
  let mode = Envi.current_mode env in
  let typ = Envi.Type.mkvar ~mode None env in
  let p, pattern_variables, env = check_pattern env typ p in
  let typ = Type1.flatten typ in
  let env = Envi.open_expr_scope env in
  let e, env = get_expression env typ e in
  let env = Envi.close_expr_scope env in
  Envi.Type.update_depths env e.exp_type ;
  let exp_type = Type1.flatten e.exp_type in
  let e = {e with exp_type} in
  let typ_vars = free_type_vars ~depth:env.Envi.depth exp_type in
  let implicit_vars =
    Envi.Type.flattened_implicit_vars ~loc ~toplevel ~unifies typ_vars env
  in
  let implicit_vars =
    List.filter implicit_vars ~f:(fun var ->
        let exp_type = Type1.remove_mode_changes var.exp_type in
        match (var.exp_desc, exp_type.type_desc) with
        | Texp_unifiable unif, Tconv _ -> (
            (* Try to find a conversion. *)
            let snap = Snapshot.create () in
            try
              (* TODO: can_add_args= true *)
              let conv =
                get_conversion
                  ~may_identity:(equal_mode exp_type.type_mode Prover)
                  ~can_add_args:false ~loc:var.exp_loc env exp_type
              in
              unif.expression
              <- Some
                   {exp_desc= Texp_convert conv; exp_type; exp_loc= var.exp_loc} ;
              false
            with _ -> backtrack snap ; true )
        | _ ->
            true )
  in
  match implicit_vars with
  | [] ->
      let env =
        List.fold ~init:env pattern_variables ~f:(fun env (name, typ) ->
            add_polymorphised name.Location.txt typ env )
      in
      (p, e, env)
  | implicit :: _ ->
      let name, typ =
        match pattern_variables with
        | [(name, typ)] ->
            (name, Type1.flatten typ)
        | _ ->
            raise (Error (loc, env, No_instance implicit.exp_type))
      in
      let e =
        match p.pat_desc with
        | Tpat_variable _ ->
            e
        | _ ->
            (* Use a let-expression to extract the variable from the expression. *)
            { exp_loc= p.pat_loc
            ; exp_type= typ
            ; exp_desc=
                Texp_let
                  ( p
                  , e
                  , { exp_loc= p.pat_loc
                    ; exp_type= typ
                    ; exp_desc=
                        Texp_variable
                          (Location.mkloc (Path.Pident name.txt) name.loc) } )
            }
      in
      (* Write function arguments for the implicit vars. *)
      let e, env =
        List.fold ~init:(e, env) implicit_vars ~f:(fun (e, env) var ->
            match var.exp_desc with
            | Texp_unifiable {expression= None; name; _} ->
                let var_typ, exp_typ = (var.exp_type, e.exp_type) in
                let var_typ =
                  match (var_typ.type_mode, exp_typ.type_mode) with
                  | Checked, Checked | Prover, Prover ->
                      var_typ
                  | Prover, Checked ->
                      Type1.Mk.other_mode ~mode:Checked var_typ.type_depth
                        var_typ
                  | Checked, Prover ->
                      Type1.Mk.other_mode ~mode:Prover var_typ.type_depth
                        var_typ
                in
                let exp_type =
                  Envi.Type.Mk.arrow ~mode ~explicit:Implicit var_typ exp_typ
                    env
                in
                let p =
                  { Typedast.pat_desc= Tpat_variable name
                  ; pat_loc= loc
                  ; pat_type= var_typ }
                in
                ( { Typedast.exp_desc= Texp_fun (Nolabel, p, e, Implicit)
                  ; exp_type
                  ; exp_loc= loc }
                , env )
            | _ ->
                raise (Error (var.exp_loc, env, No_unifiable_expr)) )
      in
      let p =
        { Typedast.pat_loc= p.pat_loc
        ; pat_type= e.exp_type
        ; pat_desc= Tpat_variable name }
      in
      let env = add_polymorphised name.Location.txt e.exp_type env in
      (p, e, env)

let type_extension ~loc variant ctors env =
  let mode = Envi.current_mode env in
  let {Parsetypes.var_ident; var_params} = variant in
  let path, {tdec_params; tdec_desc; _} =
    match Envi.raw_find_type_declaration ~mode var_ident env with
    | open_decl ->
        open_decl
    | exception _ ->
        raise (Error (loc, env, Unbound ("type constructor", var_ident)))
  in
  ( match tdec_desc with
  | TOpen ->
      ()
  | _ ->
      raise (Error (loc, env, Not_extensible var_ident.txt)) ) ;
  ( match List.iter2 tdec_params var_params ~f:(fun _ _ -> ()) with
  | Ok _ ->
      ()
  | Unequal_lengths ->
      raise (Error (loc, env, Extension_different_arity var_ident.txt)) ) ;
  let tdec_ident =
    match path with
    | Pident name ->
        Ident.name name
    | Pdot (_, _, name) ->
        name
    | _ ->
        assert false
  in
  let decl =
    { Parsetypes.tdec_ident= Location.mkloc tdec_ident loc
    ; tdec_params= var_params
    ; tdec_desc= Pdec_extend (Location.mkloc path var_ident.loc, ctors)
    ; tdec_loc= loc }
  in
  let decl, env =
    match Typet.TypeDecl.import_rec [decl] env with
    | [decl], env ->
        (decl, env)
    | _ ->
        assert false
  in
  let ctors =
    match decl.tdec_desc with
    | Tdec_extend (_, ctors) ->
        ctors
    | _ ->
        failwith "Expected a TExtend."
  in
  let variant =
    { Typedast.var_ident= Location.mkloc path var_ident.loc
    ; var_params= decl.tdec_params }
  in
  (env, variant, ctors)

let rec check_signature_item env item =
  let mode = Envi.current_mode env in
  let loc = item.sig_loc in
  match item.sig_desc with
  | Psig_value (name, typ) ->
      let env = Envi.open_expr_scope env in
      let typ, env = Typet.Type.import typ env in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env typ.type_type ;
      let name = map_loc ~f:(Ident.create ~mode) name in
      let env = add_polymorphised name.txt typ.type_type env in
      (env, {Typedast.sig_desc= Tsig_value (name, typ); sig_loc= loc})
  | Psig_instance (name, typ) ->
      let env = Envi.open_expr_scope env in
      let typ, env = Typet.Type.import typ env in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env typ.type_type ;
      let name = map_loc ~f:(Ident.create ~mode) name in
      let typ' = Type1.flatten typ.type_type in
      let typ' = polymorphise typ' env in
      let env = Envi.add_name name.txt typ' env in
      let env = Envi.add_implicit_instance name.txt typ' env in
      (env, {Typedast.sig_desc= Tsig_instance (name, typ); sig_loc= loc})
  | Psig_type decl ->
      let decl, env = Typet.TypeDecl.import decl env in
      (env, {Typedast.sig_desc= Tsig_type decl; sig_loc= loc})
  | Psig_convtype (decl, tconv, convname) ->
      if not (equal_mode mode Checked) then
        raise (Error (loc, env, Convertible_not_in_checked)) ;
      let decl, tconv, env =
        Typet.TypeDecl.import_convertible decl tconv env
      in
      let typ =
        (* Build the type corresponding to the conversion. *)
        let open Typedast in
        let params =
          List.mapi decl.tdec_params ~f:(fun i typ ->
              { typ with
                type_desc=
                  Ttyp_var
                    (Some
                       (Location.mkloc (sprintf "var_%i" (i + 1)) typ.type_loc))
              } )
        in
        let conv_params =
          List.mapi decl.tdec_params ~f:(fun i typ ->
              { typ with
                type_desc=
                  Ttyp_var
                    (Some
                       (Location.mkloc
                          (sprintf "value_%i" (i + 1))
                          typ.type_loc))
              ; type_type= typ.type_type.type_alternate } )
        in
        let mk_ctor path params type_type =
          { type_desc= Ttyp_ctor {var_ident= path; var_params= params}
          ; type_loc= loc
          ; type_type }
        in
        let typ =
          mk_ctor
            (map_loc ~f:(fun x -> Path.Pident x) decl.tdec_ident)
            params decl.tdec_tdec.tdec_ret
        in
        let other_typ =
          match tconv with
          | Ttconv_with (_, other_decl) ->
              mk_ctor
                (map_loc ~f:(fun x -> Path.Pident x) decl.tdec_ident)
                params
                (get_mode Prover other_decl.tdec_tdec.tdec_ret)
          | Ttconv_to typ ->
              typ
        in
        let loc =
          Option.value_map convname ~default:loc ~f:(fun {Location.loc; _} ->
              loc )
        in
        let typ = Typet.Type.mk_conv ~loc ~mode typ other_typ env in
        List.fold2_exn (List.rev params) (List.rev conv_params) ~init:typ
          ~f:(fun typ param conv_param ->
            let param = Typet.Type.mk_conv ~loc ~mode param conv_param env in
            Typet.Type.mk_arrow ~loc ~mode ~explicit:Implicit param typ env )
      in
      let typ' = polymorphise (Type1.flatten typ.type_type) env in
      Envi.Type.update_depths env typ' ;
      let convname =
        match convname with
        | Some convname ->
            map_loc ~f:(Ident.create ~mode) convname
        | None ->
            let name = Ident.name decl.tdec_ident.txt in
            let name = if name = "t" then "typ" else sprintf "%s_typ" name in
            Location.mkloc (Ident.create ~mode name) loc
      in
      let env = Envi.add_name convname.txt typ' env in
      let env = Envi.add_implicit_instance convname.txt typ' env in
      let env =
        Envi.add_implicit_instance convname.txt typ'.type_alternate env
      in
      ( env
      , { Typedast.sig_desc= Tsig_convtype (decl, tconv, convname, typ)
        ; sig_loc= loc } )
  | Psig_rectype decls ->
      let decls, env = Typet.TypeDecl.import_rec decls env in
      (env, {Typedast.sig_desc= Tsig_rectype decls; sig_loc= loc})
  | Psig_module (name, msig) ->
      let name = map_loc ~f:(Ident.create ~mode) name in
      let msig, m, env = check_module_sig env msig in
      let env =
        match m with
        | Envi.Scope.Immediate m ->
            Envi.add_module name.txt m env
        | Envi.Scope.Deferred path ->
            Envi.add_deferred_module name.txt path env
      in
      (env, {Typedast.sig_desc= Tsig_module (name, msig); sig_loc= loc})
  | Psig_modtype (name, signature) ->
      let env = Envi.open_module env in
      let signature, m_env, env = check_module_sig env signature in
      let name = map_loc ~f:(Ident.create ~mode) name in
      let env = Envi.add_module_type name.txt m_env env in
      (env, {Typedast.sig_desc= Tsig_modtype (name, signature); sig_loc= loc})
  | Psig_open name ->
      let path, m = Envi.find_module ~mode ~loc name env in
      let env = Envi.open_namespace_scope path m env in
      ( env
      , { Typedast.sig_desc= Tsig_open (Location.mkloc path name.loc)
        ; sig_loc= loc } )
  | Psig_typeext (variant, ctors) ->
      let env, variant, ctors = type_extension ~loc variant ctors env in
      (env, {Typedast.sig_desc= Tsig_typeext (variant, ctors); sig_loc= loc})
  | Psig_request (arg, ctor_decl) ->
      let open Ast_build in
      let variant =
        Type.variant ~loc ~params:[Type.none ~loc ()]
          (Lid.of_list ["Snarky__Request"; "t"])
      in
      let ctor_ret =
        Type.mk ~loc (Ptyp_ctor {variant with var_params= [arg]})
      in
      let ctor_decl = {ctor_decl with ctor_ret= Some ctor_ret} in
      let env, _variant, ctors = type_extension ~loc variant [ctor_decl] env in
      let ctor_decl, arg =
        match ctors with
        | [ ( { ctor_ret= Some {type_desc= Ttyp_ctor {var_params= [arg]; _}; _}
              ; _ } as ctor_decl ) ] ->
            (ctor_decl, arg)
        | _ ->
            assert false
      in
      (env, {Typedast.sig_desc= Tsig_request (arg, ctor_decl); sig_loc= loc})
  | Psig_multiple sigs ->
      let env, sigs = check_signature env sigs in
      (env, {Typedast.sig_desc= Tsig_multiple sigs; sig_loc= loc})
  | Psig_prover sigs ->
      let env = Envi.open_mode_module_scope Prover env in
      let env, sigs = check_signature env sigs in
      let env = Envi.open_mode_module_scope mode env in
      (env, {Typedast.sig_desc= Tsig_prover sigs; sig_loc= loc})
  | Psig_convert (name, typ) ->
      let env = Envi.open_expr_scope env in
      let typ, env = Typet.Type.import typ env in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env typ.type_type ;
      let name = map_loc ~f:(Ident.create ~mode) name in
      let typ' = polymorphise (Type1.flatten typ.type_type) env in
      let env = Envi.add_name name.txt typ' env in
      let env = Envi.add_implicit_instance name.txt typ' env in
      let env = Envi.add_implicit_instance name.txt typ'.type_alternate env in
      (env, {Typedast.sig_desc= Tsig_convert (name, typ); sig_loc= loc})

and check_signature env signature =
  List.fold_map ~init:env signature ~f:check_signature_item

and check_module_sig env msig =
  let mode = Envi.current_mode env in
  let loc = msig.msig_loc in
  match msig.msig_desc with
  | Pmty_sig signature ->
      let env = Envi.open_module env in
      let env, signature = check_signature env signature in
      let m, env = Envi.pop_module ~loc env in
      ( {Typedast.msig_desc= Tmty_sig signature; msig_loc= loc}
      , Envi.Scope.Immediate m
      , env )
  | Pmty_name lid ->
      let path, m =
        match Envi.find_module_type ~mode lid env with
        | Some m ->
            m
        | None ->
            raise (Envi.Error (loc, Unbound ("module type", lid.txt)))
      in
      ( { Typedast.msig_desc= Tmty_name (Location.mkloc path lid.loc)
        ; msig_loc= loc }
      , m
      , env )
  | Pmty_alias lid ->
      let path, m =
        match Envi.find_module_deferred ~loc ~mode lid env with
        | Some m ->
            m
        | None ->
            raise (Envi.Error (loc, Unbound ("module", lid.txt)))
      in
      ( { Typedast.msig_desc= Tmty_alias (Location.mkloc path lid.loc)
        ; msig_loc= loc }
      , m
      , env )
  | Pmty_abstract ->
      let env = Envi.open_module env in
      let m, env = Envi.pop_module ~loc env in
      ( {Typedast.msig_desc= Tmty_abstract; msig_loc= loc}
      , Envi.Scope.Immediate m
      , env )
  | Pmty_functor (f_name, f, msig) ->
      let f, f_mty, env = check_module_sig env f in
      let ftor f_instance =
        (* We want the functored module to be accessible only in un-prefixed
           space.
        *)
        let env = Envi.open_module env in
        (* TODO: This name should be constant, and the underlying module
           substituted.
        *)
        let f_name = map_loc ~f:(Ident.create ~mode) f_name in
        let env =
          match f_instance with
          | Envi.Scope.Immediate f ->
              Envi.add_module f_name.txt f env
          | Envi.Scope.Deferred path ->
              Envi.add_deferred_module f_name.txt path env
        in
        (* TODO: check that f_instance matches f_mty *)
        let msig, m, _env = check_module_sig env msig in
        match m with
        | Envi.Scope.Immediate m ->
            (m, msig)
        | Envi.Scope.Deferred path ->
            ( snd
                (Envi.find_module ~mode ~loc
                   (Location.mkloc (Longident.Lident path) loc)
                   env)
            , msig )
      in
      (* Check that f_mty builds the functor as expected. *)
      let _, msig = ftor f_mty in
      let m = Envi.make_functor ~mode (fun f -> fst (ftor f)) in
      ( {Typedast.msig_desc= Tmty_functor (f_name, f, msig); msig_loc= loc}
      , Envi.Scope.Immediate m
      , env )

let rec check_statement env stmt =
  let mode = Envi.current_mode env in
  let loc = stmt.stmt_loc in
  match stmt.stmt_desc with
  | Pstmt_value (p, e) ->
      let env = Envi.open_expr_scope env in
      let p, e, env = check_binding ~toplevel:true env p e in
      let scope, env = Envi.pop_expr_scope env in
      (* Uplift the names from the expression scope, discarding the scope and
         its associated type variables etc. *)
      let env = Envi.join_expr_scope env scope in
      (env, {Typedast.stmt_loc= loc; stmt_desc= Tstmt_value (p, e)})
  | Pstmt_instance (name, e) ->
      let env = Envi.open_expr_scope env in
      let p = {pat_desc= Ppat_variable name; pat_loc= name.loc} in
      let p, e, env = check_binding ~toplevel:true env p e in
      let name =
        let exception Ret of Ident.t Location.loc in
        let iter =
          { Typedast_iter.default_iterator with
            pattern_desc=
              (fun iter p ->
                match p with
                | Tpat_variable name ->
                    raise (Ret name)
                | _ ->
                    Typedast_iter.default_iterator.pattern_desc iter p ) }
        in
        try
          iter.pattern iter p ;
          assert false
        with Ret name -> name
      in
      let scope, env = Envi.pop_expr_scope env in
      let env = Envi.join_expr_scope env scope in
      let typ = Type1.flatten e.exp_type in
      let typ = polymorphise typ env in
      let env = Envi.add_implicit_instance name.txt typ env in
      (env, {Typedast.stmt_loc= loc; stmt_desc= Tstmt_instance (name, e)})
  | Pstmt_type decl -> (
    try
      (* Bail if we're not in checked mode. *)
      assert (equal_mode Checked mode) ;
      let name = Ident.create ~mode:Checked decl.tdec_ident.txt in
      let alt_name = Ident.create ~mode:Prover decl.tdec_ident.txt in
      let decl, env =
        Typet.TypeDecl.import ~name ~other_name:(Path.Pident alt_name) decl env
      in
      let alt_decl =
        let mapper =
          { Type0_map.default_mapper with
            type_expr= (fun _mapper typ -> typ.type_alternate) }
        in
        let decl =
          Untype_ast.Type0.type_decl ~loc
            (Ident.name decl.tdec_ident.txt)
            (mapper.type_decl mapper decl.tdec_tdec)
        in
        decl
      in
      let env = Envi.open_mode_module_scope Prover env in
      let alt_decl, env =
        Typet.TypeDecl.import ~name:alt_name ~other_name:(Path.Pident name)
          alt_decl env
      in
      let env = Envi.open_mode_module_scope Checked env in
      let convname =
        let name = Ident.name name in
        let name = if name = "t" then "typ" else sprintf "%s_typ" name in
        Location.mkloc (Ident.create ~mode name) loc
      in
      let typ, typ_params =
        let decl = decl.tdec_tdec in
        let snap = Snapshot.create () in
        Envi.Type.refresh_vars decl.tdec_params env ;
        let typ_params =
          List.map decl.tdec_params ~f:(fun typ -> Envi.Type.copy typ env)
        in
        let typ = Envi.Type.copy decl.tdec_ret env in
        backtrack snap ; (typ, typ_params)
      in
      let typ = Envi.Type.Mk.conv ~mode typ typ.type_alternate env in
      let typ =
        List.fold_right typ_params ~init:typ ~f:(fun param typ ->
            let param =
              Envi.Type.Mk.conv ~mode param param.type_alternate env
            in
            Envi.Type.Mk.arrow ~mode ~explicit:Implicit param typ env )
      in
      let conv =
        get_conversion ~may_identity:false ~can_add_args:true ~loc env typ
      in
      let typ = polymorphise (Type1.flatten conv.conv_type) env in
      Envi.Type.update_depths env typ ;
      let env = Envi.add_name convname.txt typ env in
      let env = Envi.add_implicit_instance convname.txt typ env in
      let env =
        Envi.add_implicit_instance convname.txt typ.type_alternate env
      in
      let stmt =
        { Typedast.stmt_loc= loc
        ; stmt_desc=
            Tstmt_convtype
              (decl, Ttconv_with (Prover, alt_decl), convname, conv) }
      in
      (env, stmt)
    with _err ->
      (*Format.eprintf "%s@." (Printexc.get_backtrace ()) ;
      Location.report_exception Format.err_formatter _err ;*)
      let decl, env = Typet.TypeDecl.import decl env in
      let stmt = {Typedast.stmt_loc= loc; stmt_desc= Tstmt_type decl} in
      (env, stmt) )
  | Pstmt_convtype (decl, tconv, convname) ->
      if not (equal_mode mode Checked) then
        raise (Error (loc, env, Convertible_not_in_checked)) ;
      let decl, tconv, env =
        Typet.TypeDecl.import_convertible decl tconv env
      in
      let typ = decl.tdec_tdec.tdec_ret in
      let typ = Envi.Type.Mk.conv ~mode typ typ.type_alternate env in
      let typ =
        List.fold_right decl.tdec_tdec.tdec_params ~init:typ
          ~f:(fun param typ ->
            let param =
              Envi.Type.Mk.conv ~mode param param.type_alternate env
            in
            Envi.Type.Mk.arrow ~mode param typ env )
      in
      let conv =
        get_conversion ~may_identity:false ~can_add_args:true ~loc env typ
      in
      let typ = polymorphise (Type1.flatten conv.conv_type) env in
      Envi.Type.update_depths env typ ;
      let convname =
        match convname with
        | Some convname ->
            map_loc ~f:(Ident.create ~mode) convname
        | None ->
            let name = Ident.name decl.tdec_ident.txt in
            let name = if name = "t" then "typ" else sprintf "%s_typ" name in
            Location.mkloc (Ident.create ~mode name) loc
      in
      let env = Envi.add_name convname.txt typ env in
      let env = Envi.add_implicit_instance convname.txt typ env in
      let env =
        Envi.add_implicit_instance convname.txt typ.type_alternate env
      in
      ( env
      , { Typedast.stmt_desc= Tstmt_convtype (decl, tconv, convname, conv)
        ; stmt_loc= loc } )
  | Pstmt_rectype decls ->
      let decls, env = Typet.TypeDecl.import_rec decls env in
      (env, {Typedast.stmt_desc= Tstmt_rectype decls; stmt_loc= loc})
  | Pstmt_module (name, m) ->
      let env = Envi.open_module env in
      let env, m = check_module_expr env m in
      let m_env, env = Envi.pop_module ~loc env in
      let name = map_loc ~f:(Ident.create ~mode) name in
      let env = Envi.add_module name.txt m_env env in
      (env, {Typedast.stmt_loc= loc; stmt_desc= Tstmt_module (name, m)})
  | Pstmt_modtype (name, signature) ->
      let signature, m_env, env = check_module_sig env signature in
      let name = map_loc ~f:(Ident.create ~mode) name in
      let env = Envi.add_module_type name.txt m_env env in
      ( env
      , {Typedast.stmt_loc= loc; stmt_desc= Tstmt_modtype (name, signature)} )
  | Pstmt_open name ->
      let path, m = Envi.find_module ~mode ~loc name env in
      ( Envi.open_namespace_scope path m env
      , { Typedast.stmt_loc= loc
        ; stmt_desc= Tstmt_open (Location.mkloc path name.loc) } )
  | Pstmt_open_instance name ->
      let path, m = Envi.find_module ~mode ~loc name env in
      ( Envi.open_instance_scope path m env
      , { Typedast.stmt_loc= loc
        ; stmt_desc= Tstmt_open_instance (Location.mkloc path name.loc) } )
  | Pstmt_typeext (variant, ctors) ->
      let env, variant, ctors = type_extension ~loc variant ctors env in
      (env, {Typedast.stmt_loc= loc; stmt_desc= Tstmt_typeext (variant, ctors)})
  | Pstmt_request (arg, ctor_decl, handler) ->
      let open Ast_build in
      let variant =
        Type.variant ~loc ~params:[Type.none ~loc ()]
          (Lid.of_list ["Snarky"; "Request"; "t"])
      in
      let ctor_ret =
        Type.mk ~loc (Ptyp_ctor {variant with var_params= [arg]})
      in
      let ctor_decl = {ctor_decl with ctor_ret= Some ctor_ret} in
      let env, _, ctors = type_extension ~loc variant [ctor_decl] env in
      let ctor_decl, arg =
        match ctors with
        | [ ( { ctor_ret= Some {type_desc= Ttyp_ctor {var_params= [arg]; _}; _}
              ; _ } as ctor ) ] ->
            (ctor, arg)
            (*match Envi.raw_find_type_declaration ~mode (Lid.of_list ["Snarky"; "Request"; "t"])
            { (Untype_ast.ctor_decl ctor) with
              ctor_ret=
                Some
                  (Type.mk ~loc
                     (Ptyp_ctor
                        (Type.variant ~loc ~params:[arg]
                           (Lid.of_list ["Snarky"; "Request"; "t"])))) }, arg'*)
        | _ ->
            failwith "Wrong number of constructors returned for Request."
      in
      let name = ctor_decl.ctor_ident.txt in
      let handler, env =
        match handler with
        | Some (pat, body) ->
            let loc, pat_loc =
              Location.(
                match pat with
                | Some pat ->
                    ( {body.exp_loc with loc_start= pat.pat_loc.loc_start}
                    , pat.pat_loc )
                | None ->
                    (body.exp_loc, body.exp_loc))
            in
            let p = Pat.var ~loc ("handle_" ^ Ident.name name) in
            let e =
              let request = Lid.of_name "request" in
              let respond = Lid.of_name "respond" in
              let body =
                Exp.let_ ~loc (Pat.var "unhandled")
                  (Exp.var (Lid.of_list ["Snarky"; "Request"; "unhandled"]))
                  (Exp.match_ ~loc:stmt.stmt_loc
                     (Exp.var ~loc (Lid.of_name "request"))
                     [ ( Pat.ctor ~loc:pat_loc
                           (Lid.of_name (Ident.name name))
                           ?args:pat
                       , body )
                     ; ( Pat.any ()
                       , Exp.var
                           (Lid.of_list ["Snarky"; "Request"; "unhandled"]) )
                     ])
              in
              Exp.fun_
                (Pat.ctor
                   (Lid.of_list ["Snarky__Request"; "With"])
                   ~args:(Pat.record [Pat.field request; Pat.field respond]))
                body
            in
            let _p, e, env = check_binding ~toplevel:true env p e in
            let pat, body =
              match e with
              | { exp_desc=
                    Texp_fun
                      ( Nolabel
                      , _
                      , { exp_desc=
                            Texp_let
                              ( _
                              , _
                              , { exp_desc=
                                    Texp_match
                                      ( _
                                      , [ ( {pat_desc= Tpat_ctor (_, pat); _}
                                          , body )
                                        ; _ ] )
                                ; _ } )
                        ; _ }
                      , _ )
                ; _ } ->
                  (pat, body)
              | _ ->
                  failwith "Unexpected output of check_binding for Request"
            in
            (Some (pat, body), env)
        | None ->
            (None, env)
      in
      (env, {stmt_loc= loc; stmt_desc= Tstmt_request (arg, ctor_decl, handler)})
  | Pstmt_multiple stmts ->
      let env, stmts = List.fold_map ~init:env stmts ~f:check_statement in
      (env, {stmt_loc= loc; stmt_desc= Tstmt_multiple stmts})
  | Pstmt_prover stmts ->
      let env = Envi.open_mode_module_scope Prover env in
      let env, stmts = List.fold_map ~init:env stmts ~f:check_statement in
      let env = Envi.open_mode_module_scope mode env in
      (env, {stmt_loc= loc; stmt_desc= Tstmt_prover stmts})
  | Pstmt_convert (name, typ) ->
      let env = Envi.open_expr_scope env in
      let typ, env = Typet.Type.import typ env in
      let env = Envi.close_expr_scope env in
      Envi.Type.update_depths env typ.type_type ;
      let conv =
        get_conversion ~may_identity:false ~can_add_args:false ~loc env
          typ.type_type
      in
      let name = map_loc ~f:(Ident.create ~mode) name in
      let typ' = polymorphise (Type1.flatten typ.type_type) env in
      let env = Envi.add_name name.txt typ' env in
      let env = Envi.add_implicit_instance name.txt typ' env in
      ( env
      , {Typedast.stmt_desc= Tstmt_convert (name, typ, conv); stmt_loc= loc} )

and check_module_expr env m =
  let mode = Envi.current_mode env in
  let loc = m.mod_loc in
  match m.mod_desc with
  | Pmod_struct stmts ->
      let env, stmts = List.fold_map ~f:check_statement ~init:env stmts in
      (env, {Typedast.mod_loc= loc; mod_desc= Tmod_struct stmts})
  | Pmod_name name ->
      (* Remove the module placed on the stack by the caller. *)
      let _, env = Envi.pop_module ~loc env in
      let name', m' = Envi.find_module ~mode ~loc name env in
      let name = Location.mkloc name' name.loc in
      let env = Envi.push_scope m' env in
      (env, {Typedast.mod_loc= loc; mod_desc= Tmod_name name})
  | Pmod_functor (f_name, f, m) ->
      (* Remove the module placed on the stack by the caller. *)
      let _, env = Envi.pop_module ~loc env in
      let f, f', env = check_module_sig env f in
      let ftor f_instance =
        (* We want the functored module to be accessible only in un-prefixed
           space.
        *)
        let env = Envi.open_module env in
        (* TODO: This name should be constant, and the underlying module
           substituted.
        *)
        let f_name = map_loc ~f:(Ident.create ~mode) f_name in
        let env =
          match f_instance with
          | Envi.Scope.Immediate f ->
              Envi.add_module f_name.txt f env
          | Envi.Scope.Deferred path ->
              Envi.add_deferred_module f_name.txt path env
        in
        (* TODO: check that f_instance matches f' *)
        let env = Envi.open_module env in
        let env, m' = check_module_expr env m in
        let m, _env = Envi.pop_module ~loc env in
        (m, m')
      in
      (* Check that f builds the functor as expected. *)
      let _, m = ftor f' in
      let env =
        Envi.push_scope (Envi.make_functor ~mode (fun f -> fst (ftor f))) env
      in
      (env, {m with mod_desc= Tmod_functor (f_name, f, m)})

let check_signature env signature = check_signature env signature

let check (ast : statement list) (env : Envi.t) =
  List.fold_map ast ~init:env ~f:check_statement

(* Error handling *)

open Format

let pp_typ = ref (fun _fmt _typ -> failwith "Typechecked.pp_typ uninitialised")

let rec report_error ppf = function
  | Check_failed (typ, constr_typ, err) ->
      fprintf ppf
        "@[<v>@[<hov>Incompatible types@ @[<h>%a@] and@ @[<h>%a@]:@]@;%a@]"
        !pp_typ typ !pp_typ constr_typ report_error err
  | Cannot_unify (typ, constr_typ) ->
      fprintf ppf "@[<hov>Cannot unify@ @[<h>%a@] and@ @[<h>%a@]@]" !pp_typ
        (*typ_debug_print*) typ !pp_typ (*typ_debug_print*) constr_typ
  | Recursive_variable typ ->
      fprintf ppf
        "@[<hov>The variable@ @[<h>%a@] would have an instance that contains \
         itself.@]"
        !pp_typ typ
  | Unbound (kind, value) ->
      fprintf ppf "@[<hov>Unbound %s@ %a.@]" kind Longident.pp value.txt
  | Unbound_value value ->
      fprintf ppf "Unbound value %s." value.txt
  | Repeated_pattern_variable name ->
      fprintf ppf
        "@[<hov>Variable@ %s@ is@ bound@ several@ times@ in@ this@ matching.@]"
        name
  | Variable_on_one_side name ->
      fprintf ppf
        "@[<hov>Variable@ %s@ must@ occur@ on@ both@ sides@ of@ this@ '|'@ \
         pattern.@]"
        name
  | Pattern_declaration (kind, name) ->
      fprintf ppf "@[<hov>Unexpected %s declaration for %s within a pattern.@]"
        kind name
  | Empty_record ->
      fprintf ppf "Unexpected empty record."
  | Wrong_record_field (field, typ) ->
      fprintf ppf
        "@[<hov>This record expression is expected to have type@ \
         @[<h>%a@]@;The field %a does not belong to type@ @[<h>%a@].@]"
        !pp_typ typ Longident.pp field !pp_typ typ
  | Repeated_field field ->
      fprintf ppf "@[<hov>The record field %a is defined several times.@]"
        Ident.pprint field
  | Missing_fields fields ->
      fprintf ppf "@[<hov>Some record fields are undefined:@ %a@]"
        (pp_print_list ~pp_sep:pp_print_space Ident.pprint)
        fields
  | Wrong_type_description (kind, name) ->
      fprintf ppf
        "@[<hov>Internal error: Expected a type declaration of kind %s, but \
         instead got %s@]"
        kind name.txt
  | Unifiable_expr ->
      fprintf ppf "Internal error: Unexpected implicit variable."
  | No_unifiable_expr ->
      fprintf ppf "Internal error: Expected an unresolved implicit variable."
  | No_instance typ ->
      fprintf ppf
        "@[<hov>Could not find an instance for an implicit variable of type@ \
         @[<h>%a@].@]"
        !pp_typ typ
  | Argument_expected lid ->
      fprintf ppf "@[<hov>The constructor %a expects an argument.@]"
        Longident.pp lid
  | Not_extensible lid ->
      fprintf ppf "@[<hov>Type definition %a is not extensible.@]" Longident.pp
        lid
  | Extension_different_arity lid ->
      fprintf ppf
        "@[<hov>This extension does not match the definition of type %a@;They \
         have different arities.@]"
        Longident.pp lid
  | Convert_failed (typ, err) ->
      fprintf ppf "@[<v>@[<hov>Could not find a conversion@ @[<h>%a@]:@]@;%a@]"
        !pp_typ typ report_error err
  | Cannot_create_conversion typ ->
      fprintf ppf "@[<hov>@[<h>%a@] and@ @[<h>%a@]@ are not convertible.@]"
        !pp_typ typ !pp_typ typ.type_alternate
  | Convertible_not_in_checked ->
      fprintf ppf "Cannot create a convertible type in a Prover block."
  | Type_modes_mismatch (typ1, typ2) ->
      fprintf ppf
        "@[<hov>Internal error: the modes of these types do not \
         match:@;%a@;%a@]"
        !pp_typ typ1 !pp_typ typ2
  | Missing_row_constructor (path, typ, constr_typ) ->
      fprintf ppf
        "@[<hov>Cannot unify@ @[<h>%a@] and@ @[<h>%a@]:@ the constructor %a \
         is not present in both.@]"
        !pp_typ typ !pp_typ constr_typ Path.pp path
  | Empty_resulting_row (typ, constr_typ) ->
      fprintf ppf
        "@[<hov>Cannot unify@ @[<h>%a@] and@ @[<h>%a@]:@ the resulting row \
         would be empty.@]"
        !pp_typ typ !pp_typ constr_typ
  | Row_different_arity (path, typ, constr_typ) ->
      fprintf ppf
        "@[<hov>Cannot unify@ @[<h>%a@] and@ @[<h>%a@]:@ the constructor %a \
         has different arities.@]"
        !pp_typ typ !pp_typ constr_typ Path.pp path

let () =
  Location.register_error_of_exn (function
    | Error (loc, env, err) ->
        let snap = Type1.Snapshot.create () in
        let set_var_names = Envi.set_var_names env in
        (pp_typ :=
           fun fmt typ ->
             Typeprint.type_expr fmt
               (set_var_names.type_expr set_var_names typ)) ;
        let err_msg = Location.error_of_printer ~loc report_error err in
        (pp_typ := fun _fmt _typ -> failwith "Typechecked.pp_typ uninitialised") ;
        Type1.backtrack snap ; Some err_msg
    | _ ->
        None )
