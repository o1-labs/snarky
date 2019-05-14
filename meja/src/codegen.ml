open Core_kernel
open Parsetypes

let poly_name name = match name with "t" -> "poly" | name -> name ^ "_poly"

let var_type_name name = match name with "t" -> "var" | name -> name ^ "_var"

let rec var_type_lident =
  Longident.(
    function
    | Lident name ->
        Lident (var_type_name name)
    | Ldot (lid, name) ->
        Ldot (lid, var_type_name name)
    | Lapply (lid1, lid2) ->
        Lapply (lid1, var_type_lident lid2))

let var_type_lident lid =
  Ast_build.(
    Longident.(
      match lid with
      | Lident "string" | Lident "int" ->
          failwith "Native type isn't snarkable"
      | Lident "bool" ->
          Lid.of_list ["Boolean"; "var"]
      | Lident "field" | Ldot (Lident "Field", "t") ->
          Lid.of_list ["Field"; "Var"; "t"]
      | _ ->
          var_type_lident lid))

let typ_name name = match name with "t" -> "typ" | name -> name ^ "_typ"

let typ_of_decl env decl =
  let loc = decl.tdec_loc in
  let name = decl.tdec_ident.txt in
  try
    match decl.tdec_desc with
    | TRecord fields ->
        let decl = {decl with tdec_implicit_params= []} in
        let constr_map = ref (Map.empty (module Int)) in
        let vars_map =
          ref
            (Set.of_list
               (module String)
               (List.map decl.tdec_params ~f:(fun param ->
                    match param.type_desc with
                    | Tvar (Some name, _, _) ->
                        name.txt
                    | _ ->
                        "" )))
        in
        let next_num = ref 1 in
        let rec next_var_name () =
          let name = sprintf "a%i" !next_num in
          if Set.mem !vars_map name then next_var_name ()
          else (
            incr next_num ;
            vars_map := Set.add !vars_map name ;
            name )
        in
        let poly_name = poly_name name in
        let poly_decl, poly_decl_content =
          let poly_decl_fields =
            List.map fields ~f:(fun field ->
                assert (
                  not
                    (Envi.Type.is_arrow (Envi.Type.flatten field.fld_type env))
                ) ;
                let typ =
                  Envi.Type.constr_map env field.fld_type ~f:(fun variant ->
                      if Int.equal variant.var_decl_id decl.tdec_id then
                        Tctor
                          { variant with
                            var_ident=
                              Location.mkloc (Longident.Lident poly_name)
                                variant.var_ident.loc }
                      else
                        let var_name =
                          match Map.find !constr_map variant.var_decl_id with
                          | Some (name, _) ->
                              name
                          | None ->
                              let name = next_var_name () in
                              constr_map :=
                                Map.add_exn !constr_map
                                  ~key:variant.var_decl_id ~data:(name, variant) ;
                              name
                        in
                        Tvar (Some (Location.mkloc var_name loc), -1, Explicit)
                  )
                in
                {field with fld_type= typ} )
          in
          let params =
            Map.fold !constr_map ~init:[] ~f:(fun ~key:_ ~data:(name, _) l ->
                Ast_build.Type.var ~loc name :: l )
          in
          let poly_decl_content =
            { decl with
              tdec_ident= Location.mkloc poly_name loc
            ; tdec_desc= TRecord poly_decl_fields
            ; tdec_params= decl.tdec_params @ params }
          in
          ( {stmt_loc= loc; stmt_desc= TypeDecl poly_decl_content}
          , poly_decl_content )
        in
        let mk_decl params =
          { stmt_loc= loc
          ; stmt_desc=
              TypeDecl
                { decl with
                  tdec_desc=
                    TAlias
                      (Envi.TypeDecl.mk_typ
                         ~params:(decl.tdec_params @ params)
                         poly_decl_content env) } }
        in
        let t_decl =
          let params =
            Map.fold !constr_map ~init:[]
              ~f:(fun ~key:_ ~data:(_, variant) l ->
                Envi.Type.mk (Tctor variant) env :: l )
          in
          mk_decl params
        in
        let var_decl =
          let params =
            Map.fold !constr_map ~init:[]
              ~f:(fun ~key:_ ~data:(_, variant) l ->
                let name = variant.var_ident in
                let variant =
                  { variant with
                    var_ident=
                      Location.mkloc (var_type_lident name.txt) name.loc }
                in
                Envi.Type.mk (Tctor variant) env :: l )
          in
          mk_decl params
        in
        let typ_instance =
          let typ_body =
            let open Ast_build in
            let bind_over ~run ~bind ~result =
              let bindings =
                List.fold ~init:result fields ~f:(fun result {fld_ident; _} ->
                    Exp.apply ~loc bind
                      [ ( Nolabel
                        , run (Exp.var ~loc (Lid.of_name fld_ident.txt)) )
                      ; ( Nolabel
                        , Exp.fun_ ~loc (Pat.var ~loc fld_ident.txt) result )
                      ] )
              in
              Exp.fun_ ~loc
                (Pat.record ~loc
                   (List.map fields ~f:(fun {fld_ident; _} ->
                        (mk_lid fld_ident, Pat.var ~loc fld_ident.txt) )))
                bindings
            in
            let var_of_list l = Exp.var ~loc (Lid.of_list l) in
            let apply_var_of_list l x =
              Exp.apply ~loc (var_of_list l) [(Nolabel, x)]
            in
            let result =
              Exp.record ~loc
                (List.map fields ~f:(fun {fld_ident; _} ->
                     ( mk_lid fld_ident
                     , Exp.var ~loc (Lid.of_name fld_ident.txt) ) ))
            in
            let store =
              bind_over
                ~result:(apply_var_of_list ["Typ"; "Store"; "return"] result)
                ~bind:(var_of_list ["Typ"; "Store"; "bind"])
                ~run:(apply_var_of_list ["Typ"; "store"])
            in
            let read =
              bind_over
                ~result:(apply_var_of_list ["Typ"; "Read"; "return"] result)
                ~bind:(var_of_list ["Typ"; "Read"; "bind"])
                ~run:(apply_var_of_list ["Snarky"; "read"])
            in
            let alloc =
              bind_over
                ~result:(apply_var_of_list ["Typ"; "Alloc"; "return"] result)
                ~bind:(var_of_list ["Typ"; "Alloc"; "bind"])
                ~run:(fun _ -> var_of_list ["Typ"; "alloc"])
            in
            let check =
              bind_over
                ~result:(Exp.ctor ~loc (Lid.of_name "()"))
                ~bind:
                  (Exp.fun_ (Pat.var ~loc "f")
                     (Exp.fun_ (Pat.var ~loc "x")
                        (Exp.apply
                           (Exp.var ~loc (Lid.of_name "f"))
                           [(Nolabel, Exp.var ~loc (Lid.of_name "x"))])))
                ~run:(apply_var_of_list ["Typ"; "check"])
            in
            let body =
              Exp.record ~loc
                [ (Loc.mk ~loc (Lid.of_list ["Typ"; "store"]), store)
                ; (Loc.mk ~loc (Lid.of_list ["Typ"; "read"]), read)
                ; (Loc.mk ~loc (Lid.of_list ["Typ"; "alloc"]), alloc)
                ; (Loc.mk ~loc (Lid.of_list ["Typ"; "check"]), check) ]
            in
            let value_type =
              Type.constr ~loc
                ~params:
                  (List.map decl.tdec_params ~f:(fun _ -> Type.none ~loc ()))
                (Lid.of_name decl.tdec_ident.txt)
            in
            let target_type =
              Type.constr ~loc
                ~params:[Type.none ~loc (); value_type]
                (Lid.of_list ["Typ"; "t"])
            in
            Exp.constraint_ ~loc body target_type
          in
          { stmt_loc= loc
          ; stmt_desc= Instance (Location.mkloc (typ_name name) loc, typ_body)
          }
        in
        if Map.is_empty !constr_map then
          Some [{stmt_loc= loc; stmt_desc= TypeDecl decl}; typ_instance]
        else Some [poly_decl; t_decl; var_decl; typ_instance]
    | _ ->
        None
  with _ -> None
