open Core_kernel
open Ast_types
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

let typ_of_decl ~loc (decl : type_decl) =
  let open Ast_build in
  let name = decl.tdec_ident.txt in
  try
    match decl.tdec_desc with
    | TRecord fields ->
        let vars = ref String.Set.empty in
        let find_name name =
          let rec find_name i =
            let name = sprintf "%s%i" name i in
            if Set.mem !vars name then find_name (i + 1)
            else (
              vars := Set.add !vars name ;
              name )
          in
          if Set.mem !vars name then find_name 1
          else (
            vars := Set.add !vars name ;
            name )
        in
        let poly_name = poly_name name in
        let poly_decl =
          let type_vars =
            List.map fields ~f:(fun {fld_ident; _} ->
                Type.var ~loc (find_name fld_ident.txt) )
          in
          let fields =
            List.map2_exn fields type_vars ~f:(fun field var ->
                {field with fld_type= var; fld_loc= loc} )
          in
          Type_decl.record poly_name ~loc ~params:type_vars fields
        in
        let t_decl =
          let type_vars = List.map fields ~f:(fun {fld_type; _} -> fld_type) in
          Type_decl.alias name ~loc ~params:decl.tdec_params
            ~implicits:decl.tdec_implicit_params
            (Type.constr ~loc ~params:type_vars (Lid.of_name poly_name))
        in
        let var_name = var_type_name name in
        let has_constr = ref false in
        let var_decl =
          let rec change_names typ =
            match typ.type_desc with
            | Ptyp_ctor ({var_ident; _} as variant) ->
                has_constr := true ;
                Typet.Type.map ~loc ~f:change_names
                  { typ with
                    type_desc=
                      Ptyp_ctor
                        { variant with
                          var_ident=
                            Loc.mk ~loc (var_type_lident var_ident.txt) } }
            | Ptyp_arrow _ ->
                (* We don't support generating [Typ.t]s on [_ -> _]. *)
                assert false
            | _ ->
                Typet.Type.map ~loc ~f:change_names typ
          in
          let type_vars =
            List.map fields ~f:(fun {fld_type; _} -> change_names fld_type)
          in
          Type_decl.alias var_name ~loc ~params:decl.tdec_params
            ~implicits:decl.tdec_implicit_params
            (Type.constr ~loc ~params:type_vars (Lid.of_name poly_name))
        in
        let typ_instance =
          let typ_body =
            let open Ast_build in
            let bindings ~run ~bind ~result =
              List.fold ~init:result fields ~f:(fun result {fld_ident; _} ->
                  Exp.apply ~loc bind
                    [ (Nolabel, run (Exp.var ~loc (Lid.of_name fld_ident.txt)))
                    ; ( Nolabel
                      , Exp.fun_ ~loc (Pat.var ~loc fld_ident.txt) result ) ]
              )
            in
            let bind_over ~run ~bind ~result =
              Exp.fun_ ~loc
                (Pat.record ~loc
                   (List.map fields ~f:(fun {fld_ident; _} ->
                        (mk_lid fld_ident, Pat.var ~loc fld_ident.txt) )))
                (bindings ~run ~bind ~result)
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
                ~run:(apply_var_of_list ["Typ"; "read"])
            in
            let alloc =
              bindings
                ~result:(apply_var_of_list ["Typ"; "Alloc"; "return"] result)
                ~bind:(var_of_list ["Typ"; "Alloc"; "bind"])
                ~run:(fun _ -> var_of_list ["Typ"; "alloc"])
            in
            let check =
              Exp.fun_ ~loc
                (Pat.record ~loc
                   (List.map fields ~f:(fun {fld_ident; _} ->
                        (mk_lid fld_ident, Pat.var ~loc fld_ident.txt) )))
                (List.fold
                   ~init:(Exp.ctor ~loc (Lid.of_name "()"))
                   fields
                   ~f:(fun result {fld_ident; _} ->
                     Exp.seq ~loc
                       (apply_var_of_list ["Typ"; "check"]
                          (Exp.var ~loc (Lid.of_name fld_ident.txt)))
                       result ))
            in
            let body =
              Exp.record ~loc
                [ (Loc.mk ~loc (Lid.of_list ["Typ"; "store"]), store)
                ; (Loc.mk ~loc (Lid.of_list ["Typ"; "read"]), read)
                ; (Loc.mk ~loc (Lid.of_list ["Typ"; "alloc"]), alloc)
                ; (Loc.mk ~loc (Lid.of_list ["Typ"; "check"]), check) ]
            in
            let fresh_names =
              List.map ~f:(fun name -> Type.var ~loc (find_name name))
            in
            let new_base_type () =
              let type_vars =
                fresh_names
                  (List.map decl.tdec_params ~f:(fun param ->
                       match param.type_desc with
                       | Ptyp_var (Some name, _) ->
                           name.txt
                       | _ ->
                           "a" ))
              in
              Type.constr ~loc ~params:type_vars (Lid.of_name name)
            in
            let value_type =
              if !has_constr then
                Type.constr ~loc ~params:poly_decl.tdec_params
                  (Lid.of_name poly_name)
              else new_base_type ()
            in
            let var_type =
              if !has_constr then
                let type_vars =
                  fresh_names
                    (List.map fields ~f:(fun {fld_ident; _} -> fld_ident.txt))
                in
                Type.constr ~loc ~params:type_vars (Lid.of_name poly_name)
              else new_base_type ()
            in
            let target_type =
              Type.constr ~loc ~params:[var_type; value_type]
                (Lid.of_list ["Typ"; "t"])
            in
            Exp.constraint_ ~loc body target_type
          in
          Pstmt_instance (Location.mkloc (typ_name name) loc, typ_body)
        in
        let mk_stmt stmt_desc = {stmt_loc= loc; stmt_desc} in
        if !has_constr then
          Some
            [ mk_stmt (Pstmt_type poly_decl)
            ; mk_stmt (Pstmt_type t_decl)
            ; mk_stmt (Pstmt_type var_decl)
            ; mk_stmt typ_instance ]
        else Some [mk_stmt (Pstmt_type decl); mk_stmt typ_instance]
    | _ ->
        None
  with _ -> None
