open Base
open Ppxlib

let mangle ~suffix name =
  if String.equal name "t" then suffix else name ^ "_" ^ suffix

let mangle_lid ~suffix lid =
  match lid with
  | Lident name ->
      Lident (mangle ~suffix name)
  | Ldot (lid, name) ->
      Ldot (lid, mangle ~suffix name)
  | Lapply _ ->
      assert false

(** Generates an expression that resolves to the given Lident. If there are
    [Lapply]s, generates the appropriate functor applications to extract the
    target value.
*)
let expr_of_lid ?(base_module_name = "M____") ~loc lid =
  let rec lid_and_bindings lid count =
    match lid with
    | Lident _ ->
        (lid, count, [])
    | Ldot (lid, name) ->
        let lid, count, module_bindings = lid_and_bindings lid count in
        (Ldot (lid, name), count, module_bindings)
    | Lapply (lid1, lid2) ->
        let lid1, count, module_bindings1 = lid_and_bindings lid1 count in
        let lid2, count, module_bindings2 = lid_and_bindings lid2 count in
        let name = Stdlib.Format.sprintf "%s%i" base_module_name (count + 1) in
        ( Lident name
        , count + 1
        , (name, Lapply (lid1, lid2)) :: (module_bindings1 @ module_bindings2)
        )
  in
  let lid, _count, module_bindings = lid_and_bindings lid 0 in
  let open Ast_builder.Default in
  List.fold_left
    ~init:(pexp_ident ~loc (Located.mk ~loc lid))
    module_bindings
    ~f:(fun exp (name, lid) ->
      pexp_letmodule ~loc (Located.mk ~loc name)
        (pmod_ident ~loc (Located.mk ~loc lid))
        exp )

let mk_lid name = Loc.make ~loc:name.loc (Longident.Lident name.txt)

let raise_errorf ~loc ~deriver_name fmt =
  let fmt =
    match fmt with
    | CamlinternalFormatBasics.Format (fmt, str) ->
        CamlinternalFormatBasics.Format
          (String (No_padding, String_literal (": ", fmt)), "%s: " ^ str)
  in
  Location.raise_errorf ~loc fmt deriver_name

let get_expr_attribute ~deriver_name attributes =
  List.find_map attributes ~f:(fun (name, payload) ->
      if String.equal name.txt deriver_name then
        match payload with
        | PStr [{pstr_desc= Pstr_eval (e, _); _}] ->
            Some e
        | _ ->
            raise_errorf ~loc:name.loc ~deriver_name
              "Expected a single expression as the argument to %s annotation"
              deriver_name deriver_name
      else None )

let params_wrap ~loc ~deriver_name (decl : type_declaration) body =
  let open Ast_builder.Default in
  List.fold_right ~init:body decl.ptype_params ~f:(fun (typ, _) acc ->
      match typ.ptyp_desc with
      | Ptyp_any ->
          [%expr fun _ -> [%e acc]]
      | Ptyp_var name ->
          [%expr
            fun [%p pvar ~loc ("_var_" ^ mangle ~suffix:deriver_name name)] ->
              [%e acc]]
      | _ ->
          raise_errorf ~deriver_name ~loc:typ.ptyp_loc
            "Don't know how to build an expression for@,%a" Pprintast.core_type
            typ )

module Size_in_field_elements = struct
  let deriver_name = "size_in_field_elements"

  let rec of_type ?loc (typ : core_type) : expression =
    let open Ast_builder.Default in
    match get_expr_attribute ~deriver_name typ.ptyp_attributes with
    | Some e ->
        e
    | None -> (
        let loc = match loc with Some loc -> loc | None -> typ.ptyp_loc in
        match typ.ptyp_desc with
        | Ptyp_any
        | Ptyp_arrow _
        | Ptyp_object _
        | Ptyp_class _
        | Ptyp_alias _
        | Ptyp_variant _
        | Ptyp_poly _
        | Ptyp_package _
        | Ptyp_extension _ ->
            raise_errorf ~deriver_name ~loc:typ.ptyp_loc
              "Don't know how to build an expression for@,%a"
              Pprintast.core_type typ
        | Ptyp_var name ->
            evar ~loc ("_var_" ^ mangle ~suffix:deriver_name name)
        | Ptyp_tuple typs ->
            List.fold_left ~init:[%expr 0] typs ~f:(fun acc typ ->
                [%expr Stdlib.( + ) [%e acc] [%e of_type typ]] )
        | Ptyp_constr (lid, []) ->
            expr_of_lid ~loc (mangle_lid ~suffix:deriver_name lid.txt)
        | Ptyp_constr (lid, args) ->
            pexp_apply ~loc
              (expr_of_lid ~loc:lid.loc
                 (mangle_lid ~suffix:deriver_name lid.txt))
              (List.map ~f:(fun typ -> (Asttypes.Nolabel, of_type typ)) args) )

  let str_decl ?loc (decl : type_declaration) : structure_item =
    let open Ast_builder.Default in
    let loc = match loc with Some loc -> loc | None -> decl.ptype_loc in
    match decl with
    | {ptype_kind= Ptype_record (field :: fields); ptype_name= name; _} ->
        [%stri
          let [%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] =
            [%e
              let body =
                let field_expr {pld_attributes; pld_type; _} =
                  match get_expr_attribute ~deriver_name pld_attributes with
                  | Some e ->
                      e
                  | None ->
                      of_type pld_type
                in
                List.fold ~init:(field_expr field) fields ~f:(fun acc field ->
                    [%expr Stdlib.( + ) [%e acc] [%e field_expr field]] )
              in
              params_wrap ~loc ~deriver_name decl body]]
    | {ptype_manifest= Some typ; ptype_name= name; _} ->
        [%stri
          let [%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] =
            [%e params_wrap ~loc ~deriver_name decl (of_type typ)]]
    | _ ->
        raise_errorf ~deriver_name ~loc:decl.ptype_loc
          "Don't know how to build an expression for %s" decl.ptype_name.txt

  let sig_decl ?loc (decl : type_declaration) : signature_item =
    let open Ast_builder.Default in
    let loc = match loc with Some loc -> loc | None -> decl.ptype_loc in
    match decl with
    | {ptype_params; ptype_name; _} ->
        psig_value ~loc
        @@ value_description ~loc ~prim:[]
             ~name:
               (Located.mk ~loc:ptype_name.loc
                  (mangle ~suffix:deriver_name ptype_name.txt))
             ~type_:
               (List.fold_left ~init:[%type: int] ptype_params ~f:(fun acc _ ->
                    [%type: int -> [%t acc]] ))

  let str_type_decl ~loc ~path:_ (_rec_flag, decls) : structure =
    List.map decls ~f:(fun decl -> str_decl ~loc decl)

  let sig_type_decl ~loc ~path:_ (_rec_flag, decls) : signature =
    List.map decls ~f:(fun decl -> sig_decl ~loc decl)

  let extension ~loc ~path:_ ty = of_type ~loc ty

  let deriver =
    Deriving.add deriver_name
      ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
      ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
      ~extension
end

module To_field_elements = struct
  let deriver_name = "to_field_elements"

  let rec of_type ?loc (typ : core_type) : expression =
    let open Ast_builder.Default in
    match get_expr_attribute ~deriver_name typ.ptyp_attributes with
    | Some e ->
        e
    | None -> (
        let loc = match loc with Some loc -> loc | None -> typ.ptyp_loc in
        match typ.ptyp_desc with
        | Ptyp_any
        | Ptyp_arrow _
        | Ptyp_object _
        | Ptyp_class _
        | Ptyp_alias _
        | Ptyp_variant _
        | Ptyp_poly _
        | Ptyp_package _
        | Ptyp_extension _
        | Ptyp_tuple [] ->
            raise_errorf ~deriver_name ~loc:typ.ptyp_loc
              "Don't know how to build an expression for@,%a"
              Pprintast.core_type typ
        | Ptyp_var name ->
            evar ~loc ("_var_" ^ mangle ~suffix:deriver_name name)
        | Ptyp_tuple (typ :: typs) ->
            [%expr
              fun [%p
                    ppat_tuple ~loc
                    @@ List.mapi (typ :: typs) ~f:(fun i _typ ->
                           pvar ~loc (Stdlib.Format.sprintf "ppx_typ__x_%i" i)
                       )] ->
                [%e
                  List.foldi
                    ~init:[%expr [%e of_type typ] ppx_typ__x_0]
                    typs
                    ~f:(fun i acc typ ->
                      [%expr
                        Stdlib.Array.append [%e acc]
                          ([%e of_type typ]
                             [%e
                               evar ~loc
                                 (Stdlib.Format.sprintf "ppx_typ__x_%i" (i + 1))])]
                      )]]
        | Ptyp_constr (lid, []) ->
            expr_of_lid ~loc (mangle_lid ~suffix:deriver_name lid.txt)
        | Ptyp_constr (lid, args) ->
            pexp_apply ~loc
              (expr_of_lid ~loc:lid.loc
                 (mangle_lid ~suffix:deriver_name lid.txt))
              (List.map ~f:(fun typ -> (Asttypes.Nolabel, of_type typ)) args) )

  let str_decl ?loc (decl : type_declaration) : structure_item =
    let open Ast_builder.Default in
    let loc = match loc with Some loc -> loc | None -> decl.ptype_loc in
    match decl with
    | {ptype_kind= Ptype_record (field :: fields); ptype_name= name; _} ->
        [%stri
          let [%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] =
            [%e
              let body =
                let field_expr {pld_attributes; pld_type; pld_name; _} =
                  [%expr
                    [%e
                      match
                        get_expr_attribute ~deriver_name pld_attributes
                      with
                      | Some e ->
                          e
                      | None ->
                          of_type pld_type]
                      [%e evar ~loc:pld_name.loc pld_name.txt]]
                in
                [%expr
                  fun [%p
                        ppat_record ~loc
                          (List.map (field :: fields) ~f:(fun {pld_name; _} ->
                               ( mk_lid pld_name
                               , pvar ~loc:pld_name.loc pld_name.txt ) ))
                          Closed] ->
                    [%e
                      List.fold ~init:(field_expr field) (List.rev fields)
                        ~f:(fun acc field ->
                          [%expr
                            Stdlib.Array.append [%e acc] [%e field_expr field]]
                      )]]
              in
              params_wrap ~loc ~deriver_name decl body]]
    | {ptype_manifest= Some typ; ptype_name= name; _} ->
        [%stri
          let [%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] =
            [%e params_wrap ~loc ~deriver_name decl (of_type typ)]]
    | _ ->
        raise_errorf ~deriver_name ~loc:decl.ptype_loc
          "Don't know how to build an expression for %s" decl.ptype_name.txt

  let sig_decl ?loc (decl : type_declaration) : signature_item =
    let open Ast_builder.Default in
    let loc = match loc with Some loc -> loc | None -> decl.ptype_loc in
    match decl with
    | {ptype_params; ptype_name; _} ->
        psig_value ~loc
        @@ value_description ~loc ~prim:[]
             ~name:
               (Located.mk ~loc:ptype_name.loc
                  (mangle ~suffix:deriver_name ptype_name.txt))
             ~type_:
               (List.fold_right
                  ~init:
                    [%type:
                         [%t
                           ptyp_constr ~loc (mk_lid ptype_name)
                             (List.map ~f:fst ptype_params)]
                      -> Field.t array] ptype_params ~f:(fun (typ, _) acc ->
                    [%type: ([%t typ] -> Field.t array) -> [%t acc]] ))

  let str_type_decl ~loc ~path:_ (_rec_flag, decls) : structure =
    List.map decls ~f:(fun decl -> str_decl ~loc decl)

  let sig_type_decl ~loc ~path:_ (_rec_flag, decls) : signature =
    List.map decls ~f:(fun decl -> sig_decl ~loc decl)

  let extension ~loc ~path:_ ty = of_type ~loc ty

  let deriver =
    Deriving.add deriver_name
      ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
      ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
      ~extension
end

module Of_field_elements_indexed = struct
  let deriver_name = "of_field_elements_indexed"

  let rec of_type ?loc (typ : core_type) : expression =
    let open Ast_builder.Default in
    match get_expr_attribute ~deriver_name typ.ptyp_attributes with
    | Some e ->
        e
    | None -> (
        let loc = match loc with Some loc -> loc | None -> typ.ptyp_loc in
        match typ.ptyp_desc with
        | Ptyp_any
        | Ptyp_arrow _
        | Ptyp_object _
        | Ptyp_class _
        | Ptyp_alias _
        | Ptyp_variant _
        | Ptyp_poly _
        | Ptyp_package _
        | Ptyp_extension _ ->
            raise_errorf ~deriver_name ~loc:typ.ptyp_loc
              "Don't know how to build an expression for@,%a"
              Pprintast.core_type typ
        | Ptyp_var name ->
            evar ~loc ("_var_" ^ mangle ~suffix:deriver_name name)
        | Ptyp_tuple typs ->
            [%expr
              fun ~offset:ppx_typ__offset ppx_typ__array ->
                [%e
                  let max = ref (-1) in
                  let init =
                    pexp_tuple ~loc
                      (List.mapi typs ~f:(fun i _typ ->
                           max := Int.max !max i ;
                           evar ~loc (Stdlib.Format.sprintf "ppx_typ__x_%i" i)
                       ))
                  in
                  List.fold_right typs ~init ~f:(fun typ acc ->
                      let i = !max in
                      Int.decr max ;
                      [%expr
                        let [%p
                              pvar ~loc
                                (Stdlib.Format.sprintf "ppx_typ__x_%i" i)] =
                          [%e of_type typ] ~offset:ppx_typ__offset
                            ppx_typ__array
                        in
                        [%e acc]] )]]
        | Ptyp_constr (lid, []) ->
            expr_of_lid ~loc (mangle_lid ~suffix:deriver_name lid.txt)
        | Ptyp_constr (lid, args) ->
            pexp_apply ~loc
              (expr_of_lid ~loc:lid.loc
                 (mangle_lid ~suffix:deriver_name lid.txt))
              (List.map ~f:(fun typ -> (Asttypes.Nolabel, of_type typ)) args) )

  let str_decl ?loc (decl : type_declaration) : structure_item =
    let open Ast_builder.Default in
    let loc = match loc with Some loc -> loc | None -> decl.ptype_loc in
    match decl with
    | {ptype_kind= Ptype_record fields; ptype_name= name; _} ->
        [%stri
          let [%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] =
            [%e
              let body =
                let field_expr {pld_attributes; pld_type; _} =
                  [%expr
                    [%e
                      match
                        get_expr_attribute ~deriver_name pld_attributes
                      with
                      | Some e ->
                          e
                      | None ->
                          of_type pld_type]
                      ~offset:ppx_typ__offset ppx_typ__array]
                in
                let init =
                  pexp_record ~loc
                    (List.map fields ~f:(fun {pld_name; _} ->
                         ( mk_lid pld_name
                         , pexp_ident ~loc:pld_name.loc (mk_lid pld_name) ) ))
                    None
                in
                List.fold_right fields ~init ~f:(fun field acc ->
                    [%expr
                      let [%p pvar ~loc:field.pld_name.loc field.pld_name.txt]
                          =
                        [%e field_expr field]
                      in
                      [%e acc]] )
              in
              params_wrap ~loc ~deriver_name decl
                [%expr fun ~offset:ppx_typ__offset ppx_typ__array -> [%e body]]]]
    | {ptype_manifest= Some typ; ptype_name= name; _} ->
        [%stri
          let [%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] =
            [%e params_wrap ~loc ~deriver_name decl (of_type typ)]]
    | _ ->
        raise_errorf ~deriver_name ~loc:decl.ptype_loc
          "Don't know how to build an expression for %s" decl.ptype_name.txt

  let sig_decl ?loc (decl : type_declaration) : signature_item =
    let open Ast_builder.Default in
    let loc = match loc with Some loc -> loc | None -> decl.ptype_loc in
    match decl with
    | {ptype_params; ptype_name; _} ->
        psig_value ~loc
        @@ value_description ~loc ~prim:[]
             ~name:
               (Located.mk ~loc:ptype_name.loc
                  (mangle ~suffix:deriver_name ptype_name.txt))
             ~type_:
               (List.fold_right
                  ~init:
                    [%type:
                         offset:int ref
                      -> Field.t array
                      -> [%t
                           ptyp_constr ~loc (mk_lid ptype_name)
                             (List.map ~f:fst ptype_params)]] ptype_params
                  ~f:(fun (typ, _) acc ->
                    [%type:
                      (offset:int ref -> Field.t array -> [%t typ]) -> [%t acc]]
                ))

  let str_type_decl ~loc ~path:_ (_rec_flag, decls) : structure =
    List.map decls ~f:(fun decl -> str_decl ~loc decl)

  let sig_type_decl ~loc ~path:_ (_rec_flag, decls) : signature =
    List.map decls ~f:(fun decl -> sig_decl ~loc decl)

  let extension ~loc ~path:_ ty = of_type ~loc ty

  let deriver =
    Deriving.add deriver_name
      ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
      ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
      ~extension
end

module Of_field_elements = struct
  let deriver_name = "of_field_elements"

  let str_decl ?loc (decl : type_declaration) : structure_item option =
    let open Ast_builder.Default in
    match decl.ptype_params with
    | [] ->
        let loc = match loc with Some loc -> loc | None -> decl.ptype_loc in
        let name = decl.ptype_name.txt in
        Some
          [%stri
            let [%p pvar ~loc (mangle ~suffix:deriver_name name)] =
              [%e
                params_wrap ~loc ~deriver_name decl
                  [%expr
                    fun ppx_typ__array ->
                      [%e
                        evar ~loc
                          (mangle
                             ~suffix:Of_field_elements_indexed.deriver_name
                             name)]]]]
    | _ ->
        None

  let sig_decl ?loc (decl : type_declaration) : signature_item option =
    let open Ast_builder.Default in
    let loc = match loc with Some loc -> loc | None -> decl.ptype_loc in
    match decl with
    | {ptype_params= []; ptype_name; _} ->
        Some
          ( psig_value ~loc
          @@ value_description ~loc ~prim:[]
               ~name:
                 (Located.mk ~loc:ptype_name.loc
                    (mangle ~suffix:deriver_name ptype_name.txt))
               ~type_:
                 [%type:
                      Field.t array
                   -> [%t ptyp_constr ~loc (mk_lid ptype_name) []]] )
    | _ ->
        None

  let str_type_decl ~loc ~path:_ (_rec_flag, decls) : structure =
    List.filter_map decls ~f:(fun decl -> str_decl ~loc decl)

  let sig_type_decl ~loc ~path:_ (_rec_flag, decls) : signature =
    List.filter_map decls ~f:(fun decl -> sig_decl ~loc decl)

  let extension = Of_field_elements_indexed.extension

  let deriver =
    Deriving.add deriver_name
      ~str_type_decl:
        (Deriving.Generator.make_noarg
           ~deps:
             [Size_in_field_elements.deriver; Of_field_elements_indexed.deriver]
           str_type_decl)
      ~sig_type_decl:
        (Deriving.Generator.make_noarg
           ~deps:
             [Size_in_field_elements.deriver; Of_field_elements_indexed.deriver]
           sig_type_decl)
      ~extension
end

module Snarky_typ = struct
  let deriver_name = "snarky_typ"

  let deriver =
    Deriving.add_alias deriver_name
      [ Of_field_elements.deriver
      ; Of_field_elements_indexed.deriver
      ; To_field_elements.deriver
      ; Size_in_field_elements.deriver ]
end
