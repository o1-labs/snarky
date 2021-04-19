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
          [%expr fun (_ : int) -> [%e acc]]
      | Ptyp_var name ->
          [%expr
            fun [%p pvar ~loc ("var_" ^ mangle ~suffix:deriver_name name)] ->
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
            evar ~loc ("var_" ^ mangle ~suffix:deriver_name name)
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
    | {ptype_kind= Ptype_record fields; ptype_name= name; _} ->
        [%stri
          let [%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] =
            [%e
              let body =
                match fields with
                | [] ->
                    raise_errorf ~loc ~deriver_name
                      "Malformed AST: record with no fields"
                | field :: fields ->
                    let field_expr {pld_attributes; pld_type; _} =
                      match
                        get_expr_attribute ~deriver_name pld_attributes
                      with
                      | Some e ->
                          e
                      | None ->
                          of_type pld_type
                    in
                    List.fold ~init:(field_expr field) fields
                      ~f:(fun acc field ->
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

  let deriver =
    Deriving.add
      ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
      ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
      deriver_name
end