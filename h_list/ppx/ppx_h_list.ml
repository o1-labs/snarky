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

let mk_lid name = Loc.make ~loc:name.loc (Longident.Lident name.txt)

let constr_of_decl ~loc decl =
  Ast_builder.Default.ptyp_constr ~loc (mk_lid decl.ptype_name)
    (List.map ~f:fst decl.ptype_params)

let fields_arrow ~loc l =
  List.fold_right ~init:[%type: unit] l ~f:(fun {pld_type; _} codomain ->
      [%type: [%t pld_type] -> [%t codomain]] )

module To_hlist = struct
  let deriver_name = "to_hlist"

  let str_decl ~loc (decl : type_declaration) : structure_item =
    let open Ast_builder.Default in
    match decl with
    | {ptype_kind= Ptype_record fields; ptype_name= name; _} ->
        [%stri
          let ([%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] :
                   [%t constr_of_decl ~loc decl]
                -> (unit, [%t fields_arrow ~loc fields]) H_list.t) =
           fun [%p
                 ppat_record ~loc
                   (List.map fields ~f:(fun {pld_name= name; _} ->
                        (mk_lid name, pvar ~loc:name.loc name.txt) ))
                   Closed] ->
            [%e
              List.fold_right fields ~init:[%expr []]
                ~f:(fun {pld_name= name; _} tl ->
                  [%expr [%e evar ~loc:name.loc name.txt] :: [%e tl]] )]]
    | {ptype_loc= loc; _} ->
        Location.raise_errorf ~loc "Cannot derive %s for this type"
          deriver_name

  let sig_decl ~loc (decl : type_declaration) : signature_item =
    let open Ast_builder.Default in
    match decl with
    | {ptype_kind= Ptype_record fields; ptype_name= name; _} ->
        psig_value ~loc
        @@ value_description ~loc ~prim:[]
             ~name:(Located.mk ~loc (mangle ~suffix:deriver_name name.txt))
             ~type_:
               [%type:
                    [%t constr_of_decl ~loc decl]
                 -> (unit, [%t fields_arrow ~loc fields]) H_list.t]
    | {ptype_loc= loc; _} ->
        Location.raise_errorf ~loc "Cannot derive %s for this type"
          deriver_name

  let str_type_decl ~loc ~path:_ (_rec_flag, decls) : structure =
    List.map ~f:(str_decl ~loc) decls

  let sig_type_decl ~loc ~path:_ (_rec_flag, decls) : signature =
    List.map ~f:(sig_decl ~loc) decls

  let deriver =
    Deriving.add
      ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
      ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
      deriver_name
end

module Of_hlist = struct
  let deriver_name = "of_hlist"

  let str_decl ~loc (decl : type_declaration) : structure_item =
    let open Ast_builder.Default in
    match decl with
    | {ptype_kind= Ptype_record fields; ptype_name= name; _} ->
        [%stri
          let ([%p pvar ~loc (mangle ~suffix:deriver_name name.txt)] :
                   (unit, [%t fields_arrow ~loc fields]) H_list.t
                -> [%t constr_of_decl ~loc decl]) =
           fun [%p
                 List.fold_right fields
                   ~init:[%pat? []]
                   ~f:(fun {pld_name= name; _} tl ->
                     [%pat? [%p pvar ~loc:name.loc name.txt] :: [%p tl]] )] ->
            [%e
              pexp_record ~loc
                (List.map fields ~f:(fun {pld_name= name; _} ->
                     (mk_lid name, evar ~loc:name.loc name.txt) ))
                None]]
    | {ptype_loc= loc; _} ->
        Location.raise_errorf ~loc "Cannot derive %s for this type"
          deriver_name

  let sig_decl ~loc (decl : type_declaration) : signature_item =
    let open Ast_builder.Default in
    match decl with
    | {ptype_kind= Ptype_record fields; ptype_name= name; _} ->
        psig_value ~loc
        @@ value_description ~loc ~prim:[]
             ~name:(Located.mk ~loc (mangle ~suffix:deriver_name name.txt))
             ~type_:
               [%type:
                    (unit, [%t fields_arrow ~loc fields]) H_list.t
                 -> [%t constr_of_decl ~loc decl]]
    | {ptype_loc= loc; _} ->
        Location.raise_errorf ~loc "Cannot derive %s for this type"
          deriver_name

  let str_type_decl ~loc ~path:_ (_rec_flag, decls) : structure =
    List.map ~f:(str_decl ~loc) decls

  let sig_type_decl ~loc ~path:_ (_rec_flag, decls) : signature =
    List.map ~f:(sig_decl ~loc) decls

  let deriver =
    Deriving.add
      ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
      ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
      deriver_name
end

let str_type_decl ~loc ~path:_ (_rec_flag, decls) : structure =
  List.concat_map decls ~f:(fun decl ->
      [To_hlist.str_decl ~loc decl; Of_hlist.str_decl ~loc decl] )

let sig_type_decl ~loc ~path:_ (_rec_flag, decls) : signature =
  List.concat_map decls ~f:(fun decl ->
      [To_hlist.sig_decl ~loc decl; Of_hlist.sig_decl ~loc decl] )

let deriver =
  Deriving.add
    ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
    ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
    "hlist"
