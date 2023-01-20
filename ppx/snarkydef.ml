open Ppxlib
open Ast_helper
open Ast_builder.Default
open Asttypes

let name = "snarkydef"

let located_label_expr expr =
  let loc = expr.pexp_loc in
  [%expr Stdlib.( ^ ) [%e expr] (Stdlib.( ^ ) ": " Stdlib.__LOC__)]

let located_label_string ~loc str =
  [%expr
    Stdlib.( ^ )
      [%e Exp.constant ~loc (Const.string (str ^ ": "))]
      Stdlib.__LOC__]

let with_label ~local ~loc ~arg exprs =
  let with_label_expr =
    if local then [%expr with_label]
    else
      match arg with
      | None ->
          failwith
            "use `snarkydef_` if you have access to with_label locally, \
             otherwise specify the correct path (e.g. `let%snarkdef.Tick ...`)"
      | Some path ->
          pexp_ident ~loc
            (Located.mk ~loc:path.loc (Longident.Ldot (path.txt, "with_label")))
  in
  Exp.apply ~loc with_label_expr exprs

let with_label_one ~local ~loc ~path:_ ~arg expr =
  with_label ~local ~loc ~arg [ (Nolabel, located_label_expr expr) ]

let rec snarkydef_inject ~local ~loc ~arg ~name expr =
  match expr.pexp_desc with
  | Pexp_fun (lbl, default, pat, body) ->
      { expr with
        pexp_desc =
          Pexp_fun
            (lbl, default, pat, snarkydef_inject ~local ~loc ~arg ~name body)
      }
  | Pexp_newtype (typname, body) ->
      { expr with
        pexp_desc =
          Pexp_newtype (typname, snarkydef_inject ~local ~loc ~arg ~name body)
      }
  | Pexp_function _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "%%snarkydef currently doesn't support 'function'"
  | _ ->
      with_label ~local ~loc ~arg
        [ (Nolabel, located_label_string ~loc name)
        ; (Nolabel, [%expr fun () -> [%e expr]])
        ]

let snarkydef ~local ~loc ~path:_ ~arg name expr =
  [%stri
    let [%p Pat.var ~loc (Located.mk ~loc name)] =
      [%e snarkydef_inject ~local ~loc ~arg ~name expr]]

let with_label_ext name =
  Extension.declare_with_path_arg name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (with_label_one ~local:false)

let with_label_local_ext name =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (with_label_one ~local:true ~arg:None)

let snarkydef_ext name =
  Extension.declare_with_path_arg name Extension.Context.structure_item
    Ast_pattern.(
      pstr
        ( pstr_value nonrecursive
            (value_binding ~pat:(ppat_var __) ~expr:__ ^:: nil)
        ^:: nil ))
    (snarkydef ~local:false)

let snarkydef_local_ext name =
  Extension.declare name Extension.Context.structure_item
    Ast_pattern.(
      pstr
        ( pstr_value nonrecursive
            (value_binding ~pat:(ppat_var __) ~expr:__ ^:: nil)
        ^:: nil ))
    (snarkydef ~local:true ~arg:None)

let main () =
  Driver.register_transformation name
    ~rules:
      [ Context_free.Rule.extension (with_label_ext "with_label")
      ; Context_free.Rule.extension (with_label_local_ext "with_label_")
      ; Context_free.Rule.extension (snarkydef_ext "snarkydef")
      ; Context_free.Rule.extension (snarkydef_local_ext "snarkydef_")
      ]
