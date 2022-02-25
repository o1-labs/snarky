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

let with_label ~local ~loc exprs =
  let with_label_expr =
    if local then [%expr with_label]
    else [%expr Snarky_backendless.Checked.with_label]
  in
  Exp.apply ~loc with_label_expr exprs

let with_label_one ~local ~loc ~path:_ expr =
  with_label ~local ~loc [ (Nolabel, located_label_expr expr) ]

let rec snarkydef_inject ~local ~loc ~name expr =
  match expr.pexp_desc with
  | Pexp_fun (lbl, default, pat, body) ->
      { expr with
        pexp_desc =
          Pexp_fun (lbl, default, pat, snarkydef_inject ~local ~loc ~name body)
      }
  | Pexp_newtype (typname, body) ->
      { expr with
        pexp_desc =
          Pexp_newtype (typname, snarkydef_inject ~local ~loc ~name body)
      }
  | Pexp_function _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "%%snarkydef currently doesn't support 'function'"
  | _ ->
      with_label ~local ~loc
        [ (Nolabel, located_label_string ~loc name); (Nolabel, expr) ]

let snarkydef ~local ~loc ~path:_ name expr =
  [%stri
    let [%p Pat.var ~loc (Located.mk ~loc name)] =
      [%e snarkydef_inject ~local ~loc ~name expr]]

let with_label_ext ~local name =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (with_label_one ~local)

let snarkydef_ext ~local name =
  Extension.declare name Extension.Context.structure_item
    Ast_pattern.(
      pstr
        ( pstr_value nonrecursive
            (value_binding ~pat:(ppat_var __) ~expr:__ ^:: nil)
        ^:: nil ))
    (snarkydef ~local)

let main () =
  Driver.register_transformation name
    ~rules:
      [ Context_free.Rule.extension (with_label_ext ~local:false "with_label")
      ; Context_free.Rule.extension (with_label_ext ~local:true "with_label_")
      ; Context_free.Rule.extension (snarkydef_ext ~local:false "snarkydef")
      ; Context_free.Rule.extension (snarkydef_ext ~local:true "snarkydef_")
      ]
