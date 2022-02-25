(** The default initial environment. *)
open Typet

open TypeDecl

(** The built-in types. These match the OCaml built-ins. *)
module TypeDecls = struct
  open Ast_build.Type
  open Ast_build.Type_decl

  let int = abstract "int"

  let unit = variant "unit" [ Ctor.with_args "()" [] ]

  let bool =
    variant "bool" [ Ctor.with_args "true" []; Ctor.with_args "false" [] ]

  let char = abstract "char"

  let string = abstract "string"

  let float = abstract "float"

  let exn = open_ "exn"

  let option =
    variant "option" ~params:[ var "a" ]
      [ Ctor.with_args "None" []; Ctor.with_args "Some" [ var "a" ] ]

  let list =
    variant "list" ~params:[ var "a" ]
      [ Ctor.with_args "[]" []
      ; Ctor.with_args "::"
          [ var "a"; constr (Lident "list") ~params:[ var "a" ] ]
      ]

  let bytes = abstract "bytes"

  let int32 = abstract "int32"

  let int64 = abstract "int64"

  let nativeint = abstract "nativeint"

  let lazy_t = abstract "lazy_t" ~params:[ var "a" ]

  let array = abstract "array" ~params:[ var "a" ]

  (** Meja-specific built-ins. *)

  let field = abstract "field"

  let boolean = abstract "boolean"
end

(** Empty environment. *)
let env = Envi.(empty empty_resolve_env)

open TypeDecls

(** Import the built-in type definitions, overriding the previous definition of
    the environment [env] each time.
*)

let { Typedast.tdec_tdec = int; tdec_ident = int_ident; _ }, env =
  import int env

let { Typedast.tdec_tdec = unit; _ }, env = import unit env

let bool, boolean, env =
  match
    import_convertible boolean (Parsetypes.Ptconv_with (Checked, bool)) env
  with
  | boolean, Ttconv_with (Checked, bool), env ->
      (bool.tdec_tdec, boolean.tdec_tdec, env)
  | _ ->
      assert false

let { Typedast.tdec_tdec = char; _ }, env = import char env

let { Typedast.tdec_tdec = string; _ }, env = import string env

let { Typedast.tdec_tdec = float; _ }, env = import float env

let { Typedast.tdec_tdec = exn; _ }, env = import exn env

let { Typedast.tdec_tdec = option; _ }, env = import option env

(* NOTE: list is a recursive type. *)
let { Typedast.tdec_tdec = list; _ }, env =
  let decls, env = import_rec [ list ] env in
  (List.hd decls, env)

let { Typedast.tdec_tdec = bytes; _ }, env = import bytes env

let { Typedast.tdec_tdec = int32; _ }, env = import int32 env

let { Typedast.tdec_tdec = int64; _ }, env = import int64 env

let { Typedast.tdec_tdec = nativeint; _ }, env = import nativeint env

let field, field_var, env =
  match
    import_convertible field (Parsetypes.Ptconv_with (Prover, field)) env
  with
  | field_var, Ttconv_with (Prover, field), env ->
      (field.tdec_tdec, field_var.tdec_tdec, env)
  | _ ->
      assert false

let { Typedast.tdec_tdec = lazy_t; _ }, env = import lazy_t env

let { Typedast.tdec_tdec = array; _ }, env = import array env

(** Canonical references for each of the built-in types that the typechecker
    refers to.
*)
module Type = struct
  open Envi

  let int = TypeDecl.mk_typ ~mode:Checked int ~params:[] env

  let int32 = TypeDecl.mk_typ ~mode:Checked int32 ~params:[] env

  let int64 = TypeDecl.mk_typ ~mode:Checked int64 ~params:[] env

  let nativeint = TypeDecl.mk_typ ~mode:Checked nativeint ~params:[] env

  let unit = TypeDecl.mk_typ ~mode:Checked unit ~params:[] env

  let bool = TypeDecl.mk_typ ~mode:Checked bool ~params:[] env

  let char = TypeDecl.mk_typ ~mode:Checked char ~params:[] env

  let string = TypeDecl.mk_typ ~mode:Checked string ~params:[] env

  let float = TypeDecl.mk_typ ~mode:Checked float ~params:[] env

  let exn = TypeDecl.mk_typ ~mode:Checked exn ~params:[] env

  let option a =
    { (TypeDecl.mk_typ ~mode:a.Type0.type_mode option ~params:[ a ] env) with
      type_depth = a.type_depth
    }

  let list a =
    { (TypeDecl.mk_typ ~mode:a.Type0.type_mode list ~params:[ a ] env) with
      type_depth = a.type_depth
    }

  let boolean = TypeDecl.mk_typ ~mode:Checked boolean ~params:[] env

  let field_var = TypeDecl.mk_typ ~mode:Checked field_var ~params:[] env
end

let env = Envi.open_module env

let () = Type1.mk_option := Type.option
