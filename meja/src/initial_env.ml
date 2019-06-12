(** The default initial environment. *)
open Typet

open TypeDecl

(** The built-in types. These match the OCaml built-ins. *)
module TypeDecls = struct
  open Ast_build.Type
  open Ast_build.Type_decl

  let int = abstract "int"

  let unit = variant "unit" [Ctor.with_args "()" []]

  let bool =
    variant "bool" [Ctor.with_args "true" []; Ctor.with_args "false" []]

  let char = abstract "char"

  let string = abstract "string"

  let float = abstract "float"

  let exn = open_ "exn"

  let option =
    variant "option" ~params:[var "a"]
      [Ctor.with_args "None" []; Ctor.with_args "Some" [var "a"]]

  let list =
    variant "list" ~params:[var "a"]
      [ Ctor.with_args "[]" []
      ; Ctor.with_args "::" [var "a"; constr (Lident "list") ~params:[var "a"]]
      ]

  let bytes = abstract "bytes"

  let int32 = abstract "int32"

  let int64 = abstract "int64"

  let nativeint = abstract "nativeint"

  let lazy_t = abstract "lazy_t" ~params:[var "a"]

  let array = abstract "array" ~params:[var "a"]

  (** Meja-specific built-ins. *)

  let field = unfold "field" (var ~explicit:Implicit "field")
end

(** Empty environment. *)
let env = Envi.(empty empty_resolve_env)

open TypeDecls

(** Import the built-in type definitions, overriding the previous definition of
    the environment [env] each time.
*)

let int, env = import Checked int env

let unit, env = import Checked unit env

let bool, env = import Checked bool env

let char, env = import Checked char env

let string, env = import Checked string env

let float, env = import Checked float env

let exn, env = import Checked exn env

let option, env = import Checked option env

let list, env = import Checked list env

let bytes, env = import Checked bytes env

let int32, env = import Checked int32 env

let int64, env = import Checked int64 env

let nativeint, env = import Checked nativeint env

let field, env = import Checked field env

let lazy_t, env = import Checked lazy_t env

let array, env = import Checked array env

(** Canonical references for each of the built-in types that the typechecker
    refers to.
*)
module Type = struct
  open Envi

  let int = TypeDecl.mk_typ Checked int ~params:[] env

  let unit = TypeDecl.mk_typ Checked unit ~params:[] env

  let bool = TypeDecl.mk_typ Checked bool ~params:[] env

  let char = TypeDecl.mk_typ Checked char ~params:[] env

  let string = TypeDecl.mk_typ Checked string ~params:[] env

  let float = TypeDecl.mk_typ Checked float ~params:[] env

  let exn = TypeDecl.mk_typ Checked exn ~params:[] env

  let option a = TypeDecl.mk_typ Checked option ~params:[a] env

  let list a = TypeDecl.mk_typ Checked list ~params:[a] env
end
