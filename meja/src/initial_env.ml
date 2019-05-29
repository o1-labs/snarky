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

let int, env = import int env

let unit, env = import unit env

let bool, env = import bool env

let char, env = import char env

let string, env = import string env

let float, env = import float env

let exn, env = import exn env

let option, env = import option env

let list, env = import list env

let bytes, env = import bytes env

let int32, env = import int32 env

let int64, env = import int64 env

let nativeint, env = import nativeint env

let field, env = import field env

let lazy_t, env = import lazy_t env

let array, env = import array env

(** Canonical references for each of the built-in types that the typechecker
    refers to.
*)
module Type = struct
  open Envi

  let int = TypeDecl.mk_typ int ~params:[] env

  let unit = TypeDecl.mk_typ unit ~params:[] env

  let bool = TypeDecl.mk_typ bool ~params:[] env

  let char = TypeDecl.mk_typ char ~params:[] env

  let string = TypeDecl.mk_typ string ~params:[] env

  let float = TypeDecl.mk_typ float ~params:[] env

  let exn = TypeDecl.mk_typ exn ~params:[] env

  let option a = TypeDecl.mk_typ option ~params:[a] env

  let list a = TypeDecl.mk_typ list ~params:[a] env
end
