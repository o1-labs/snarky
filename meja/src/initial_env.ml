(** The default initial environment. *)
open Core_kernel

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

  let floatarray = abstract "floatarray"

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

  let field = abstract "field"

  let field_var = abstract "field_var"

  let bool_var = abstract "boolean"
end

(** Empty environment. *)
let env = Envi.(empty empty_resolve_env)

open TypeDecls

(** Import the built-in type definitions, overriding the previous definition of
    the environment [env] each time.
*)

let int, env = import Checked int env

let unit, env = import Checked unit env

let char, env = import Checked char env

let bool, env = import Checked bool env

let string, env = import Checked string env

let float, env = import Checked float env

let floatarray, env = import Checked floatarray env

let exn, env = import Checked exn env

let option, env = import Checked option env

let list, env = import Checked list env

let () = Typet.list := Some list

let bytes, env = import Checked bytes env

let int32, env = import Checked int32 env

let int64, env = import Checked int64 env

let nativeint, env = import Checked nativeint env

let lazy_t, env = import Checked lazy_t env

let array, env = import Checked array env

(* Specialised types that may differ across environments. *)

let env = Envi.open_module "__field__" Checked env

let field, env = import Prover field env

let field_var, env = import Checked field_var env

let bool_var, env = import Checked bool_var env

let env = snd (Envi.pop_module ~loc:Location.none env)

(* End of specialised types. *)

(** Canonical references for each of the built-in types that the typechecker
    refers to.
*)
module Type = struct
  open Envi

  let int = TypeDecl.mk_typ Checked int ~params:[] env

  let unit = TypeDecl.mk_typ Checked unit ~params:[] env

  let char = TypeDecl.mk_typ Checked char ~params:[] env

  let string = TypeDecl.mk_typ Checked string ~params:[] env

  let float = TypeDecl.mk_typ Checked float ~params:[] env

  let exn = TypeDecl.mk_typ Checked exn ~params:[] env

  let option a = TypeDecl.mk_typ Checked option ~params:[a] env

  let list a = TypeDecl.mk_typ Checked list ~params:[a] env

  let bool = TypeDecl.mk_typ Checked bool ~params:[] env

  let field =
    let field_var = TypeDecl.mk_typ Checked field_var ~params:[] env in
    let field = TypeDecl.mk_typ Prover field ~params:[] env in
    fun mode -> match mode with Ast_types.Checked -> field_var | _ -> field

  let boolean =
    let bool_var = TypeDecl.mk_typ Checked bool_var ~params:[] env in
    fun mode -> match mode with Ast_types.Checked -> bool_var | _ -> bool
end

let env =
  { env with
    scope_stack=
      List.map env.Envi.scope_stack ~f:(fun scope ->
          let checked_scope = Option.value_exn scope.checked_scope in
          { scope with
            Envi.FullScope.ocaml_scope= checked_scope
          ; prover_scope= checked_scope } ) }

(* Add field types to the respective environments. *)

let env =
  let open Ast_build in
  let reg = Envi.register_type_declaration_raw in
  env
  (* Field.t/Field.Constant.t *)
  |> reg OCaml ~name:"field" (Lid.of_list ["Field"; "Constant"; "t"]) field
  |> reg OCaml ~name:"field_var" (Lid.of_list ["Field"; "t"]) field_var
  |> reg Prover ~name:"field" (Lid.of_name "field") field
  |> reg Checked ~name:"field" (Lid.of_name "field") field_var
  (* bool/Boolean.var *)
  |> reg OCaml (Lid.of_name "bool") bool
  |> reg OCaml ~name:"bool_var" (Lid.of_list ["Boolean"; "var"]) bool_var
  |> reg Prover ~name:"boolean" (Lid.of_name "bool") bool
  |> reg Checked ~name:"boolean" (Lid.of_name "boolean") bool_var
