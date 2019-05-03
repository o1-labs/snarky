open Core_kernel
open Parsetypes
open Envi
open Longident

let mkloc s = Location.(mkloc s none)

let mk_type_decl ?(params = []) ?(implicit_params = []) name desc =
  { tdec_ident= mkloc name
  ; tdec_params= params
  ; tdec_implicit_params= implicit_params
  ; tdec_desc= desc
  ; tdec_id= 0
  ; tdec_loc= Location.none }

let mk_constructor ?(params = []) name =
  { ctor_ident= mkloc name
  ; ctor_args= Ctor_tuple params
  ; ctor_ret= None
  ; ctor_loc= Location.none }

let env = empty (empty_resolve_env ())

let int, env = TypeDecl.import (mk_type_decl "int" TAbstract) env

let unit, env =
  TypeDecl.import (mk_type_decl "unit" (TVariant [mk_constructor "()"])) env

let bool, env =
  TypeDecl.import
    (mk_type_decl "bool"
       (TVariant [mk_constructor "true"; mk_constructor "false"]))
    env

let char, env = TypeDecl.import (mk_type_decl "char" TAbstract) env

let string, env = TypeDecl.import (mk_type_decl "string" TAbstract) env

let float, env = TypeDecl.import (mk_type_decl "float" TAbstract) env

let exn, env = TypeDecl.import (mk_type_decl "exn" TOpen) env

let option, env =
  let var = Type.mkvar ~loc:Location.none (Some (mkloc "a")) env in
  TypeDecl.import
    (mk_type_decl "option" ~params:[var]
       (TVariant [mk_constructor "Some" ~params:[var]; mk_constructor "None"]))
    env

let list, env =
  let var = Type.mkvar ~loc:Location.none (Some (mkloc "a")) env in
  let typ =
    Type.mk ~loc:Location.none
      (Tctor
         { var_ident= mkloc (Lident "list")
         ; var_params= [var]
         ; var_implicit_params= []
         ; var_decl_id= 0 })
      env
  in
  TypeDecl.import
    (mk_type_decl "list" ~params:[var]
       (TVariant [mk_constructor "::" ~params:[var; typ]; mk_constructor "[]"]))
    env

let env =
  List.fold ~init:env
    ~f:(fun env (name, vars) ->
      let params =
        List.init vars ~f:(fun _ -> Type.mkvar ~loc:Location.none None env)
      in
      snd (TypeDecl.import (mk_type_decl name ~params TAbstract) env) )
    [("bytes", 0); ("int32", 0); ("int64", 0); ("nativeint", 0)]

let field, env =
  let var =
    Type.mkvar ~loc:Location.none ~explicitness:Implicit
      (Some (mkloc "field"))
      env
  in
  TypeDecl.import (mk_type_decl "field" (TUnfold var)) env

let lazy_t, env =
  let var = Type.mkvar ~loc:Location.none (Some (mkloc "a")) env in
  TypeDecl.import (mk_type_decl "lazy_t" ~params:[var] TAbstract) env

let array, env =
  let var = Type.mkvar ~loc:Location.none (Some (mkloc "a")) env in
  TypeDecl.import (mk_type_decl "array" ~params:[var] TAbstract) env

module Type = struct
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
