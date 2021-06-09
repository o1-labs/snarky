(** Replace paths in types to account for changing scope. *)

open Core_kernel

(* Note: we need to distinguish types and modules because there can be overlap.
         In particular, the type of a record argument to a GADT constructor has
         name A.B.X, where X is the name of the constructor.
*)
type t =
  { types : Path.t Path.Map.t
  ; modules : Path.t Path.Map.t
  ; expressions : Path.t Path.Map.t
  }

let empty =
  { types = Path.Map.empty
  ; modules = Path.Map.empty
  ; expressions = Path.Map.empty
  }

let with_type src dst s = { s with types = Map.set ~key:src ~data:dst s.types }

let with_module src dst s =
  { s with modules = Map.set ~key:src ~data:dst s.modules }

let with_expression src dst s =
  { s with expressions = Map.set ~key:src ~data:dst s.expressions }

let rec module_path s path =
  match Map.find s.modules path with
  | Some path' ->
      path'
  | None -> (
      match path with
      | Pident _ ->
          path
      | Pdot (path, mode, name) ->
          Pdot (module_path s path, mode, name)
      | Pocamldot (path, mode, name, ocaml_name) ->
          Pocamldot (module_path s path, mode, name, ocaml_name)
      | Papply (path1, path2) ->
          Papply (module_path s path1, module_path s path2) )

let type_path s path =
  match Map.find s.types path with
  | Some path' ->
      path'
  | None -> (
      match path with
      | Pident _ ->
          path
      | Pdot (path, mode, name) ->
          Pdot (module_path s path, mode, name)
      | Pocamldot (path, mode, name, ocaml_name) ->
          Pocamldot (module_path s path, mode, name, ocaml_name)
      | Papply _ ->
          failwith "Subst.type_path: Unhandled Papply" )

let expression_path s path =
  match Map.find s.expressions path with
  | Some path ->
      path
  | None -> (
      match path with
      | Pident _ ->
          path
      | Pdot (path, mode, name) ->
          Pdot (module_path s path, mode, name)
      | Pocamldot (path, mode, name, ocaml_name) ->
          Pocamldot (module_path s path, mode, name, ocaml_name)
      | Papply _ ->
          failwith "Subst.expression_path: Unhandled Papply" )

let type0_mapper s =
  { Type0_map.default_mapper with path = (fun _mapper -> type_path s) }

let type_expr s =
  let mapper = type0_mapper s in
  mapper.type_expr mapper

let type_decl s =
  let mapper = type0_mapper s in
  mapper.type_decl mapper
