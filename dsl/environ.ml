open Core_kernel
open Parsetypes

type 'a ident_table = (string, 'a, String.comparator_witness) Map.t

let empty_ident_table = Map.empty (module String)

type t =
  { names: ([`Copy | `NoCopy] * type_expr) ident_table
  ; typ_vars: ([`User | `Generated] * type_expr) ident_table
  ; types: type_decl ident_table
  ; fields: (type_expr * int) ident_table
  ; vars_size: int
  ; depth: int }

let pp (output : Format.formatter) (env : t) =
  Format.pp_print_string output "Types:\n" ;
  Map.iteri env.types ~f:(fun ~key ~data:type_decl ->
      Format.fprintf output "%s = %a\n" key pp_type_decl type_decl ) ;
  Format.pp_print_string output "Names:\n" ;
  Map.iteri env.names ~f:(fun ~key ~data:(_, typ) ->
      Format.fprintf output "%s : %a\n" key pp_type_expr typ ) ;
  Format.pp_print_string output "Free type variables:\n" ;
  Map.iteri env.typ_vars ~f:(fun ~key ~data:(_, typ) ->
      Format.fprintf output "%s : %a\n" key pp_type_expr typ )

let pp_ocaml (output : Format.formatter) (env : t) =
  Format.pp_print_string output "Types:\n" ;
  Map.iteri env.types ~f:(fun ~key ~data:type_decl ->
      Format.fprintf output "%a\n" Pprintast.structure
        [ Ast_helper.Str.type_ Nonrecursive
            [To_ocaml.of_type_decl Location.(mkloc key none) type_decl] ] ) ;
  Format.pp_print_string output "Names:\n" ;
  Map.iteri env.names ~f:(fun ~key ~data:(_, typ) ->
      Format.fprintf output "%s : %a\n" key Pprintast.core_type
        (To_ocaml.of_typ typ) )

let empty =
  { names= empty_ident_table
  ; typ_vars= empty_ident_table
  ; types= empty_ident_table
  ; fields= empty_ident_table
  ; vars_size= 0
  ; depth= 0 }

let add_name name typ env =
  {env with names= Map.update env.names name.txt ~f:(fun _ -> typ)}

let find_name name {names; _} = Map.find names name.txt

let add_type_var ~user ~name var env =
  { env with
    typ_vars=
      Map.update env.typ_vars name.txt ~f:(fun _ ->
          if user then (`User, var) else (`Generated, var) ) }

let find_type_var name {typ_vars; _} = Map.find typ_vars name.txt

let register_type name typ_decl env =
  let env =
    {env with types= Map.update env.types name.txt ~f:(fun _ -> typ_decl)}
  in
  match typ_decl.type_decl_desc with
  | Record fields ->
      let typ = Type.mk_constr' ~loc:name.loc ~decl:typ_decl name in
      { env with
        fields=
          List.foldi fields ~init:env.fields ~f:(fun i env {field_ident; _} ->
              Map.update env field_ident.txt ~f:(fun _ -> (typ, i)) ) }
  | _ -> env

let find_type name {types; _} = Map.find types name.txt

let find_record_type name {fields; _} =
  Option.map ~f:fst (Map.find fields name.txt)

let find_field_type name {fields; _} =
  match Map.find fields name.txt with
  | None -> None
  | Some (typ, i) -> (
    match typ.type_desc with
    | Tconstr {constr_type_decl= {type_decl_desc= Record fields; _}; _} ->
        Some (List.nth_exn fields i).field_type
    | _ -> None )

module Core = struct
  let int = TypeDecl.mk Abstract
  let unit = TypeDecl.mk (Alias (Type.mk (Ttuple [])))

  let mkloc s = Location.(mkloc s none)

  let env = empty |> register_type (mkloc "int") int
    |> register_type (mkloc "unit") unit
end
