open Core_kernel
open Parsetypes
open Option.Let_syntax

type 'a ident_table = (string, 'a, String.comparator_witness) Map.t

let empty_ident_table = Map.empty (module String)

type t =
  { names: ([`Copy | `NoCopy] * type_expr) ident_table
  ; typ_vars: ([`User | `Generated] * type_expr) ident_table
  ; types: type_decl ident_table
  ; fields: (type_expr * int) ident_table
  ; constructors: (type_expr * int) ident_table
  ; match_instances: type_expr list list
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
      Format.fprintf output "%s : %a\n" key pp_type_expr typ ) ;
  Format.pp_print_string output "Fields:\n" ;
  Map.iteri env.fields ~f:(fun ~key ~data:(typ, _) ->
      Format.fprintf output "%s : %a\n" key pp_type_expr typ ) ;
  Format.pp_print_string output "Constructors:\n" ;
  Map.iteri env.constructors ~f:(fun ~key ~data:(typ, _) ->
      Format.fprintf output "%s : %a\n" key pp_type_expr typ )

let pp_ocaml (output : Format.formatter) (env : t) =
  let open Ast_helper in
  let doc_comment str =
    Ast_helper.Sig.attribute
      Docstrings.(docs_attr (docstring str Location.none))
  in
  let types =
    doc_comment "Types:"
    :: ( List.rev
       @@ Map.fold env.types ~init:[] ~f:(fun ~key ~data:type_decl types ->
              Sig.type_ Recursive
                [To_ocaml.of_type_decl Location.(mkloc key none) type_decl]
              :: types ) )
  in
  let values =
    doc_comment "Values:"
    :: ( List.rev
       @@ Map.fold env.names ~init:[] ~f:(fun ~key ~data:(_, typ) names ->
              Sig.value
                (Val.mk Location.(mkloc key none) (To_ocaml.of_typ typ))
              :: names ) )
  in
  let fields =
    doc_comment "Fields:"
    :: ( List.rev
       @@ Map.fold env.fields ~init:[] ~f:(fun ~key ~data:(typ, _) fields ->
              Sig.value
                (Val.mk Location.(mkloc key none) (To_ocaml.of_typ typ))
              :: fields ) )
  in
  let constructors =
    doc_comment "Constructors:"
    :: ( List.rev
       @@ Map.fold env.constructors ~init:[]
            ~f:(fun ~key ~data:(typ, _) constructors ->
              Sig.value
                (Val.mk Location.(mkloc key none) (To_ocaml.of_typ typ))
              :: constructors ) )
  in
  Pprintast.signature output (types @ values @ fields @ constructors)

let empty =
  { names= empty_ident_table
  ; typ_vars= empty_ident_table
  ; types= empty_ident_table
  ; fields= empty_ident_table
  ; constructors= empty_ident_table
  ; match_instances= []
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

let rec register_type name typ_decl env =
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
  | Variant ctors ->
      let typ = Type.mk_constr' ~loc:name.loc ~decl:typ_decl name in
      List.foldi ctors ~init:env
        ~f:(fun i env {constr_decl_ident= ident; constr_decl_args= args; _} ->
          let env =
            { env with
              constructors=
                Map.update env.constructors ident.txt ~f:(fun _ -> (typ, i)) }
          in
          match args with
          | Constr_tuple _ -> env
          | Constr_record fields ->
              register_type ident
                (TypeDecl.mk ~loc:ident.loc (VariantRecord fields))
                env )
  | _ -> env

let find_type name {types; _} = Map.find types name.txt

let find_record_type name {fields; _} =
  Option.map ~f:fst (Map.find fields name.txt)

let find_field_type name {fields; _} =
  let%bind typ, i = Map.find fields name.txt in
  match typ.type_desc with
  | Tconstr
      { constr_type_decl=
          {type_decl_desc= Record fields | VariantRecord fields; _}; _ } ->
      let%map {field_type; _} = List.nth fields i in
      field_type
  | _ -> None

let find_constructor_types name env =
  let%bind typ, i = Map.find env.constructors name.txt in
  match typ.type_desc with
  | Tconstr {constr_type_decl= {type_decl_desc= Variant ctors; _}; _} ->
      let%bind { constr_decl_return= typ_ret
               ; constr_decl_loc= loc
               ; constr_decl_ident= ident
               ; constr_decl_args= args } =
        List.nth ctors i
      in
      let return_type = Option.value ~default:typ typ_ret in
      let%map argument_type =
        match args with
        | Constr_tuple [x] -> Some x
        | Constr_tuple types -> Some (Type.mk ~loc (Ttuple types))
        | Constr_record _ ->
            let%map record_typ = find_type ident env in
            Type.mk_constr' ~loc ~decl:record_typ ident
      in
      (argument_type, return_type)
  | _ -> None

let push_match_instances instances env =
  {env with match_instances= instances :: env.match_instances}

let pop_match_instances env =
  match env.match_instances with
  | [] -> failwith "No match instances to pop!"
  | instances :: match_instances -> (instances, {env with match_instances})

module Core = struct
  let int = TypeDecl.mk Abstract

  let unit = TypeDecl.mk (Alias (Type.mk (Ttuple [])))

  let bool =
    TypeDecl.mk
      (Variant
         [ TypeDecl.mk_constructor "true" (Constr_tuple [])
         ; TypeDecl.mk_constructor "false" (Constr_tuple []) ])

  let char = TypeDecl.mk Abstract

  let string = TypeDecl.mk Abstract

  let float = TypeDecl.mk Abstract

  let mkloc s = Location.(mkloc s none)

  module Type = struct
    let int = Type.mk_constr' ~decl:int (mkloc "int")

    let unit = Type.mk_constr' ~decl:unit (mkloc "unit")

    let bool = Type.mk_constr' ~decl:bool (mkloc "bool")

    let char = Type.mk_constr' ~decl:char (mkloc "char")

    let string = Type.mk_constr' ~decl:string (mkloc "string")

    let float = Type.mk_constr' ~decl:float (mkloc "float")
  end

  let env =
    empty
    |> register_type (mkloc "int") int
    |> register_type (mkloc "unit") unit
    |> register_type (mkloc "bool") bool
    |> register_type (mkloc "char") char
    |> register_type (mkloc "string") string
    |> register_type (mkloc "float") float
end
