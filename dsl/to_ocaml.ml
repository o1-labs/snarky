open Core_kernel
open Asttypes
open Ast_helper
open Parsetypes

let mk_lid name = Location.mkloc (Longident.Lident name.txt) name.loc

let rec of_typ typ =
  let loc = typ.type_loc in
  match typ.type_desc with
  | Tpoly (var, typ) ->
      var.in_recursion <- true ;
      let typ' = of_typ typ in
      var.in_recursion <- false ;
      typ'
  | Tvar {instance= Some typ; _} when not typ.in_recursion ->
      typ.in_recursion <- true ;
      let typ' = of_typ typ in
      typ.in_recursion <- false ;
      typ'
  | Tvar {name= None; _} ->
      (*Typ.any ~loc ()*) Typ.var ~loc (sprintf "_a%d" typ.id)
  | Tvar {name= Some name; _} -> Typ.var ~loc name.txt
  | Tarrow (typ1, typ2) -> Typ.arrow ~loc Nolabel (of_typ typ1) (of_typ typ2)
  | Tconstr {constr_ident; _} -> Typ.constr ~loc (mk_lid constr_ident) []
  | Ttuple [] ->
      Typ.constr ~loc Location.(mkloc (Longident.Lident "unit") none) []
  | Ttuple typs -> Typ.tuple ~loc (List.map ~f:of_typ typs)
  | Tdefer typ -> of_typ typ

let of_field_decl field =
  let open Ast_helper in
  Type.field ~loc:field.field_loc field.field_ident (of_typ field.field_type)

let of_constr_args = function
  | Constr_tuple args -> Parsetree.Pcstr_tuple (List.map ~f:of_typ args)
  | Constr_record fields ->
      Parsetree.Pcstr_record (List.map ~f:of_field_decl fields)

let of_type_decl name typ_dec =
  let open Ast_helper in
  let loc = typ_dec.type_decl_loc in
  match typ_dec.type_decl_desc with
  | Abstract -> Type.mk name ~loc
  | Alias typ -> Type.mk name ~loc ~manifest:(of_typ typ)
  | Record fields | VariantRecord fields ->
      Type.mk name ~loc
        ~kind:(Parsetree.Ptype_record (List.map ~f:of_field_decl fields))
  | Variant ctors ->
      Type.mk name ~loc
        ~kind:
          (Parsetree.Ptype_variant
             (List.map ctors ~f:(fun ctor ->
                  Type.constructor ~loc
                    ~args:(of_constr_args ctor.constr_decl_args)
                    ?res:(Option.map ~f:of_typ ctor.constr_decl_return)
                    ctor.constr_decl_ident )))

let rec of_pattern pat =
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | PAny -> Pat.any ~loc ()
  | PConstant c -> Pat.constant ~loc c
  | PVariable str -> Pat.var ~loc str
  | PConstraint {pcon_pat= p; pcon_typ= typ} ->
      Pat.constraint_ ~loc (of_pattern p) (of_typ typ)
  | PRecord (fields, closed_flag) ->
      Pat.record ~loc
        (List.map fields ~f:(fun (ident, p) -> (mk_lid ident, of_pattern p)))
        closed_flag
  | POr (p1, p2) -> Pat.or_ ~loc (of_pattern p1) (of_pattern p2)
  | PTuple ps -> Pat.tuple ~loc (List.map ~f:of_pattern ps)

let rec of_expression exp =
  let loc = exp.exp_loc in
  match exp.exp_desc with
  | Constant c -> Exp.constant ~loc c
  | Variable name -> Exp.ident ~loc (mk_lid name)
  | Apply (f, xs) ->
      let xs = List.map xs ~f:(fun x -> (Nolabel, of_expression x)) in
      Exp.apply ~loc (of_expression f) xs
  | Fun (p, body) ->
      Exp.fun_ ~loc Nolabel None (of_pattern p) (of_expression body)
  | Constraint {econ_exp= e; econ_typ= typ} ->
      Exp.constraint_ ~loc (of_expression e) (of_typ typ)
  | Seq (e1, e2) -> Exp.sequence ~loc (of_expression e1) (of_expression e2)
  | Let (p, e_rhs, e) ->
      Exp.let_ ~loc Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)
  | Tuple es -> Exp.tuple ~loc (List.map ~f:of_expression es)
  | Record_literal {record_values; record_fields; _} ->
      Exp.record ~loc
        (List.zip_exn
           (List.map
              ~f:(fun {field_ident; _} -> mk_lid field_ident)
              record_fields)
           (List.map ~f:of_expression record_values))
        None
  | Field (e, field) -> Exp.field ~loc (of_expression e) (mk_lid field)
  | Match (e, cases) ->
      Exp.match_ ~loc (of_expression e)
        (List.map cases ~f:(fun (p, e) ->
             Exp.case (of_pattern p) (of_expression e) ))
  | Constructor (id, args) ->
      Exp.construct (mk_lid id) (Option.map ~f:of_expression args)

let of_statement stmt =
  let loc = stmt.stmt_loc in
  match stmt.stmt_desc with
  | Value (p, e) ->
      Str.value ~loc Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]
  | Type (name, typ_decl) ->
      Str.type_ ~loc Recursive [of_type_decl name typ_decl]

let of_file = List.map ~f:of_statement
