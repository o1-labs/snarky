open Core_kernel
open Ast_types
open Parsetypes

module Loc = struct
  let mk ?(loc = Location.none) (x : 'a) : 'a Location.loc =
    Location.mkloc x loc

  let map x ~f = {Location.loc= x.Location.loc; txt= f x.Location.txt}

  let of_prim = Ast_types.loc_of_prim

  let of_pos (loc_start, loc_end) =
    {Location.loc_start; loc_end; loc_ghost= false}
end

module Lid = struct
  open Longident

  let of_list l =
    let init, rest =
      match l with
      | [] ->
          failwith "Cannot build a Longident.t from an empty list"
      | x :: rest ->
          (Lident x, rest)
    in
    List.fold ~init rest ~f:(fun lid x -> Ldot (lid, x))

  let of_name str = Lident str

  let last = Longident.last
end

module Type = struct
  let mk ?(loc = Location.none) d : Parsetypes.type_expr =
    {type_desc= d; type_loc= loc}

  let variant ?loc ?(params = []) ident =
    {var_ident= Loc.mk ident ?loc; var_params= params}

  let none ?loc () = mk ?loc (Ptyp_var None)

  let var ?loc name = mk ?loc (Ptyp_var (Some (Loc.mk ?loc name)))

  let constr ?loc ?params ident =
    mk ?loc (Ptyp_ctor (variant ?loc ?params ident))

  let tuple ?loc typs = mk ?loc (Ptyp_tuple typs)

  let arrow ?loc ?(explicit = Explicit) ?(label = Asttypes.Nolabel) typ1 typ2 =
    mk ?loc (Ptyp_arrow (typ1, typ2, explicit, label))

  let poly ?loc vars var = mk ?loc (Ptyp_poly (vars, var))

  let conv ?loc typ1 typ2 = mk ?loc (Ptyp_conv (typ1, typ2))

  let opaque ?loc typ = mk ?loc (Ptyp_opaque typ)

  let row ?loc tags closed min_tags =
    mk ?loc (Ptyp_row (tags, closed, min_tags))

  let row_subtract ?loc typ tags = mk ?loc (Ptyp_row_subtract (typ, tags))
end

module Type_decl = struct
  let mk ?(loc = Location.none) ?(params = []) name d : Parsetypes.type_decl =
    { tdec_ident= Loc.mk ~loc name
    ; tdec_params= params
    ; tdec_desc= d
    ; tdec_loc= loc }

  let abstract ?loc ?params name = mk ?loc ?params name Pdec_abstract

  let alias ?loc ?params name typ = mk ?loc ?params name (Pdec_alias typ)

  let record ?loc ?params name fields =
    mk ?loc ?params name (Pdec_record fields)

  let variant ?loc ?params name ctors =
    mk ?loc ?params name (Pdec_variant ctors)

  let open_ ?loc ?params name = mk ?loc ?params name Pdec_open

  module Field = struct
    let mk ?(loc = Location.none) name typ : Parsetypes.field_decl =
      {fld_ident= Loc.mk ~loc name; fld_type= typ; fld_loc= loc}
  end

  module Ctor = struct
    let mk ?(loc = Location.none) ?ret name d : Parsetypes.ctor_decl =
      {ctor_ident= Loc.mk ~loc name; ctor_args= d; ctor_ret= ret; ctor_loc= loc}

    let with_args ?loc ?ret name args = mk ?loc ?ret name (Ctor_tuple args)

    let with_record ?loc ?ret name fields =
      mk ?loc ?ret name (Ctor_record fields)
  end
end

module Pat = struct
  let mk ?(loc = Location.none) d : Parsetypes.pattern =
    {pat_desc= d; pat_loc= loc}

  let any ?loc () = mk ?loc Ppat_any

  let var ?loc ident = mk ?loc (Ppat_variable (Loc.mk ?loc ident))

  let ctor ?loc ?args name = mk ?loc (Ppat_ctor (Loc.mk ?loc name, args))

  let record ?loc fields = mk ?loc (Ppat_record fields)

  let field ?loc ?eq name =
    let eq =
      match eq with Some eq -> eq | None -> var ?loc (Lid.last name)
    in
    (Loc.mk ?loc name, eq)
end

module Exp = struct
  let mk ?(loc = Location.none) d : Parsetypes.expression =
    {exp_desc= d; exp_loc= loc}

  let fun_ ?loc ?(explicit = Explicit) ?(label = Asttypes.Nolabel) p body =
    mk ?loc (Pexp_fun (label, p, body, explicit))

  let apply ?loc e es = mk ?loc (Pexp_apply (e, es))

  let match_ ?loc e cases = mk ?loc (Pexp_match (e, cases))

  let var ?loc name = mk ?loc (Pexp_variable (Loc.mk ?loc name))

  let ctor ?loc ?args name = mk ?loc (Pexp_ctor (Loc.mk ?loc name, args))

  let record ?loc ?default fields = mk ?loc (Pexp_record (fields, default))

  let let_ ?loc p e_eq e = mk ?loc (Pexp_let (p, e_eq, e))

  let constraint_ ?loc e typ = mk ?loc (Pexp_constraint (e, typ))

  let seq ?loc e1 e2 = mk ?loc (Pexp_seq (e1, e2))

  let literal ?loc l = mk ?loc (Pexp_literal l)
end
