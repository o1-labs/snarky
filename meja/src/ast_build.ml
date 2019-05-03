open Core_kernel
open Parsetypes

module Loc = struct
  let mk ?(loc = Location.none) x = Location.mkloc x loc

  let map x ~f = {Location.loc= x.Location.loc; txt= f x.Location.txt}
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
  let mk ?(loc = Location.none) d = {type_desc= d; type_id= -1; type_loc= loc}

  let variant ?loc ?(params = []) ?(implicits = []) ident =
    { var_ident= Loc.mk ident ?loc
    ; var_params= params
    ; var_implicit_params= implicits
    ; var_decl_id= -1 }

  let none ?loc ?(explicit = Explicit) () = mk ?loc (Tvar (None, -1, explicit))

  let var ?loc ?(explicit = Explicit) name =
    mk ?loc (Tvar (Some (Loc.mk ?loc name), -1, explicit))

  let constr ?loc ?params ?implicits ident =
    mk ?loc (Tctor (variant ?loc ?params ?implicits ident))

  let tuple ?loc typs = mk ?loc (Ttuple typs)

  let arrow ?loc ?(explicit = Explicit) ?(label = Asttypes.Nolabel) typ1 typ2 =
    mk ?loc (Tarrow (typ1, typ2, explicit, label))

  let poly ?loc vars var = mk ?loc (Tpoly (vars, var))
end

module Type_decl = struct
  let mk ?(loc = Location.none) ?(params = []) ?(implicits = []) name d =
    { tdec_ident= Loc.mk ~loc name
    ; tdec_params= params
    ; tdec_implicit_params= implicits
    ; tdec_desc= d
    ; tdec_id= -1
    ; tdec_loc= loc }

  let abstract ?loc ?params ?implicits name =
    mk ?loc ?params ?implicits name TAbstract

  let alias ?loc ?params ?implicits name typ =
    mk ?loc ?params ?implicits name (TAlias typ)

  let unfold ?loc ?params ?implicits name typ =
    mk ?loc ?params ?implicits name (TUnfold typ)

  let record ?loc ?params ?implicits name fields =
    mk ?loc ?params ?implicits name (TRecord fields)

  let variant ?loc ?params ?implicits name ctors =
    mk ?loc ?params ?implicits name (TVariant ctors)

  let open_ ?loc ?params ?implicits name =
    mk ?loc ?params ?implicits name TOpen

  let forward ?loc ?params ?implicits name =
    mk ?loc ?params ?implicits name (TForward (ref None))

  module Field = struct
    let mk ?(loc = Location.none) name typ =
      {fld_ident= Loc.mk ~loc name; fld_type= typ; fld_id= -1; fld_loc= loc}
  end

  module Ctor = struct
    let mk ?(loc = Location.none) ?ret name d =
      {ctor_ident= Loc.mk ~loc name; ctor_args= d; ctor_ret= ret; ctor_loc= loc}

    let with_args ?loc ?ret name args = mk ?loc ?ret name (Ctor_tuple args)

    let with_record ?loc ?ret name fields =
      mk ?loc ?ret name (Ctor_record (-1, fields))
  end
end

module Pat = struct
  let mk ?(loc = Location.none) d =
    {pat_desc= d; pat_loc= loc; pat_type= Type.none ~loc ()}

  let any ?loc () = mk ?loc PAny

  let var ?loc ident = mk ?loc (PVariable (Loc.mk ?loc ident))

  let ctor ?loc ?args name = mk ?loc (PCtor (Loc.mk ?loc name, args))

  let record ?loc fields = mk ?loc (PRecord fields)

  let field ?loc ?eq name =
    let eq =
      match eq with Some eq -> eq | None -> var ?loc (Lid.last name)
    in
    (Loc.mk ?loc name, eq)
end

module Exp = struct
  let mk ?(loc = Location.none) d =
    {exp_desc= d; exp_loc= loc; exp_type= Type.none ~loc ()}

  let fun_ ?loc ?(explicit = Explicit) ?(label = Asttypes.Nolabel) p body =
    mk ?loc (Fun (label, p, body, explicit))

  let apply ?loc e es = mk ?loc (Apply (e, es))

  let match_ ?loc e cases = mk ?loc (Match (e, cases))

  let var ?loc name = mk ?loc (Variable (Loc.mk ?loc name))

  let ctor ?loc ?args name = mk ?loc (Ctor (Loc.mk ?loc name, args))

  let record ?loc ?default fields = mk ?loc (Record (fields, default))

  let let_ ?loc p e_eq e = mk ?loc (Let (p, e_eq, e))

  let constraint_ ?loc e typ = mk ?loc (Constraint (e, typ))
end
