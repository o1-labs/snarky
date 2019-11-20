open Core_kernel

type pattern_case =
  | Pcase_empty
  | Pcase_open
  | Pcase_type of Type0.type_expr
  | Pcase_tuple of pattern_case list
  | Pcase_ctor of Ident.t * pattern_case option
  | Pcase_int of int
  | Pcase_record of (pattern_case * Type0.type_expr) String.Map.t
  | Pcase_or of pattern_case list

let rec pprint_case fmt case =
  let open Format in
  match case with
  | Pcase_empty ->
      fprintf fmt "---EMPTY---" (*assert false*)
  | Pcase_open | Pcase_type _ ->
      fprintf fmt "_"
  | Pcase_tuple cases ->
      fprintf fmt "(@[<hv1>@,%a@,@])"
        (pp_print_list ~pp_sep:Ast_print.comma_sep pprint_case)
        cases
  | Pcase_ctor (name, None) ->
      Ident.pprint fmt name
  | Pcase_ctor (name, Some (Pcase_tuple _ as arg)) ->
      fprintf fmt "%a@ %a" Ident.pprint name pprint_case arg
  | Pcase_ctor (name, Some arg) ->
      fprintf fmt "%a@ (@[<hv1>@,%a@,@])" Ident.pprint name pprint_case arg
  | Pcase_int i ->
      pp_print_int fmt i
  | Pcase_record fields ->
      let first = ref true in
      fprintf fmt "{@[<hv2>" ;
      Map.iteri fields ~f:(fun ~key:name ~data:(case, _) ->
          if !first then first := false else Ast_print.comma_sep fmt () ;
          fprintf fmt "%s:@ @[<hv>%a@]" name pprint_case case ) ;
      fprintf fmt "@]}"
  | Pcase_or cases ->
      let pp_sep fmt () = fprintf fmt "@]@ | @[<hv0>" in
      pp_print_list ~pp_sep pprint_case fmt cases

exception Empty_case

let is_case_empty = function Pcase_empty -> true | _ -> false

let unifies = ref (fun _ _ _ -> assert false)

let rec case_of_type_decl env typ decl =
  let open Type0 in
  let open Type1 in
  let snap = Snapshot.create () in
  assert (!unifies env typ (get_mode typ.type_mode decl.tdec_ret)) ;
  let ret =
    match decl.Type0.tdec_desc with
    | TAlias {type_desc= Ttuple typs; _} ->
        Pcase_tuple (List.map typs ~f:(fun typ -> Pcase_type typ))
    | TAbstract | TAlias _ ->
        failwith "Could not generate a pattern case for type."
    | TRecord _fields ->
        let fields =
          String.Map.empty
          (*List.fold ~init:String.Map.empty fields ~f:(fun map field ->
              let typ = Envi.Type.copy field.fld_type env in
              Map.set map
                ~key:(Ident.name field.fld_ident)
                ~data:(Pcase_type typ, typ) )*)
        in
        Pcase_record fields
    | TVariant ctors ->
        let ctors =
          List.filter_map ctors ~f:(fun ctor ->
              let snap = Snapshot.create () in
              if Option.for_all ~f:(!unifies env typ) ctor.ctor_ret then (
                let arg =
                  match ctor.ctor_args with
                  | Ctor_tuple [] ->
                      None
                  | Ctor_tuple [typ] ->
                      Some (Pcase_type (Envi.Type.copy typ env))
                  | Ctor_tuple typs ->
                      let typs =
                        List.map typs ~f:(fun typ ->
                            Pcase_type (Envi.Type.copy typ env) )
                      in
                      Some (Pcase_tuple typs)
                  | Ctor_record decl ->
                      Some (case_of_type_decl env decl.tdec_ret decl)
                in
                backtrack snap ;
                Some (Pcase_ctor (ctor.ctor_ident, arg)) )
              else None )
        in
        Pcase_or ctors
    | TOpen ->
        Pcase_open
    | TExtend _ ->
        assert false
  in
  backtrack snap ; ret

let case_of_type env typ =
  let typ = Type1.repr typ in
  match
    ( Envi.TypeDecl.find_unaliased_of_type typ env
        ~loc:(Ast_types.loc_of_prim __POS__)
    , typ.type_desc )
  with
  | Some (decl, typ), _ ->
      case_of_type_decl env typ decl
  | None, Ttuple typs ->
      Pcase_tuple (List.map typs ~f:(fun typ -> Pcase_type typ))
  | None, _ ->
      failwith "Could not find the type to generate a case."

let rec intersect_case env case1 case2 =
  match (case1, case2) with
  | Pcase_empty, _ | _, Pcase_empty ->
      Pcase_empty
  | (Pcase_open | Pcase_type _), _ ->
      case2
  | _, (Pcase_open | Pcase_type _) ->
      case1
  | Pcase_tuple cases1, Pcase_tuple cases2 ->
      let cases =
        List.map2_exn cases1 cases2 ~f:(fun case1 case2 ->
            let case = intersect_case env case1 case2 in
            if is_case_empty case then raise Empty_case ;
            case )
      in
      Pcase_tuple cases
  | Pcase_ctor (name1, None), Pcase_ctor (name2, None)
    when Ident.equal name1 name2 ->
      case1
  | Pcase_ctor (name1, Some case1), Pcase_ctor (name2, Some case2)
    when Ident.equal name1 name2 -> (
    match intersect_case env case1 case2 with
    | Pcase_empty ->
        Pcase_empty
    | case ->
        Pcase_ctor (name1, Some case) )
  | Pcase_int i, Pcase_int j when i = j ->
      case1
  | Pcase_int i, _ | _, Pcase_int i ->
      Pcase_int i
  | Pcase_record fields1, Pcase_record fields2 ->
      let fields =
        Map.merge fields1 fields2 ~f:(fun ~key:_ data ->
            let case, typ =
              match data with
              | `Left case | `Right case ->
                  case
              | `Both ((case1, typ), (case2, _typ)) ->
                  (intersect_case env case1 case2, typ)
            in
            match case with
            | Pcase_empty ->
                raise Empty_case
            | Pcase_type _ ->
                None
            | _ ->
                Some (case, typ) )
      in
      Pcase_record fields
  | Pcase_or cases, _ ->
      let cases =
        List.filter_map cases ~f:(fun case1 ->
            match intersect_case env case1 case2 with
            | Pcase_empty ->
                None
            | case ->
                Some case
            | exception Empty_case ->
                None )
      in
      if List.is_empty cases then Pcase_empty else Pcase_or cases
  | _, Pcase_or cases ->
      let cases =
        List.filter_map cases ~f:(fun case2 ->
            match intersect_case env case1 case2 with
            | Pcase_empty ->
                None
            | case ->
                Some case
            | exception Empty_case ->
                None )
      in
      if List.is_empty cases then Pcase_empty else Pcase_or cases
  | _ ->
      Pcase_empty

let intersect_case env case1 case2 =
  try intersect_case env case1 case2 with Empty_case -> Pcase_empty

let rec subtract_case env case sub_case =
  match (case, sub_case) with
  | Pcase_empty, _ ->
      Pcase_empty
  | _, Pcase_empty ->
      case
  | _, (Pcase_type _ | Pcase_open) ->
      Pcase_empty
  | Pcase_open, _ ->
      Pcase_open
  | Pcase_or cases, _ ->
      let cases =
        List.filter_map cases ~f:(fun case ->
            match subtract_case env case sub_case with
            | Pcase_empty ->
                None
            | case ->
                Some case
            | exception Empty_case ->
                None )
      in
      if List.is_empty cases then Pcase_empty else Pcase_or cases
  | _, Pcase_or sub_cases ->
      List.fold ~f:(subtract_case env) ~init:case sub_cases
  | Pcase_type _, Pcase_int _ ->
      (* Don't explode int. *)
      case
  | Pcase_type typ, _ ->
      subtract_case env (case_of_type env typ) sub_case
  | Pcase_tuple cases, Pcase_tuple sub_cases ->
      let cases =
        List.map2_exn cases sub_cases ~f:(fun case sub_case ->
            let case = subtract_case env case sub_case in
            if is_case_empty case then raise Empty_case ;
            case )
      in
      Pcase_tuple cases
  | Pcase_ctor (name1, None), Pcase_ctor (name2, None)
    when Ident.equal name1 name2 ->
      Pcase_empty
  | Pcase_ctor (name, Some case), Pcase_ctor (sub_name, Some sub_case)
    when Ident.equal name sub_name -> (
    match subtract_case env case sub_case with
    | Pcase_empty ->
        Pcase_empty
    | case ->
        Pcase_ctor (name, Some case) )
  | Pcase_int i, Pcase_int j when i = j ->
      Pcase_empty
  | Pcase_record fields, Pcase_record sub_fields -> (
      let is_empty = ref true in
      let subbed_fields =
        Map.merge fields sub_fields ~f:(fun ~key:_ data ->
            let typ =
              match data with
              | `Left (_, typ) | `Right (_, typ) | `Both ((_, typ), _) ->
                  typ
            in
            let case =
              try
                match data with
                | `Left _ ->
                    Pcase_empty
                | `Right (sub_case, typ) ->
                    subtract_case env (Pcase_type typ) sub_case
                | `Both ((case, _), (sub_case, _)) ->
                    subtract_case env case sub_case
              with Empty_case -> Pcase_empty
            in
            if is_case_empty case then None
            else (
              is_empty := false ;
              Some (case, typ) ) )
      in
      if !is_empty && not (Map.is_empty fields && Map.is_empty sub_fields) then
        raise Empty_case ;
      let exception Return in
      try
        Map.iter2 fields sub_fields ~f:(fun ~key:_ ~data ->
            match data with
            | `Left _ | `Right _ ->
                ()
            | `Both ((case, _), (sub_case, _)) ->
                if is_case_empty (intersect_case env case sub_case) then
                  raise Return ) ;
        let cases =
          Map.fold ~init:[] subbed_fields
            ~f:(fun ~key ~data:((case, _) as data) cases ->
              if is_case_empty case then cases
              else Pcase_record (Map.set fields ~key ~data) :: cases )
        in
        Pcase_or cases
      with Return -> case )
  | _ ->
      case

let subtract_case env case sub_case =
  try subtract_case env case sub_case with Empty_case -> Pcase_empty

let rec case_of_pattern env pat =
  let open Typedast in
  match pat.pat_desc with
  | Tpat_any | Tpat_variable _ ->
      Pcase_type pat.pat_type
  | Tpat_constraint (pat, _) ->
      case_of_pattern env pat
  | Tpat_tuple [pat] ->
      case_of_pattern env pat
  | Tpat_tuple pats ->
      Pcase_tuple (List.map ~f:(case_of_pattern env) pats)
  | Tpat_or (pat1, pat2) -> (
      let case1 = case_of_pattern env pat1 in
      let case2 = case_of_pattern env pat2 in
      match (case1, case2) with
      | Pcase_or cases1, Pcase_or cases2 ->
          Pcase_or (cases1 @ cases2)
      | Pcase_or cases, case | case, Pcase_or cases ->
          Pcase_or (case :: cases)
      | _ ->
          Pcase_or [case1; case2] )
  | Tpat_int i ->
      Pcase_int i
  | Tpat_record fields ->
      let fields =
        List.fold ~init:String.Map.empty fields ~f:(fun fields (path, pat) ->
            let key =
              match path.Location.txt with
              | Path.Pident ident ->
                  Ident.name ident
              | Pdot (_path, _mode, name) ->
                  name
              | Papply _ ->
                  assert false
            in
            match
              Map.add fields ~key ~data:(case_of_pattern env pat, pat.pat_type)
            with
            | `Ok fields ->
                fields
            | `Duplicate ->
                (* TODO: Full error. *)
                failwithf "Duplicated field %s" key () )
      in
      let fields =
        Map.filter fields ~f:(fun (case, _) -> not (is_case_empty case))
      in
      Pcase_record fields
  | Tpat_ctor (path, arg) ->
      let name, _ =
        Envi.get_of_constructor path.txt env
          ~loc:(Ast_types.loc_of_prim __POS__)
      in
      Pcase_ctor (name, Option.map ~f:(case_of_pattern env) arg)

(** Returns [Some pcase] for the pattern case that is not matched, if one
    exists, or [None] otherwise.
*)
let check_exhaustivity env typ pats =
  let sub_case = Pcase_or (List.map ~f:(case_of_pattern env) pats) in
  let case = Pcase_type typ in
  match subtract_case env case sub_case with
  | Pcase_empty ->
      None
  | case ->
      Some case

let get_unmatched_cases ~count env typ pats =
  let rec go count candidates =
    if count > 0 then
      match candidates with
      | [] ->
          []
      | (candidate, sub_cases) :: candidates -> (
        match sub_cases with
        | [] ->
            candidate :: go (count - 1) candidates
        | sub_case :: sub_cases -> (
          match subtract_case env candidate sub_case with
          | Pcase_empty ->
              go count candidates
          | Pcase_or cases ->
              let new_candidates =
                List.map cases ~f:(fun case -> (case, sub_cases))
              in
              go count (new_candidates @ candidates)
          | _ ->
              go count ((candidate, sub_cases) :: candidates) ) )
    else []
  in
  let sub_cases = List.map ~f:(case_of_pattern env) pats in
  let case = Pcase_type typ in
  go count [(case, sub_cases)]
