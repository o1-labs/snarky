open Core_kernel
open Ast_types
open Type0

type error =
  | Mk_wrong_mode of string * type_expr list * mode * type_expr
  | Mk_invalid of string * type_expr list * type_expr

exception Error of Location.t * error

let check_mode ~pos ~error_info mode typ =
  if not (equal_mode mode typ.type_mode) then
    let kind, typs = error_info () in
    raise
      (Error (Ast_build.Loc.of_prim pos, Mk_wrong_mode (kind, typs, mode, typ)))

(** Get the stitched [type_expr] in the given mode. *)
let get_mode mode typ =
  if equal_mode typ.type_mode mode then typ
  else
    let typ = typ.type_alternate in
    (* Sanity check. *)
    assert (equal_mode typ.type_mode mode) ;
    typ

let type_id = ref 0

(** An invalid [type_expr] in checked mode. *)
let rec checked_none =
  { type_desc= Tvar None
  ; type_id= -1
  ; type_depth= 0
  ; type_mode= Checked
  ; type_alternate= prover_none }

(** An invalid [type_expr] in prover mode. *)
and prover_none =
  { type_desc= Tvar None
  ; type_id= -2
  ; type_depth= 0
  ; type_mode= Prover
  ; type_alternate= checked_none }

(** Chose an invalid [type_expr] based on the given mode. *)
let none = function Checked -> checked_none | Prover -> prover_none

(** Chose an invalid [type_expr] for the other mode to the one given.

    [other_none mode = none (other_mode mode)]
*)
let other_none = function Checked -> prover_none | Prover -> checked_none

let type_alternate {type_alternate= typ; _} = typ

let is_poly = function {type_desc= Tpoly _; _} -> true | _ -> false

(** Returns [true] if the [type_expr] argument is valid, false otherwise.
   Can be used to check that a type-stitching has been created or modified
   correctly.
*)
let is_valid {type_id; _} = type_id > 0

(** Equivalent to [not (is_valid typ)]. *)
let is_invalid {type_id; _} = type_id <= 0

(** Judge equality based on type id. *)
let equal {type_id= id1; _} {type_id= id2; _} = Int.equal id1 id2

let check_valid ~pos ~error_info typ =
  if is_invalid typ then
    let kind, typs = error_info () in
    raise (Error (Ast_build.Loc.of_prim pos, Mk_invalid (kind, typs, typ)))

(** Make a new type at the given mode and depth.

    The [type_alternate] field is set to the invalid value
    [none (other_mode mode)].
*)
let mk' ~mode depth type_desc =
  incr type_id ;
  (*assert (!type_id <> 51696) ;*)
  { type_desc
  ; type_id= !type_id
  ; type_depth= depth
  ; type_mode= mode
  ; type_alternate= other_none mode }

(** [stitch typ typ'] sets [typ] and [typ'] as eachothers [type_alternate]s.
    Returns the first argument [typ].

    Raises [AssertionError] if the types have the same modes, or if either type
    has an already-initialised [type_alternate].
*)
let stitch typ typ' =
  (* Check that the types have distinct modes. *)
  assert (equal_mode typ.type_mode (other_mode typ'.type_mode)) ;
  (* Check that the [type_alternate]s are uninitialised. *)
  assert (is_invalid typ.type_alternate) ;
  assert (is_invalid typ'.type_alternate) ;
  typ.type_alternate <- typ' ;
  typ'.type_alternate <- typ ;
  typ

(** [tri_stitch in_typ ptyp ctyp] stitches the prover-mode type [ptyp] to the
    checked-mode [ctyp], and sets the [type_alternate] of the checked-mode type
    [in_typ] to [ptyp].

    This allows [in_typ] to be the [tdec_ret] for a type defined in checked
    mode, so that a round-trip [Checked -> Prover -> Checked] conversion chain
    may start and end with different types.

    The canonical example of this is [bool -> bool -> boolean].
*)
let tri_stitch in_typ ptyp ctyp =
  assert (equal_mode in_typ.type_mode Checked) ;
  assert (equal_mode ptyp.type_mode Prover) ;
  assert (equal_mode ctyp.type_mode Checked) ;
  assert (is_invalid in_typ.type_alternate) ;
  assert (is_invalid ptyp.type_alternate) ;
  assert (is_invalid ctyp.type_alternate) ;
  in_typ.type_alternate <- ptyp ;
  ptyp.type_alternate <- ctyp ;
  ctyp.type_alternate <- ptyp ;
  in_typ

(** Returns [true] if the types are stitched together, [false] otherwise. *)
let are_stitched typ typ' =
  (phys_equal typ typ'.type_alternate && phys_equal typ' typ.type_alternate)
  ||
  (* Tri-stitching check. ctyp -> ptyp -> _ -> ptyp *)
  let ctyp, ptyp, modes_match =
    match (typ.type_mode, typ'.type_mode) with
    | Checked, Prover ->
        (typ, typ', true)
    | Prover, Checked ->
        (typ', typ, true)
    | _ ->
        (* Dummy ordering of types. The [false] value will shortcut the
           tri-stitching check.
        *)
        (typ, typ', false)
  in
  modes_match
  && phys_equal ctyp.type_alternate ptyp
  && phys_equal ptyp ptyp.type_alternate.type_alternate

(** Hash set to track types printed in [typ_debug_print], to ensure that we
    don't get stuck in a recursion loop.
*)
let typ_debug_print_hash_tbl = Hash_set.create (module Int) ()

let rec typ_debug_print fmt typ =
  let hashtbl = typ_debug_print_hash_tbl in
  let open Format in
  let print i = fprintf fmt i in
  let print_comma fmt () = pp_print_char fmt ',' in
  let print_list pp = pp_print_list ~pp_sep:print_comma pp in
  let print_label fmt = function
    | Asttypes.Nolabel ->
        ()
    | Asttypes.Labelled str ->
        fprintf fmt "~%s:" str
    | Asttypes.Optional str ->
        fprintf fmt "?%s:" str
  in
  print "(%i%a:" typ.type_id mode_debug_print typ.type_mode ;
  if Hash_set.mem hashtbl typ.type_id then
    (* Recursion breaking. *)
    print "RECURSIVE"
  else (
    ( Hash_set.add hashtbl typ.type_id ;
      match typ.type_desc with
      | Tvar None ->
          print "var _"
      | Tvar (Some name) ->
          print "var %s" name
      | Tpoly (typs, typ) ->
          print "poly [%a] %a"
            (print_list typ_debug_print)
            typs typ_debug_print typ
      | Tarrow (typ1, typ2, Explicit, label) ->
          print "%a%a -> %a" print_label label typ_debug_print typ1
            typ_debug_print typ2
      | Tarrow (typ1, typ2, Implicit, label) ->
          print "%a{%a} -> %a" print_label label typ_debug_print typ1
            typ_debug_print typ2
      | Tctor {var_ident= name; var_params= params; _} ->
          print "%a (%a)" Path.debug_print name
            (print_list typ_debug_print)
            params
      | Ttuple typs ->
          print "(%a)" (print_list typ_debug_print) typs
      | Tref typ ->
          print "= " ; typ_debug_print fmt typ
      | Tconv typ ->
          typ_debug_print fmt (get_mode Checked typ) ;
          print " --> " ;
          typ_debug_print fmt (get_mode Prover typ)
      | Topaque typ ->
          print "opaque " ; typ_debug_print fmt typ
      | Tother_mode typ ->
          print "mode-change " ; typ_debug_print fmt typ
      | Treplace typ ->
          print "=== " ; typ_debug_print fmt typ ) ;
    Hash_set.remove hashtbl typ.type_id ) ;
  print " @%i)" typ.type_depth

let typ_debug_print_alts fmt typ =
  let open Format in
  if phys_equal typ typ.type_alternate.type_alternate then
    fprintf fmt "@[<hov>(stitched@ (%a)@ (%a))@]" typ_debug_print typ
      typ_debug_print typ.type_alternate
  else
    fprintf fmt "@[<hov>(tri-stitched@ (%a)@ (%a)@ (%a))@]" typ_debug_print typ
      typ_debug_print typ.type_alternate typ_debug_print
      typ.type_alternate.type_alternate

(** Create a new type variable with the given name.

    The returned type is properly stitched to an equivalent type in the other
    mode.
*)
let mkvar ~mode depth name =
  let typ = mk' ~mode depth (Tvar name) in
  let other_typ = mk' ~mode:(other_mode mode) depth (Tvar name) in
  if equal_mode mode Checked then
    let tri_typ = mk' ~mode depth (Tvar name) in
    tri_stitch typ other_typ tri_typ
  else stitch typ other_typ

(** Constructors for stitched types. *)
module Mk = struct
  let var = mkvar

  let stitch ~mode depth desc1 desc2 =
    stitch (mk' ~mode depth desc1) (mk' ~mode:(other_mode mode) depth desc2)

  let tri_stitch ~mode depth desc1 desc2 desc3 =
    assert (equal_mode mode Checked) ;
    tri_stitch
      (mk' ~mode:Checked depth desc1)
      (mk' ~mode:Prover depth desc2)
      (mk' ~mode:Checked depth desc3)

  let tuple ~mode depth typs =
    let error_info () = ("tuple", typs) in
    let alts = List.map ~f:type_alternate typs in
    let alt_alts = List.map ~f:type_alternate alts in
    if
      List.for_all2_exn typs alt_alts ~f:(fun typ alt ->
          (* Sanity check. *)
          check_mode ~pos:__POS__ ~error_info mode typ ;
          check_mode ~pos:__POS__ ~error_info mode alt ;
          assert (not (is_poly typ)) ;
          check_valid ~pos:__POS__ ~error_info typ ;
          check_valid ~pos:__POS__ ~error_info alt ;
          phys_equal typ alt )
    then stitch ~mode depth (Ttuple typs) (Ttuple alts)
    else
      (* One or more types is tri-stitched, so tri-stitch here too. *)
      tri_stitch ~mode depth (Ttuple typs) (Ttuple alts) (Ttuple alt_alts)

  let arrow ~mode ?(explicit = Explicit) ?(label = Nolabel) depth typ1 typ2 =
    let error_info () = ("arrow", [typ1; typ2]) in
    let alt1 = type_alternate typ1 in
    let alt2 = type_alternate typ2 in
    let alt_alt1 = type_alternate alt1 in
    let alt_alt2 = type_alternate alt2 in
    if
      List.for_all2_exn [typ1; typ2] [alt_alt1; alt_alt2] ~f:(fun typ alt ->
          (* Sanity check. *)
          check_mode ~pos:__POS__ ~error_info mode typ ;
          check_mode ~pos:__POS__ ~error_info mode alt ;
          check_valid ~pos:__POS__ ~error_info typ ;
          check_valid ~pos:__POS__ ~error_info alt ;
          assert (not (is_poly typ)) ;
          phys_equal typ alt )
    then
      stitch ~mode depth
        (Tarrow (typ1, typ2, explicit, label))
        (Tarrow (alt1, alt2, explicit, label))
    else
      (* One or more types is tri-stitched, so tri-stitch here too. *)
      tri_stitch ~mode depth
        (Tarrow (typ1, typ2, explicit, label))
        (Tarrow (alt1, alt2, explicit, label))
        (Tarrow (alt_alt1, alt_alt2, explicit, label))

  let ctor ~mode depth path ?other_path ?tri_path params =
    let error_info () =
      let desc =
        Format.(fprintf str_formatter "ctor (%a)" Path.debug_print path) ;
        Format.flush_str_formatter ()
      in
      (desc, params)
    in
    assert (Option.is_some other_path || Option.is_none tri_path) ;
    let other_path = Option.value ~default:path other_path in
    let alts = List.map ~f:type_alternate params in
    let alt_alts = List.map ~f:type_alternate alts in
    if
      List.for_all2_exn params alt_alts ~f:(fun typ alt ->
          (* Sanity check. *)
          check_mode ~pos:__POS__ ~error_info mode typ ;
          check_mode ~pos:__POS__ ~error_info mode alt ;
          check_valid ~pos:__POS__ ~error_info typ ;
          check_valid ~pos:__POS__ ~error_info alt ;
          assert (not (is_poly typ)) ;
          phys_equal typ alt )
      && Option.is_none tri_path
    then
      stitch ~mode depth
        (Tctor {var_ident= path; var_params= params})
        (Tctor {var_ident= other_path; var_params= alts})
    else
      (* There is a distinguished third type, so tri-stitch. *)
      let tri_path = Option.value ~default:path tri_path in
      tri_stitch ~mode depth
        (Tctor {var_ident= path; var_params= params})
        (Tctor {var_ident= other_path; var_params= alts})
        (Tctor {var_ident= tri_path; var_params= alt_alts})

  (* NOTE: The types in [vars] are not modified in their alternates, to ensure
     that tri-stitchings are preserved, especially for conversions.

     This means that the variable arguments to [Tpoly] may be from a different
     mode to the type itself (which will match the mode of the polymorphised
     type).

     This ensures that we don't encounter a particularly heinous bug where we
     have the tri-stitching ['a -> 'b <-> 'c] and
       [Tpoly (Tvar "a", Tconv(Tvar "a", Tvar "b"))]
     is added to the environment along with its prover-mode alternate
       [Tpoly (Tvar "b", Tconv(Tvar "a", Tvar "b"))].
     In this scenario, instantiating the prover-mode variable ['b] with a new
     stitched variable leaks ['a] unchanged. Then, unification clobbers ['a],
     effectively causing it to become a weak type variable.

     There is now an assertion to avoid this case in [Envi.Type.copy]; see the
     [Tvar] match branch there for the particular details.
  *)
  let poly ~mode depth vars typ =
    let error_info () = ("poly", typ :: vars) in
    check_valid ~pos:__POS__ ~error_info typ ;
    assert (not (is_poly typ)) ;
    check_mode ~pos:__POS__ ~error_info mode typ ;
    let alt = type_alternate typ in
    let alt_alt = type_alternate alt in
    check_valid ~pos:__POS__ ~error_info alt_alt ;
    let get_alt_var pos mode var =
      let alt = get_mode mode var in
      check_valid ~pos:__POS__ ~error_info alt ;
      match alt.type_desc with
      | Tvar _ ->
          alt
      | _ ->
          (* This is a tri-stitched type where the stitched type variables
             have been instantiated already.
          *)
          check_mode ~pos ~error_info Checked var ;
          assert (not (phys_equal alt.type_alternate var)) ;
          var
    in
    let alts = List.map ~f:(get_alt_var __POS__ (other_mode mode)) vars in
    let alt_alts = List.map ~f:(get_alt_var __POS__ mode) alts in
    (* Sanity check: [vars] is a list of type variables. *)
    if
      equal_mode mode Prover
      || List.for_all2_exn vars alt_alts ~f:(fun typ alt ->
             (* NOTE: Don't check mode here. *)
             ( match (typ.type_desc, alt.type_desc) with
             | Tvar _, Tvar _ ->
                 ()
             | _ ->
                 assert false ) ;
             phys_equal typ alt )
         && phys_equal typ alt_alt
    then stitch ~mode depth (Tpoly (vars, typ)) (Tpoly (vars, alt))
    else
      (* The type is tri-stitched, so tri-stitch this type too. *)
      tri_stitch ~mode depth
        (Tpoly (vars, typ))
        (Tpoly (vars, alt))
        (Tpoly (vars, alt_alt))

  let conv ~mode depth typ1 typ2 =
    let error_info () = ("conv", [typ1; typ2]) in
    check_valid ~pos:__POS__ ~error_info typ1 ;
    check_valid ~pos:__POS__ ~error_info typ2 ;
    assert (not (is_poly typ1)) ;
    assert (not (is_poly typ2)) ;
    check_mode ~pos:__POS__ ~error_info Checked typ1 ;
    check_mode ~pos:__POS__ ~error_info Prover typ2 ;
    let typ_stitched =
      if are_stitched typ1 typ2 then typ1
      else stitch ~mode:Checked depth typ1.type_desc typ2.type_desc
    in
    let typ =
      stitch ~mode:Checked depth (Tconv typ_stitched) (Tconv typ_stitched)
    in
    get_mode mode typ

  let opaque ~mode depth typ =
    let error_info () = ("opaque", [typ]) in
    check_valid ~pos:__POS__ ~error_info typ ;
    check_valid ~pos:__POS__ ~error_info typ.type_alternate.type_alternate ;
    assert (not (is_poly typ)) ;
    check_mode ~pos:__POS__ ~error_info Prover typ ;
    stitch ~mode depth (Topaque typ) (Topaque typ)

  let other_mode ~mode depth typ =
    let error_info () = ("other_mode", [typ]) in
    check_valid ~pos:__POS__ ~error_info typ ;
    check_valid ~pos:__POS__ ~error_info typ.type_alternate.type_alternate ;
    assert (not (is_poly typ)) ;
    stitch ~mode depth (Tother_mode typ) (Tother_mode typ)
end

type change =
  | Depth of (type_expr * int)
  | Desc of (type_expr * type_desc)
  (* This is equivalent to [Desc], but allows for filtering the backtrace
       when [Treplace] has been set for recursion-breaking.
    *)
  | Replace of (type_expr * type_desc)

let debug_print_change fmt = function
  | Depth (typ, depth) ->
      Format.fprintf fmt "depth(id= %i, %i)" typ.type_id depth
  | Desc (typ, _) ->
      Format.fprintf fmt "desc(id= %i, _)" typ.type_id
  | Replace (typ, _) ->
      Format.fprintf fmt "replace(id= %i, _)" typ.type_id

(** Implements a weak, mutable linked-list containing the history of changes.

    Every change is added to the same list, and the snapshots correspond to
    cuts of this list.
    We gain several advantages from using a weak, mutable linked-list:
    * if there are no active snapshots, the whole list will be GC'd and any
      changes don't need to be stored at all
    * if there are active snapshots, OCaml will GC the history of changes up to
      the first active snapshot
    * all simultaneously active snapshots point to a part of the same physical
      list
    * the snapshots can be erased during backtracking, so that different
      snapshots can't be used out of order to 'restore' a state that didn't
      exist previously
*)
module Snapshot : sig
  type t

  val create : unit -> t
  (** Get a new snapshot. *)

  val add_to_history : change -> unit
  (** Add a change to the history of all active snapshots. *)

  val backtrack : t -> change list
  (** Erase the history back to the snapshot, and return the list of changes
      that occurred since, ordered from newest to oldest.
  *)

  val filtered_backtrack : f:(change -> bool) -> t -> change list
  (** Erase all changes matching the filter [f] back to the snapshot, and
      return the erased changes, ordered from newest to oldest.
  *)

  val debug_print : Format.formatter -> t -> unit

  val debug_print_latest : Format.formatter -> unit -> unit
end = struct
  type node = Change of (change * t) | LinkedChange of t | NoChange

  and t = node ref

  (* Points to the end of the current history list.

     If multiple snapshots are captured before the next change, they will all
     point to the value held here.

     If there are no snapshots active, OCaml is free to GC the value held here.
  *)
  let current = Weak.create 1

  (* Update the value held by [current] to represent the given change, and set
     current to be a new empty value.
  *)
  let add_to_history change =
    match Weak.get current 0 with
    | Some ptr ->
        let new_ptr = ref NoChange in
        ptr := Change (change, new_ptr) ;
        Weak.set current 0 (Some new_ptr)
    | None ->
        (* No snapshots active, no list to add to. *)
        ()

  let create () =
    match Weak.get current 0 with
    | Some ptr ->
        ptr
    | None ->
        let new_ptr = ref NoChange in
        Weak.set current 0 (Some new_ptr) ;
        new_ptr

  let rec collect snap =
    match !snap with
    | Change (change, ptr) ->
        change :: collect ptr
    | LinkedChange ptr ->
        collect ptr
    | NoChange ->
        []

  let debug_print fmt snap =
    let open Format in
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
      debug_print_change fmt (collect snap)

  let debug_print_latest fmt () =
    match Weak.get current 0 with
    | Some snap ->
        debug_print fmt snap
    | None ->
        Format.pp_print_list debug_print_change fmt []

  let backtrack snap =
    let current = create () in
    let rec backtrack changes ptr =
      match !ptr with
      | Change (change, ptr') ->
          ptr := LinkedChange current ;
          backtrack (change :: changes) ptr'
      | LinkedChange ptr' ->
          ptr := LinkedChange current ;
          backtrack changes ptr'
      | NoChange ->
          changes
    in
    backtrack [] snap

  let filtered_backtrack ~f snap =
    let rec backtrack changes ptrs_to_clear ptr =
      match !ptr with
      | Change (change, ptr') when f change ->
          backtrack (change :: changes) (ptr :: ptrs_to_clear) ptr'
      | Change (_change, ptr') ->
          List.iter ptrs_to_clear ~f:(fun ptr' -> ptr' := LinkedChange ptr) ;
          backtrack changes [] ptr'
      | LinkedChange ptr' ->
          backtrack changes ptrs_to_clear ptr'
      | NoChange ->
          let current = create () in
          List.iter ptrs_to_clear ~f:(fun ptr' -> ptr' := LinkedChange current) ;
          changes
    in
    backtrack [] [] snap
end

let revert = function
  | Depth (typ, depth) ->
      typ.type_depth <- depth ;
      typ.type_alternate.type_depth <- depth ;
      typ.type_alternate.type_alternate.type_depth <- depth
  | Desc (typ, desc) ->
      typ.type_desc <- desc
  | Replace (typ, desc) ->
      typ.type_desc <- desc

let backtrack snap =
  let changes = Snapshot.backtrack snap in
  List.iter ~f:revert changes

let filtered_backtrack ~f snap =
  let changes = Snapshot.filtered_backtrack ~f snap in
  List.iter ~f:revert changes

(** The representative of a type. This unfolds any [Tref] values that are
    present to get to the true underlying type.
*)
let rec repr typ = match typ.type_desc with Tref typ -> repr typ | _ -> typ

let fold ~init ~f typ =
  match typ.type_desc with
  | Tvar _ ->
      init
  | Ttuple typs ->
      List.fold ~init ~f typs
  | Tarrow (typ1, typ2, _, _) ->
      let acc = f init typ1 in
      f acc typ2
  | Tctor variant ->
      List.fold ~init ~f variant.var_params
  | Tpoly (typs, typ) ->
      let acc = List.fold ~init ~f typs in
      f acc typ
  | Tref typ ->
      f init typ
  | Tconv typ ->
      f init typ
  | Topaque typ ->
      f init typ
  | Tother_mode typ ->
      f init typ
  | Treplace _ ->
      assert false

let iter ~f = fold ~init:() ~f:(fun () -> f)

(** Make a copy of the [type_desc], using [f] as a recursor. Unfolds any
    references to make a copy of the representative's description.
*)
let rec copy_desc ~f = function
  | Tvar _ as typ ->
      typ
  | Ttuple typs ->
      Ttuple (List.map ~f typs)
  | Tarrow (typ1, typ2, explicitness, label) ->
      Tarrow (f typ1, f typ2, explicitness, label)
  | Tctor ({var_params; _} as variant) ->
      Tctor {variant with var_params= List.map ~f var_params}
  | Tpoly (typs, typ) ->
      Tpoly (List.map ~f typs, f typ)
  | Tref typ ->
      copy_desc ~f typ.type_desc
  | Tconv typ ->
      Tconv (f typ)
  | Topaque typ ->
      Topaque (f typ)
  | Tother_mode typ ->
      Tother_mode (f typ)
  | Treplace _ ->
      assert false

let rec equal_at_depth ~get_decl ~depth typ1 typ2 =
  let equal_at_depth = equal_at_depth ~get_decl ~depth in
  let typ1 = repr typ1 in
  let typ2 = repr typ2 in
  if Int.equal typ1.type_id typ2.type_id then true
  else
    match (typ1.type_desc, typ2.type_desc) with
    | Tvar _, _ when typ1.type_depth > depth ->
        true
    | _, Tvar _ when typ2.type_depth > depth ->
        true
    | Ttuple typs1, Ttuple typs2 -> (
      match List.for_all2 typs1 typs2 ~f:equal_at_depth with
      | Ok b ->
          b
      | Unequal_lengths ->
          false )
    | ( Tarrow (typ1a, typ1b, explicitness1, label1)
      , Tarrow (typ2a, typ2b, explicitness2, label2) ) ->
        equal_explicitness explicitness1 explicitness2
        && equal_arg_label label1 label2
        && equal_at_depth typ1a typ2a && equal_at_depth typ1b typ2b
    | ( Tctor ({var_ident= path1; _} as variant1)
      , Tctor ({var_ident= path2; _} as variant2) ) ->
        let decl1 = get_decl path1 in
        let decl2 = get_decl path2 in
        Int.equal decl1.tdec_id decl2.tdec_id
        && List.for_all2_exn ~f:equal_at_depth variant1.var_params
             variant2.var_params
    | Tpoly (typs1, typ1), Tpoly (typs2, typ2) -> (
      match List.for_all2 typs1 typs2 ~f:equal_at_depth with
      | Ok true ->
          equal_at_depth typ1 typ2
      | _ ->
          false )
    | _, _ ->
        false

let set_depth depth typ =
  Snapshot.add_to_history (Depth (typ, typ.type_depth)) ;
  typ.type_depth <- depth ;
  typ.type_alternate.type_depth <- depth ;
  typ.type_alternate.type_alternate.type_depth <- depth

let update_depth depth typ = if typ.type_depth > depth then set_depth depth typ

let unify_depths typ1 typ2 =
  iter ~f:(update_depth typ1.type_depth) typ2 ;
  iter ~f:(update_depth typ2.type_depth) typ1

let set_desc typ desc =
  Snapshot.add_to_history (Desc (typ, typ.type_desc)) ;
  typ.type_desc <- desc

let unsafe_set_single_replacement typ typ' =
  Snapshot.add_to_history (Replace (typ, typ.type_desc)) ;
  typ.type_desc <- Treplace typ'

let set_replacement typ typ' =
  let replace_one = unsafe_set_single_replacement in
  replace_one typ typ' ;
  replace_one typ.type_alternate typ'.type_alternate ;
  let alt_alt = typ.type_alternate.type_alternate in
  let alt_alt' = typ'.type_alternate.type_alternate in
  if (not (phys_equal typ alt_alt)) || not (phys_equal typ' alt_alt') then
    replace_one alt_alt alt_alt'

(** Backtrack only undoing the [Replace] operations since the last snapshot. *)
let backtrack_replace =
  filtered_backtrack ~f:(function Replace _ -> true | _ -> false)

(** [set_repr typ typ'] sets the representative of [typ] to be [typ']. *)
let set_repr typ typ' =
  (* Sanity check. *)
  assert (equal_mode typ.type_mode typ'.type_mode) ;
  (* Stitching is compatible. *)
  assert (
    phys_equal typ typ.type_alternate = phys_equal typ' typ'.type_alternate ) ;
  set_desc typ (Tref typ') ;
  set_desc typ.type_alternate (Tref typ'.type_alternate) ;
  set_desc typ.type_alternate.type_alternate
    (Tref typ'.type_alternate.type_alternate)

(** [choose_variable_name var typ] lifts the type variable name for [var] into
    the [type_desc] for [typ] when it is also a variable.

    Raises [AssertionError] if [var] is not a type variable.
*)
let choose_variable_name typ typ' =
  match (typ.type_desc, typ'.type_desc) with
  | Tvar (Some name), Tvar None ->
      (* We would lose the user-provided name associated with [typ], so promote
         it to be the name of [typ'].
      *)
      set_desc typ' (Tvar (Some name))
  | Tvar _, _ ->
      ()
  | _ ->
      assert false

(** [add_instance var typ'] changes the representative of the type variable
    [var] to [typ']. If [typ'] is also a type variable, then the user-provided
    of [var] is added to [typ'], unless [typ'] already has a user-provided name
    of its own.

    Raises [AssertionError] if [var] is not a type variable.
*)
let add_instance ~unify typ typ' =
  (*Format.(
    fprintf err_formatter "add_instance:@.%a@.%a@." typ_debug_print_alts typ
      typ_debug_print_alts typ') ;*)
  (* Sanity check. *)
  assert (equal_mode typ.type_mode typ'.type_mode) ;
  assert (
    phys_equal typ typ.type_alternate.type_alternate
    = phys_equal typ' typ'.type_alternate.type_alternate ) ;
  choose_variable_name typ typ' ;
  match typ.type_alternate.type_desc with
  | Tvar _ ->
      choose_variable_name typ.type_alternate typ'.type_alternate ;
      choose_variable_name typ.type_alternate.type_alternate
        typ'.type_alternate.type_alternate ;
      (* Chose again in case [typ.type_alternate.type_alternate] found a new name
       that could be propagated back to [typ].
    *)
      choose_variable_name typ typ' ;
      set_repr typ typ'
  | _ ->
      (* Variable is tri-stitched to stitched type variables which have been
       instantiated.
    *)
      assert (equal_mode Checked typ.type_mode) ;
      set_desc typ (Tref typ') ;
      unify typ.type_alternate typ'.type_alternate ;
      if not (phys_equal typ typ.type_alternate.type_alternate) then
        unify typ.type_alternate.type_alternate
          typ'.type_alternate.type_alternate

(** Create an equivalent type by unfolding all of the type representatives. *)
let flatten typ =
  let rec flatten typ =
    let typ = repr typ in
    match typ.type_desc with
    | Treplace typ' ->
        (* Recursion breaking. *)
        ( match typ.type_alternate.type_desc with
        | Treplace _ ->
            ()
        | _ ->
            (* Inconsistent state. [Envi.Type.copy] should be used to resolve
               replaced type variables in context.
            *)
            assert false ) ;
        typ'
    | Tvar _ ->
        (* Don't copy variables! *)
        typ
    | desc -> (
      match typ.type_alternate.type_desc with
      | Treplace alt ->
          (* Tri-stitching, where the stitched part has already been
               flattened.
            *)
          assert (not (phys_equal typ typ.type_alternate.type_alternate)) ;
          assert (equal_mode typ.type_mode Checked) ;
          let typ' = mk' ~mode:typ.type_mode typ.type_depth (Tvar None) in
          typ'.type_alternate <- alt ;
          unsafe_set_single_replacement typ typ' ;
          typ'.type_desc <- copy_desc ~f:flatten desc ;
          typ'
      | Tvar _ ->
          (* If the tri-stitched type isn't a type variable, this should also
             have been instantiated.
          *)
          assert false
      | _ ->
          let alt_desc = typ.type_alternate.type_desc in
          let alt_alt_desc = typ.type_alternate.type_alternate.type_desc in
          let typ' = mkvar ~mode:typ.type_mode typ.type_depth None in
          let stitched = phys_equal typ typ.type_alternate.type_alternate in
          if stitched then typ'.type_alternate.type_alternate <- typ' ;
          set_replacement typ typ' ;
          typ'.type_desc <- copy_desc ~f:flatten desc ;
          typ'.type_alternate.type_desc <- copy_desc ~f:flatten alt_desc ;
          if not stitched then
            (* tri-stitched *)
            typ'.type_alternate.type_alternate.type_desc
            <- copy_desc ~f:flatten alt_alt_desc ;
          typ' )
  in
  let snap = Snapshot.create () in
  let typ = flatten typ in
  backtrack snap ; typ

let type_vars ?depth typ =
  let deep_enough =
    match depth with
    | Some depth ->
        fun typ -> depth <= typ.type_depth
    | None ->
        fun _ -> true
  in
  let empty = Typeset.empty in
  let rec type_vars set typ =
    match typ.type_desc with
    | Tvar _ when deep_enough typ ->
        Set.add set typ
    | Tpoly (vars, typ) ->
        let poly_vars = List.fold ~init:empty vars ~f:type_vars in
        Set.union set (Set.diff (type_vars empty typ) poly_vars)
    | _ ->
        fold ~init:set typ ~f:type_vars
  in
  type_vars empty typ

let mk_option : (Type0.type_expr -> Type0.type_expr) ref =
  ref (fun _ -> failwith "mk_option not initialised")

let rec bubble_label_aux label typ =
  let {type_depth; type_mode= mode; _} = typ in
  match (repr typ).type_desc with
  | Tarrow (typ1, typ2, explicit, arr_label)
    when Int.equal (compare_arg_label label arr_label) 0 ->
      (Some (typ1, explicit, arr_label), typ2)
  | Tarrow (typ1, typ2, explicit, arr_label)
    when match (label, arr_label) with
         | Labelled lbl, Optional arr_lbl ->
             String.equal lbl arr_lbl
         | _ ->
             false ->
      (Some (!mk_option typ1, explicit, arr_label), typ2)
  | Tarrow (typ1, typ2, explicit, arr_label) -> (
    match bubble_label_aux label typ2 with
    | None, _ ->
        (None, typ)
    | res, typ2 ->
        (res, Mk.arrow ~mode ~explicit ~label:arr_label type_depth typ1 typ2) )
  | _ ->
      (None, typ)

let bubble_label label typ =
  let {type_depth; type_mode= mode; _} = typ in
  match bubble_label_aux label typ with
  | Some (typ1, explicit, label), typ2 ->
      Mk.arrow ~mode ~explicit ~label type_depth typ1 typ2
  | None, typ ->
      typ

let discard_optional_labels typ =
  let rec go typ' =
    let typ' = repr typ' in
    match typ'.type_desc with
    | Tarrow (_, typ2, _, Optional _) ->
        go typ2
    | Tarrow (_, _, _, _) ->
        typ
    | _ ->
        typ'
  in
  go typ

let is_arrow typ =
  match (repr typ).type_desc with
  | Tarrow _ | Tpoly (_, {type_desc= Tarrow _; _}) ->
      true
  | _ ->
      false

let is_var typ = match (repr typ).type_desc with Tvar _ -> true | _ -> false

let get_rev_arrow_args typ =
  let rec go args typ =
    let typ = repr typ in
    match typ.type_desc with
    | Tarrow (typ1, typ2, explicit, label) ->
        go ((typ1, explicit, label) :: args) typ2
    | _ ->
        (args, typ)
  in
  go [] typ

let rec get_rev_implicits acc typ =
  match typ.type_desc with
  | Tarrow (typ1, typ2, Implicit, label) ->
      get_rev_implicits ((label, typ1) :: acc) typ2
  | _ ->
      (acc, typ)

let get_implicits typ =
  let implicits, typ = get_rev_implicits [] typ in
  (List.rev implicits, typ)

let get_rev_implicits typ = get_rev_implicits [] typ

(** Returns [true] if [typ] is a strict subtype of [in_]
    (i.e. excluding [typ == in_]), or [false] otherwise.
*)
let contains typ ~in_ =
  let typ = repr typ in
  let equal = phys_equal typ in
  let rec contains in_ =
    let in_ = repr in_ in
    match in_.type_desc with
    | Tvar _ ->
        false
    | Ttuple typs ->
        List.exists ~f:equal typs || List.exists ~f:contains typs
    | Tarrow (typ1, typ2, _explicit, _label) ->
        equal typ1 || equal typ2 || contains typ1 || contains typ2
    | Tctor variant ->
        List.exists ~f:equal variant.var_params
        || List.exists ~f:contains variant.var_params
    | Tpoly (typs, typ) ->
        List.exists ~f:equal typs || equal typ
        || List.exists ~f:contains typs
        || contains typ
    | Tconv typ' ->
        let typ' = get_mode typ.type_mode typ' in
        equal typ' || contains typ'
    | Topaque typ' ->
        equal typ' || contains typ'
    | Tother_mode typ' ->
        equal typ' || contains typ'
    | Tref _ ->
        assert false
    | Treplace _ ->
        assert false
  in
  contains in_

(** Collapse any [Tother_mode] types until the modes are the same, if possible.

    Checked mode is always preferred over prover mode, so that tri-stitching is
    retained whenever possible.
*)
let rec get_same_mode typ1 typ2 =
  let typ1 = repr typ1 in
  let typ2 = repr typ2 in
  match (typ1.type_desc, typ2.type_desc) with
  | Tother_mode typ1', Tother_mode typ2' -> (
    match (typ1.type_mode, typ2.type_mode) with
    | Checked, Checked | Prover, Prover ->
        get_same_mode typ1' typ2'
    | Checked, Prover ->
        get_same_mode typ1 typ2'
    | Prover, Checked ->
        get_same_mode typ1' typ2 )
  | Tother_mode typ1, _ when equal_mode typ1.type_mode typ2.type_mode ->
      (repr typ1, typ2)
  | _, Tother_mode typ2 when equal_mode typ1.type_mode typ2.type_mode ->
      (typ1, repr typ2)
  | _ ->
      (typ1, typ2)

(** Remove any [Topaque] wrappers, returning the outermost non-opaque type. *)
let rec remove_opaques typ =
  let typ = repr typ in
  match typ.type_desc with Topaque typ -> remove_opaques typ | _ -> typ

(** Remove any [Tother_mode] wrappers. *)
let rec remove_mode_changes typ =
  let typ = repr typ in
  match typ.type_desc with
  | Tother_mode typ ->
      remove_mode_changes typ
  | _ ->
      typ

module Decl = struct
  let decl_id = ref 0

  let next_id () = incr decl_id ; !decl_id

  let mk ~name ~params desc =
    incr decl_id ;
    let tdec_ret =
      Mk.ctor ~mode:(Ident.mode name) 10000 (Path.Pident name) params
    in
    {tdec_params= params; tdec_desc= desc; tdec_id= !decl_id; tdec_ret}
end

open Format

let report_error ppf = function
  | Mk_wrong_mode (kind, typs, mode, typ) ->
      fprintf ppf
        "@[<hov>Internal error: Could not make a type %s from \
         types@;@[<hov2>%a@]@;The type %a was expected to have mode %a.@]"
        kind
        (pp_print_list ~pp_sep:pp_print_newline typ_debug_print)
        typs typ_debug_print typ pp_mode mode
  | Mk_invalid (kind, typs, typ) ->
      fprintf ppf
        "@[<hov>Internal error: Could not make a type %s from \
         types@;@[<hov2>%a@]@;The type %a was invalid.@]"
        kind
        (pp_print_list ~pp_sep:pp_print_newline typ_debug_print_alts)
        typs typ_debug_print_alts typ

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
    | _ ->
        None )
