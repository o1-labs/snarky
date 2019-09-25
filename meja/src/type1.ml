open Core_kernel
open Ast_types
open Type0

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

(** Returns [true] if the [type_expr] argument is valid, false otherwise.
   Can be used to check that a type-stitching has been created or modified
   correctly.
*)
let is_valid {type_id; _} = type_id > 0

(** Equivalent to [not (is_valid typ)]. *)
let is_invalid {type_id; _} = type_id <= 0

(** Make a new type at the given mode and depth.

    The [type_alternate] field is set to the invalid value
    [none (other_mode mode)].
*)
let mk' ~mode depth type_desc =
  incr type_id ;
  { type_desc
  ; type_id= !type_id
  ; type_depth= depth
  ; type_mode= mode
  ; type_alternate= other_none mode }

(** [stitch typ typ'] sets [typ] and [typ'] as eachothers [type_alternate]s.
    Returns the first argument [typ].

    Raises [AssertionError] if the types have different modes, or if either
    type has an already-initialised [type_alternate].
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

(** Returns [true] if the types are stitched together, [false] otherwise. *)
let are_stitched typ typ' =
  phys_equal typ typ'.type_alternate && phys_equal typ' typ.type_alternate

(** Create a new type variable with the given name.

    The returned type is properly stitched to an equivalent type in the other
    mode.
*)
let mkvar ~mode depth name =
  let typ = mk' ~mode depth (Tvar name) in
  let other_typ = mk' ~mode:(other_mode mode) depth (Tvar name) in
  stitch typ other_typ

(** Get the stitched [type_expr] in the given mode. *)
let get_mode mode typ =
  if equal_mode typ.type_mode mode then typ
  else
    let typ = typ.type_alternate in
    (* Sanity check. *)
    assert (equal_mode typ.type_mode mode) ;
    typ

(** Constructors for stitched types. *)
module Mk = struct
  let var = mkvar

  let tuple ~mode depth typs =
    (* Create a dummy type variable to get proper stitching. *)
    let typ = mkvar ~mode depth None in
    let typs = List.map ~f:(get_mode mode) typs in
    typ.type_desc <- Ttuple typs ;
    let alt_typs = List.map ~f:(get_mode (other_mode mode)) typs in
    typ.type_alternate.type_desc <- Ttuple alt_typs ;
    typ

  let arrow ~mode ?(explicit = Explicit) ?(label = Nolabel) depth typ1 typ2 =
    (* Create a dummy type variable to get proper stitching. *)
    let typ = mkvar ~mode depth None in
    typ.type_desc
    <- Tarrow (get_mode mode typ1, get_mode mode typ2, explicit, label) ;
    let other_mode = other_mode mode in
    typ.type_alternate.type_desc
    <- Tarrow
         (get_mode other_mode typ1, get_mode other_mode typ2, explicit, label) ;
    typ

  (* TODO: This isn't really what we want. Integrate with proper type
           declaration stitching. *)
  let ctor ~mode depth path params =
    (* Create a dummy type variable to get proper stitching. *)
    let typ = mkvar ~mode depth None in
    typ.type_desc
    <- Tctor {var_ident= path; var_params= List.map ~f:(get_mode mode) params} ;
    (* This is usually not what we want. *)
    typ.type_alternate.type_desc
    <- Tctor
         { var_ident= path
         ; var_params= List.map ~f:(get_mode (other_mode mode)) params } ;
    typ

  (* TODO: This should be seperated out so that each mode is polymorphised over
           the type variables present in its mode.
           This will become important when we make some types opaque in Checked
           mode.
  *)
  let poly ~mode depth vars typ =
    (* Create a dummy type variable to get proper stitching. *)
    let typ' = mkvar ~mode depth None in
    typ'.type_desc
    <- Tpoly (List.map ~f:(get_mode mode) vars, get_mode mode typ) ;
    typ'.type_alternate.type_desc
    <- Tpoly
         ( List.map ~f:(get_mode (other_mode mode)) vars
         , get_mode (other_mode mode) typ ) ;
    typ'
end

type change =
  | Depth of (type_expr * int)
  | Desc of (type_expr * type_desc)
  (* This is equivalent to [Desc], but allows for filtering the backtrace
       when [Treplace] has been set for recursion-breaking.
    *)
  | Replace of (type_expr * type_desc)

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

  let backtrack snap =
    let rec backtrack changes ptr =
      match !ptr with
      | Change (change, ptr') ->
          (* Clear this snapshot so that it can't be re-used. *)
          ptr := NoChange ;
          backtrack (change :: changes) ptr'
      | LinkedChange ptr' ->
          (* Clear this snapshot so that it can't be re-used. *)
          ptr := NoChange ;
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
          List.iter ptrs_to_clear ~f:(fun ptr' -> ptr' := NoChange) ;
          changes
    in
    backtrack [] [] snap
end

let revert = function
  | Depth (typ, depth) ->
      typ.type_depth <- depth ;
      typ.type_alternate.type_depth <- depth
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
      | Treplace typ ->
          print "=== " ; typ_debug_print fmt typ ) ;
    Hash_set.remove hashtbl typ.type_id ) ;
  print " @%i)" typ.type_depth

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
  typ.type_alternate.type_depth <- depth

let update_depth depth typ = if typ.type_depth > depth then set_depth depth typ

let unify_depths typ1 typ2 =
  iter ~f:(update_depth typ1.type_depth) typ2 ;
  iter ~f:(update_depth typ2.type_depth) typ1

let set_desc typ desc =
  Snapshot.add_to_history (Desc (typ, typ.type_desc)) ;
  typ.type_desc <- desc

let set_replacement typ typ' =
  Snapshot.add_to_history (Replace (typ, typ.type_desc)) ;
  typ.type_desc <- Treplace typ' ;
  Snapshot.add_to_history
    (Replace (typ.type_alternate, typ.type_alternate.type_desc)) ;
  typ.type_alternate.type_desc <- Treplace typ'.type_alternate

(** Backtrack only undoing the [Replace] operations since the last snapshot. *)
let backtrack_replace =
  filtered_backtrack ~f:(function Replace _ -> true | _ -> false)

(** [set_repr typ typ'] sets the representative of [typ] to be [typ']. *)
let set_repr typ typ' =
  (* Sanity check. *)
  assert (equal_mode typ.type_mode typ'.type_mode) ;
  set_desc typ (Tref typ') ;
  set_desc typ.type_alternate (Tref typ'.type_alternate)

(** [add_instance var typ'] changes the representative of the type variable
    [var] to [typ']. If [typ'] is also a type variable, then the user-provided
    of [var] is added to [typ'], unless [typ'] already has a user-provided name
    of its own.

    Raises [AssertionError] if [var] is not a type variable.
*)
let add_instance typ typ' =
  let choose_name typ typ' =
    match (typ.type_desc, typ'.type_desc) with
    | Tvar (Some name), Tvar None ->
        (* We would lose the user-provided name associated with [typ], so promote
         it to be the name of [typ'].
      *)
        set_desc typ' (Tvar (Some name))
    | Tvar _, _ ->
        ()
    | _ ->
        (* Sanity check: we should be adding an instance to a type variable. *)
        assert false
  in
  choose_name typ typ' ;
  choose_name typ.type_alternate typ'.type_alternate ;
  set_repr typ typ'

(** Create an equivalent type by unfolding all of the type representatives. *)
let flatten typ =
  let rec flatten typ =
    let typ = repr typ in
    match typ.type_desc with
    | Treplace typ ->
        (* Recursion breaking. *)
        typ
    | Tvar _ ->
        (* Don't copy variables! *)
        typ
    | desc ->
        (* Placeholder variable. *)
        let typ' = mkvar ~mode:typ.type_mode typ.type_depth None in
        let alt_desc = typ.type_alternate.type_desc in
        set_replacement typ typ' ;
        (* Overwrite placeholder. *)
        typ'.type_desc <- copy_desc ~f:flatten desc ;
        typ'.type_alternate.type_desc <- copy_desc ~f:flatten alt_desc ;
        typ'
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
    | Tref _ ->
        assert false
    | Treplace _ ->
        assert false
  in
  contains in_

module Decl = struct
  let decl_id = ref 0

  let mk ~name ~params desc =
    incr decl_id ;
    let tdec_ret =
      Mk.ctor ~mode:(Ident.mode name) 10000 (Path.Pident name) params
    in
    { tdec_ident= name
    ; tdec_params= params
    ; tdec_desc= desc
    ; tdec_id= !decl_id
    ; tdec_ret }
end
