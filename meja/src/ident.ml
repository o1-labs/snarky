open Core_kernel

type t =
  | Regular of
      { ident_id: int
      ; ident_name: string
      ; ident_ocaml: string ref option
      ; ident_mode: Ast_types.mode }
  | Global of string
  | Row of string
[@@deriving sexp]

type ident = t [@@deriving sexp]

let current_id = ref 0

let create ~mode ?(ocaml = false) name =
  incr current_id ;
  Regular
    { ident_id= !current_id
    ; ident_name= name
    ; ident_ocaml= (if ocaml then Some (ref name) else None)
    ; ident_mode= mode }

let create_global name = Global name

let create_row name = Row name

let is_global = function Global _ -> true | _ -> false

let is_row = function Row _ -> true | _ -> false

let name = function
  | Regular {ident_name= name; _} ->
      name
  | Global name ->
      name
  | Row name ->
      name

let ocaml_name = function
  | Regular {ident_ocaml= Some name; _} ->
      !name
  | id ->
      name id

let ocaml_name_ref = function
  | Regular {ident_ocaml; _} ->
      ident_ocaml
  | _ ->
      None

let mode = function
  | Regular {ident_mode= mode; _} ->
      mode
  | Global _ ->
      Checked
  | Row _ ->
      failwith "Cannot derive a mode from row identifiers."

let compare ident1 ident2 =
  match (ident1, ident2) with
  | Regular {ident_id= id1; _}, Regular {ident_id= id2; _} ->
      Int.compare id1 id2
  | Regular _, _ ->
      -1
  | _, Regular _ ->
      1
  | Global name1, Global name2 ->
      String.compare name1 name2
  | Global _, _ ->
      -1
  | _, Global _ ->
      1
  | Row name1, Row name2 ->
      String.compare name1 name2

let equal ident1 ident2 =
  match (ident1, ident2) with
  | Regular {ident_id= id1; _}, Regular {ident_id= id2; _} ->
      Int.equal id1 id2
  | Global name1, Global name2 ->
      String.equal name1 name2
  | Row name1, Row name2 ->
      String.equal name1 name2
  | (Regular _ | Global _ | Row _), _ ->
      (* NOTE: This case is expanded to ensure that the match is not exhaustive
               if we add new identifier kinds.
      *)
      false

let pprint fmt = function
  | Regular {ident_name= name; _} ->
      Ast_types.pp_name fmt name
  | Global name ->
      Format.pp_print_string fmt name
  | Row name ->
      Format.fprintf fmt "`%s" name

let debug_print fmt = function
  | Regular {ident_name; ident_ocaml= Some ident_ocaml; ident_id; ident_mode}
    when not (String.equal ident_name !ident_ocaml) ->
      Format.fprintf fmt "(%s=%s)/%a.%i" ident_name !ident_ocaml
        Ast_types.mode_debug_print ident_mode ident_id
  | Regular {ident_name; ident_ocaml= _; ident_id; ident_mode} ->
      Format.fprintf fmt "%s/%a.%i" ident_name Ast_types.mode_debug_print
        ident_mode ident_id
  | Global name ->
      Format.fprintf fmt "%s/G" name
  | Row name ->
      Format.fprintf fmt "`%s" name

let fresh_id = ref 0

let fresh mode = create ~mode (sprintf "x___%i" (incr fresh_id ; !fresh_id))

module Table = struct
  type 'a t = (ident * 'a) list String.Map.t

  let empty = String.Map.empty

  let is_empty = String.Map.is_empty

  let remove_from_row ident =
    List.filter ~f:(fun (ident2, _) -> not (Int.equal (compare ident ident2) 0))

  let add ~key:ident ~data tbl =
    Map.change tbl (name ident) ~f:(function
      | Some row ->
          Some ((ident, data) :: remove_from_row ident row)
      | None ->
          Some [(ident, data)] )

  let remove ident tbl =
    Map.change tbl (name ident) ~f:(function
      | Some row ->
          let row = remove_from_row ident row in
          if List.is_empty row then None else Some row
      | None ->
          None )

  let find ident tbl =
    match Map.find tbl (name ident) with
    | Some row ->
        List.find_map row ~f:(fun (ident2, data) ->
            if Int.equal (compare ident ident2) 0 then Some data else None )
    | None ->
        None

  let find_name name ~modes tbl =
    Option.bind
      ~f:(List.find ~f:(fun (ident, _) -> modes (mode ident)))
      (Map.find tbl name)

  let first_exn tbl = List.hd_exn (snd (Map.min_elt_exn tbl))

  let keys tbl = List.concat_map ~f:(List.map ~f:fst) (Map.data tbl)

  let foldi tbl ~init ~f =
    Map.fold tbl ~init ~f:(fun ~key:_ ~data init ->
        List.fold ~init data ~f:(fun init (ident, data) -> f ident init data)
    )

  let fold tbl ~init ~f = foldi tbl ~init ~f:(fun _key -> f)

  let fold_keys tbl ~init ~f =
    foldi tbl ~init ~f:(fun key init _value -> f init key)

  let fold2_names tbl1 tbl2 ~init ~f =
    Map.fold2 tbl1 tbl2 ~init ~f:(fun ~key ~data acc ->
        let data =
          match data with
          | `Both ((_, v1) :: _, (_, v2) :: _) ->
              `Both (v1, v2)
          | `Left ((_, v1) :: _) ->
              `Left v1
          | `Right ((_, v2) :: _) ->
              `Right v2
          | _ ->
              assert false
        in
        f ~key ~data acc )

  let merge_skewed_names tbl1 tbl2 ~combine =
    Map.merge_skewed tbl1 tbl2 ~combine:(fun ~key v1 v2 ->
        let res = combine ~key (List.hd_exn v1) (List.hd_exn v2) in
        if not (String.equal (name (fst res)) key) then
          failwith
            "merge_skewed_names: The name provided by combine does not match \
             the key" ;
        (res :: v1) @ v2 )

  let map tbl ~f =
    Map.map ~f:(List.map ~f:(fun (ident, data) -> (ident, f data))) tbl

  let mapi tbl ~f =
    Map.map ~f:(List.map ~f:(fun (ident, data) -> (ident, f ident data))) tbl
end

module T = struct
  type t = ident [@@deriving sexp]

  let compare = compare
end

module Set = Set.Make (T)
module Map = Map.Make (T)
