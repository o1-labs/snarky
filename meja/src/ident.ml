open Core_kernel

type t = {ident_id: int; ident_name: string} [@@deriving sexp]

type ident = t

let current_id = ref 0

let create name =
  incr current_id ;
  {ident_id= !current_id; ident_name= name}

let name {ident_name= name; _} = name

let compare {ident_id= id1; _} {ident_id= id2; _} = Int.compare id1 id2

module Table = struct
  type 'a t = (ident * 'a) list String.Map.t

  let add ~key:ident ~data tbl =
    Map.change tbl (name ident) ~f:(function
      | Some row ->
          Some ((ident, data) :: row)
      | None ->
          Some [(ident, data)] )

  let remove ident tbl =
    Map.change tbl (name ident) ~f:(function
      | Some row ->
          let row =
            List.filter row ~f:(fun (ident2, _) ->
                Int.equal (compare ident ident2) 0 )
          in
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

  let find_name name tbl = Option.bind ~f:List.hd (Map.find tbl name)
end
