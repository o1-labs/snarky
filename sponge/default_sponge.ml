open Core_kernel

module F (Field : sig
  type t

  val zero : t
end) =
struct
  let to_blocks rate field_elems =
    let n = Array.length field_elems in
    let num_blocks = if n = 0 then 1 else (n + rate - 1) / rate in
    let fill_block block_idx pos =
      let global_pos = (rate * block_idx) + pos in
      if global_pos < n then field_elems.(global_pos)
      else (* padding *) Field.zero
    in
    let create_block idx = Array.init rate ~f:(fill_block idx) in
    Array.init num_blocks ~f:create_block

  let%test_unit "empty field_elems to_blocks" =
    let blocks = to_blocks 2 [||] in
    assert (Array.length blocks = 1) ;
    [%test_eq: unit array array]
      (Array.map blocks ~f:(Array.map ~f:ignore))
      [| [| (); () |] |]

  let%test_unit "block" =
    let z = Field.zero in
    [%test_eq: unit array array]
      (Array.map (to_blocks 2 [| z; z; z |]) ~f:(Array.map ~f:ignore))
      [| [| (); () |]; [| (); () |] |]

  let sponge ~add_assign perm inputs ~rate ~state =
    Array.fold ~init:state (to_blocks rate inputs) ~f:(fun state block ->
        Array.iteri ~f:(add_assign ~state) block ;
        perm state )
end
