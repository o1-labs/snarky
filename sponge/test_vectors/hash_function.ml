open Core_kernel

(* *************** *
 *    our field    *
 * *************** *)

module Field = struct
  include Snarkette.Pasta.Fp

  (* Converts a byterray into a [Field.t], raises an exception if the number obtained is larger than the order *)
  let of_bytes (bytearray : string) : t =
    let aux i acc c =
      let big = Nat.of_int @@ int_of_char c in
      let offset = Nat.shift_left big (Int.( * ) i 8) in
      Nat.(acc + offset)
    in
    let zero = Nat.of_int 0 in
    let big = String.foldi bytearray ~init:zero ~f:aux in
    let one = Nat.of_int 1 in
    if Nat.(order - one < big) then
      failwith "the given field is larger than the order" ;
    of_bigint big

  (* Converts an hexadecimal string into a [Field.t], raises an exception if the number obtained is larger than the order *)
  let of_hex (hexstring : string) : t =
    let bytearray : string = Hex.decode hexstring |> Bytes.to_string in
    of_bytes bytearray

  (* Converts a field element into a bytearray (encoding the field element in little-endian) *)
  let to_bytes (field : t) : bytes =
    (* taken from src/lib/pickles *)
    let bits_to_bytes bits =
      let byte_of_bits bs =
        List.foldi bs ~init:0 ~f:(fun i acc b ->
            if b then acc lor (1 lsl i) else acc )
        |> Char.of_int_exn
      in
      List.map
        (List.groupi bits ~break:(fun i _ _ -> i mod 8 = 0))
        ~f:byte_of_bits
      |> Bytes.of_char_list
    in
    let bytearray = to_bits field |> bits_to_bytes in
    bytearray

  (* Converts a field element into an hexadecimal string (encoding the field element in little-endian) *)
  let to_hex (field : t) : string =
    let bytearray : bytes = to_bytes field in
    Hex.encode bytearray
end

(* ****************** *
 * legacy permutation *
 * ****************** *)

module ConfigFpLegacy = struct
  module Field = Field

  let rounds_full = 63

  let initial_ark = true

  let rounds_partial = 0

  let alpha = 5

  let to_the_alpha x =
    let open Field in
    let x_2 = x * x in
    let x_4 = x_2 * x_2 in
    let x_5 = x_4 * x in
    x_5

  module Operations = struct
    let add_assign ~state i x = Field.(state.(i) <- state.(i) + x)

    let apply_affine_map (matrix, constants) v =
      let dotv row =
        Array.reduce_exn (Array.map2_exn row v ~f:Field.( * )) ~f:Field.( + )
      in
      let res = Array.map matrix ~f:dotv in
      Array.map2_exn res constants ~f:Field.( + )

    let copy a = Array.map a ~f:Fn.id
  end
end

(* ****************** *
 * kimchi permutation *
 * ****************** *)
module ConfigFpKimchi = struct
  module Field = Field

  let rounds_full = 55

  let initial_ark = false

  let rounds_partial = 0

  let alpha = 7

  let to_the_alpha x =
    let open Field in
    let x_2 = x * x in
    let x_4 = x_2 * x_2 in
    let x_7 = x_4 * x_2 * x in
    x_7

  module Operations = struct
    let add_assign ~state i x = Field.(state.(i) <- state.(i) + x)

    let apply_affine_map (matrix, constants) v =
      let dotv row =
        Array.reduce_exn (Array.map2_exn row v ~f:Field.( * )) ~f:Field.( + )
      in
      let res = Array.map matrix ~f:dotv in
      Array.map2_exn res constants ~f:Field.( + )

    let copy a = Array.map a ~f:Fn.id
  end
end

(* ***************** *
 *   hash function   *
 * ***************** *)

module FpLegacy = struct
  include Sponge.Make_hash (Sponge.Poseidon (ConfigFpLegacy))

  let params : Field.t Sponge.Params.t =
    Sponge.Params.(map pasta_p_legacy ~f:Field.of_string)

  let hash ?init = hash ?init params

  (* input is an array of field elements encoded as hexstrings *)
  let hash_field_elems (field_elems : string list) : string =
    let input : Field.t array =
      if List.length field_elems = 0 then [||]
      else Array.of_list @@ List.map field_elems ~f:Field.of_hex
    in
    let digest = hash ~init:initial_state input in
    Field.to_hex digest
end

module FpKimchi = struct
  include Sponge.Make_hash (Sponge.Poseidon (ConfigFpKimchi))

  let params : Field.t Sponge.Params.t =
    Sponge.Params.(map pasta_p_kimchi ~f:Field.of_string)

  let hash ?init = hash ?init params

  (* input is an array of field elements encoded as hexstrings *)
  let hash_field_elems (field_elems : string list) : string =
    let input : Field.t array =
      if List.length field_elems = 0 then [||]
      else Array.of_list @@ List.map field_elems ~f:Field.of_hex
    in
    let digest = hash ~init:initial_state input in
    Field.to_hex digest
end
