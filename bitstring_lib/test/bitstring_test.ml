open Core_kernel

(* Import the Bitstring module *)
open Bitstring_lib.Bitstring

(* Test helpers and Alcotest testable instances *)
let bool_list = Alcotest.(list bool)

let int_list = Alcotest.(list int)

let int_triple_list = Alcotest.(list (triple int int int))

(* Msb_first Module Tests *)
let test_msb_first_of_list () =
  let list = [ true; false; true; false ] in
  let bitstring = Msb_first.of_list list in
  Alcotest.check bool_list "same list" list (bitstring :> bool list)

let test_msb_first_init () =
  let length = 4 in
  let bitstring = Msb_first.init length ~f:(fun i -> i mod 2 = 0) in
  Alcotest.check bool_list "same list"
    [ true; false; true; false ]
    (bitstring :> bool list)

let test_msb_first_map () =
  let bitstring = Msb_first.of_list [ 0; 1; 2; 3 ] in
  let mapped = Msb_first.map bitstring ~f:(fun x -> x * 2) in
  Alcotest.check int_list "same list" [ 0; 2; 4; 6 ] (mapped :> int list)

let test_msb_first_pad () =
  let bitstring = Msb_first.of_list [ 1; 2; 3 ] in
  let padded = Msb_first.pad ~padding_length:2 ~zero:0 bitstring in
  Alcotest.check int_list "same list" [ 0; 0; 1; 2; 3 ] (padded :> int list)

let test_msb_first_of_lsb_first () =
  let list = [ 1; 2; 3; 4 ] in
  let lsb_first = Lsb_first.of_list list in
  (* Convert to list and then create Msb_first to work around type issues *)
  let msb_first = Msb_first.of_list (List.rev (lsb_first :> int list)) in
  Alcotest.check int_list "reversed list" [ 4; 3; 2; 1 ] (msb_first :> int list)

(* Lsb_first Module Tests *)
let test_lsb_first_of_list () =
  let list = [ true; false; true; false ] in
  let bitstring = Lsb_first.of_list list in
  Alcotest.check bool_list "same list" list (bitstring :> bool list)

let test_lsb_first_init () =
  let length = 4 in
  let bitstring = Lsb_first.init length ~f:(fun i -> i mod 2 = 0) in
  Alcotest.check bool_list "same list"
    [ true; false; true; false ]
    (bitstring :> bool list)

let test_lsb_first_map () =
  let bitstring = Lsb_first.of_list [ 0; 1; 2; 3 ] in
  let mapped = Lsb_first.map bitstring ~f:(fun x -> x * 2) in
  Alcotest.check int_list "same list" [ 0; 2; 4; 6 ] (mapped :> int list)

let test_lsb_first_pad () =
  let bitstring = Lsb_first.of_list [ 1; 2; 3 ] in
  let padded = Lsb_first.pad ~padding_length:2 ~zero:0 bitstring in
  Alcotest.check int_list "same list" [ 1; 2; 3; 0; 0 ] (padded :> int list)

let test_lsb_first_of_msb_first () =
  let list = [ 1; 2; 3; 4 ] in
  let msb_first = Msb_first.of_list list in
  (* Convert to list and then create Lsb_first to work around type issues *)
  let lsb_first = Lsb_first.of_list (List.rev (msb_first :> int list)) in
  Alcotest.check int_list "reversed list" [ 4; 3; 2; 1 ] (lsb_first :> int list)

(* Pad to Triple List Tests *)
let test_pad_to_triple_complete () =
  let list = [ 1; 2; 3; 4; 5; 6 ] in
  let triples = pad_to_triple_list ~default:0 list in
  Alcotest.check int_triple_list "complete triples"
    [ (1, 2, 3); (4, 5, 6) ]
    triples

let test_pad_to_triple_incomplete () =
  let list = [ 1; 2; 3; 4; 5 ] in
  let triples = pad_to_triple_list ~default:0 list in
  Alcotest.check int_triple_list "incomplete triple padded"
    [ (1, 2, 3); (4, 5, 0) ]
    triples

let test_pad_to_triple_single () =
  let list = [ 1 ] in
  let triples = pad_to_triple_list ~default:0 list in
  Alcotest.check int_triple_list "single element padded" [ (1, 0, 0) ] triples

let test_pad_to_triple_double () =
  let list = [ 1; 2 ] in
  let triples = pad_to_triple_list ~default:0 list in
  Alcotest.check int_triple_list "two elements padded" [ (1, 2, 0) ] triples

let test_pad_to_triple_empty () =
  let list = [] in
  let triples = pad_to_triple_list ~default:0 list in
  Alcotest.check int_triple_list "empty list" [] triples

(* Practical Examples *)

(* Example 1: Binary conversion *)
let test_binary_conversion () =
  (* Convert decimal 42 to binary (101010) MSB-first *)
  let decimal_to_binary n =
    let rec aux n acc =
      if n = 0 then acc else aux (n / 2) ((n mod 2 = 1) :: acc)
    in
    if n = 0 then [ false ] else aux n []
  in

  let binary_42 = decimal_to_binary 42 in
  let bitstring = Msb_first.of_list binary_42 in

  (* Convert back to decimal *)
  let binary_to_decimal bits =
    List.foldi bits ~init:0 ~f:(fun i acc bit ->
        if bit then acc + (1 lsl (List.length bits - i - 1)) else acc )
  in

  let result = binary_to_decimal (bitstring :> bool list) in
  Alcotest.(check int) "decimal to binary and back" 42 result

(* Example 2: ASCII text encoding/decoding *)
let test_ascii_encoding () =
  (* Convert a char to a list of 8 bits (MSB first) *)
  let char_to_bits c =
    let code = Char.to_int c in
    let bits = List.init 8 ~f:(fun i -> code land (1 lsl (7 - i)) <> 0) in
    bits
  in

  (* Convert 8 bits to a char *)
  let bits_to_char bits =
    List.foldi bits ~init:0 ~f:(fun i acc bit ->
        if bit then acc lor (1 lsl (7 - i)) else acc )
    |> Char.of_int_exn
  in

  let text = "hello" in

  (* Encode each character to bits *)
  let all_bits =
    String.to_list text |> List.map ~f:char_to_bits |> List.concat
  in

  let encoded = Msb_first.of_list all_bits in

  (* Decode bits back to text *)
  let bit_groups =
    List.groupi (encoded :> bool list) ~break:(fun i _ _ -> i mod 8 = 0)
  in

  let decoded = bit_groups |> List.map ~f:bits_to_char |> String.of_char_list in

  Alcotest.(check string) "ASCII encoding/decoding" text decoded

(* Example 3: Bitwise operations *)
let test_bitwise_operations () =
  (* Create two 8-bit numbers *)
  let a =
    Msb_first.of_list [ true; false; true; true; false; false; true; false ]
  in
  (* 0b10110010 = 178 *)
  let b =
    Msb_first.of_list [ false; true; true; false; true; false; true; true ]
  in

  (* 0b01101011 = 107 *)

  (* Bitwise operations *)
  let bitwise_and =
    List.map2_exn (a :> bool list) (b :> bool list) ~f:(fun x y -> x && y)
    |> Msb_first.of_list
  in

  let bitwise_or =
    List.map2_exn (a :> bool list) (b :> bool list) ~f:(fun x y -> x || y)
    |> Msb_first.of_list
  in

  let bitwise_xor =
    List.map2_exn
      (a :> bool list)
      (b :> bool list)
      ~f:(fun x y -> (x && not y) || ((not x) && y))
    |> Msb_first.of_list
  in

  (* Convert to decimal for easier verification *)
  let to_decimal bits =
    let bit_list = (bits :> bool list) in
    List.foldi bit_list ~init:0 ~f:(fun i acc bit ->
        if bit then acc + (1 lsl (List.length bit_list - i - 1)) else acc )
  in

  (* Verify operations *)
  Alcotest.(check int)
    "Bitwise AND" (178 land 107)
    (to_decimal (bitwise_and :> bool list)) ;
  Alcotest.(check int)
    "Bitwise OR" (178 lor 107)
    (to_decimal (bitwise_or :> bool list)) ;
  Alcotest.(check int)
    "Bitwise XOR" (178 lxor 107)
    (to_decimal (bitwise_xor :> bool list))

(* Main test runner *)
let () =
  Alcotest.run "Bitstring_lib Tests"
    [ ( "Msb_first"
      , [ ("of_list", `Quick, test_msb_first_of_list)
        ; ("init", `Quick, test_msb_first_init)
        ; ("map", `Quick, test_msb_first_map)
        ; ("pad", `Quick, test_msb_first_pad)
        ; ("of_lsb_first", `Quick, test_msb_first_of_lsb_first)
        ] )
    ; ( "Lsb_first"
      , [ ("of_list", `Quick, test_lsb_first_of_list)
        ; ("init", `Quick, test_lsb_first_init)
        ; ("map", `Quick, test_lsb_first_map)
        ; ("pad", `Quick, test_lsb_first_pad)
        ; ("of_msb_first", `Quick, test_lsb_first_of_msb_first)
        ] )
    ; ( "pad_to_triple_list"
      , [ ("complete triples", `Quick, test_pad_to_triple_complete)
        ; ("incomplete triple", `Quick, test_pad_to_triple_incomplete)
        ; ("single element", `Quick, test_pad_to_triple_single)
        ; ("two elements", `Quick, test_pad_to_triple_double)
        ; ("empty list", `Quick, test_pad_to_triple_empty)
        ] )
    ; ( "Examples"
      , [ ("binary conversion", `Quick, test_binary_conversion)
        ; ("ASCII encoding/decoding", `Quick, test_ascii_encoding)
        ; ("bitwise operations", `Quick, test_bitwise_operations)
        ] )
    ]
