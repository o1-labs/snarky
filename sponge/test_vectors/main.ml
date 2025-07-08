open Core_kernel
open Alcotest

type test_vector = { input : string list; output : string }

type test_vectors = { name : string; test_vectors : test_vector list }

let parse_test_vectors filepath =
  let json = Yojson.Basic.from_file filepath in
  let open Yojson.Basic.Util in
  let name = json |> member "name" |> to_string in
  let test_vectors = json |> member "test_vectors" |> to_list in
  let json_to_test_vector test_vector =
    let input = test_vector |> member "input" |> to_list |> filter_string in
    let output = test_vector |> member "output" |> to_string in
    { input; output }
  in
  let test_vectors = List.map test_vectors ~f:json_to_test_vector in
  { name; test_vectors }

let test_legacy_vectors () =
  let cur_dir = Sys.getcwd () in
  let test_vector_file = Filename.concat cur_dir "legacy.json" in
  let test_vectors = parse_test_vectors test_vector_file in
  check string "legacy name" "legacy" test_vectors.name ;
  let check_test_vector test_vector =
    let digest = Hash_function.FpLegacy.hash_field_elems test_vector.input in
    check string "legacy hash" test_vector.output digest
  in
  List.iter test_vectors.test_vectors ~f:check_test_vector

let test_kimchi_vectors () =
  let cur_dir = Sys.getcwd () in
  let test_vector_file = Filename.concat cur_dir "kimchi.json" in
  let test_vectors = parse_test_vectors test_vector_file in
  check string "kimchi name" "kimchi" test_vectors.name ;
  let check_test_vector test_vector =
    let digest = Hash_function.FpKimchi.hash_field_elems test_vector.input in
    check string "kimchi hash" test_vector.output digest
  in
  List.iter test_vectors.test_vectors ~f:check_test_vector

let test_cases =
  [ ("legacy test vectors", [ test_case "legacy" `Quick test_legacy_vectors ])
  ; ("kimchi test vectors", [ test_case "kimchi" `Quick test_kimchi_vectors ])
  ]

let () = run "Sponge test vectors" test_cases
