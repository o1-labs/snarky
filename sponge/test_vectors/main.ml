open Core_kernel

type test_vector = {input: string list; output: string}

type test_vectors = {name: string; test_vectors: test_vector list}

let parse_test_vectors filepath =
  let json = Yojson.Basic.from_file filepath in
  let open Yojson.Basic.Util in
  let name = json |> member "name" |> to_string in
  let test_vectors = json |> member "test_vectors" |> to_list in
  let json_to_test_vector test_vector =
    let input = test_vector |> member "input" |> to_list |> filter_string in
    let output = test_vector |> member "output" |> to_string in
    {input; output}
  in
  let test_vectors = List.map test_vectors ~f:json_to_test_vector in
  {name; test_vectors}

(* three_wire test vectors *)
let () =
  let cur_dir = Sys.getcwd () in
  let test_vector_file = Filename.concat cur_dir "three_wire.json" in
  let test_vectors = parse_test_vectors test_vector_file in
  assert (String.equal test_vectors.name "three_wire") ;
  let check_test_vector test_vector =
    let digest = Hash_function.ThreeWire.hash_field_elems test_vector.input in
    assert (digest = test_vector.output)
  in
  List.iter test_vectors.test_vectors ~f:check_test_vector

(* fp_3 test vectors *)
let () =
  let cur_dir = Sys.getcwd () in
  let test_vector_file = Filename.concat cur_dir "fp_3.json" in
  let test_vectors = parse_test_vectors test_vector_file in
  assert (String.equal test_vectors.name "fp_3") ;
  let check_test_vector test_vector =
    let digest = Hash_function.Fp3.hash_field_elems test_vector.input in
    assert (digest = test_vector.output)
  in
  List.iter test_vectors.test_vectors ~f:check_test_vector
