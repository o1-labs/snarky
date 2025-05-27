open Core_kernel
open Fold_lib

(* Define Alcotest testable for int list *)
let int_list = Alcotest.(list int)

let test_fold_to_list () =
  Quickcheck.test (Quickcheck.Generator.list Int.quickcheck_generator)
    ~f:(fun xs ->
      Alcotest.(check int_list)
        "to_list(of_list(xs)) = xs" xs
        (Fold.to_list (Fold.of_list xs)) )

let test_group3 () =
  Quickcheck.test (Quickcheck.Generator.list Int.quickcheck_generator)
    ~f:(fun xs ->
      let default = 0 in
      let n = List.length xs in
      let tuples = Fold.to_list (Fold.group3 ~default (Fold.of_list xs)) in
      let k = List.length tuples in
      let r = n mod 3 in
      let padded =
        xs @ if r = 0 then [] else List.init (3 - r) ~f:(fun _ -> default)
      in
      let concated =
        List.concat_map ~f:(fun (b1, b2, b3) -> [ b1; b2; b3 ]) tuples
      in
      Alcotest.(check int_list)
        "padded list equals concatenated tuples" padded concated ;
      Alcotest.(check bool) "tuple count is correct" true ((n + 2) / 3 = k) )

let () =
  Alcotest.run "Fold_lib"
    [ ( "Fold"
      , [ Alcotest.test_case "fold_to_list" `Quick test_fold_to_list
        ; Alcotest.test_case "group3" `Quick test_group3
        ] )
    ]
