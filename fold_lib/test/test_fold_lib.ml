open Core_kernel
open Alcotest
open Fold_lib

let int_list = list int

let test_fold_to_list () =
  Quickcheck.test (Quickcheck.Generator.list Int.quickcheck_generator)
    ~f:(fun xs ->
      let result = Fold.to_list (Fold.of_list xs) in
      Alcotest.(check int_list) "fold-to-list" xs result )

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
      Alcotest.(check int_list) "group3 padded equals concated" padded concated ;
      Alcotest.(check int) "group3 length" ((n + 2) / 3) k )

let fold_tests =
  [ ("fold-to-list", `Quick, test_fold_to_list)
  ; ("group3", `Quick, test_group3)
  ]

let () = Alcotest.run "Fold_lib" [ ("fold", fold_tests) ]
