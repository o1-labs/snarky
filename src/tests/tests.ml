open Alcotest

(* Run all test suites *)
let () =
  run "All tests"
    [ ("Fermat tests", Fermat.test_cases)
      (* Add more test categories as you create them *)
      (* "Other tests", Other_module.test_cases; *)
    ]
