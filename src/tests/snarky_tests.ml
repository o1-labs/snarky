open Alcotest

let () = run "Snarky tests" [ ("Fermat tests", Fermat.test_cases) ]
