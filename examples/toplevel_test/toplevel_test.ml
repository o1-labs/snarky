open Core_kernel
open Snarky
module Impl = Snark.Run.Make (Backends.Mnt4.GM)
open Impl

let () = Printexc.record_backtrace true

let read = As_prover.read Field.typ

let x = Field.of_int 15

let y = Field.of_int 25

let z = Field.(x * y)

let () = Field.Constant.print (read z)

let test_function x y z = Field.(x * y * z)

let assertion x y z expected () =
  let res = test_function x y z in
  Field.Assert.equal res expected ;
  res

let proof_system =
  Proof_system.create
    ~public_input:[Field.typ; Field.typ; Field.typ; Field.typ]
    assertion

let expected = test_function x y z

let proof =
  Proof_system.prove
    ~public_input:[read x; read y; read z; read expected]
    proof_system

let () =
  Format.printf "Created proof that test_function %s %s %s = %s@."
    (Field.Constant.to_string (read x))
    (Field.Constant.to_string (read y))
    (Field.Constant.to_string (read z))
    (Field.Constant.to_string (read expected))

let verified =
  Proof_system.verify
    ~public_input:[read x; read y; read z; read expected]
    proof_system proof

let () =
  if verified then
    Format.printf "Verified proof that test_function %s %s %s = %s@."
      (Field.Constant.to_string (read x))
      (Field.Constant.to_string (read y))
      (Field.Constant.to_string (read z))
      (Field.Constant.to_string (read expected))
  else
    Format.printf
      "Could not verify the proof that test_function %s %s %s = %s@."
      (Field.Constant.to_string (read x))
      (Field.Constant.to_string (read y))
      (Field.Constant.to_string (read z))
      (Field.Constant.to_string (read expected))

let res =
  Or_error.ok_exn
  @@ Proof_system.run_checked
       ~public_input:[read x; read y; read z; read expected]
       proof_system

let () =
  let res = read res in
  let expected = read expected in
  if Field.Constant.equal res expected then
    Format.printf "Ran successfully and got %s = %s as expected@."
      (Field.Constant.to_string res)
      (Field.Constant.to_string expected)
  else
    Format.printf "Unexpected failure: got %s <> %s@."
      (Field.Constant.to_string res)
      (Field.Constant.to_string expected)

let constraint_system = Proof_system.constraint_system proof_system

let () =
  Format.printf
    "Generated the constraint system correctly and still read expected=%s@."
    (Field.Constant.to_string (read expected))

let keys = Proof_system.generate_keypair proof_system

let () =
  Format.printf "Generated a keypair and still read expected=%s@."
    (Field.Constant.to_string (read expected))

let exists_test x y z () =
  let expected =
    exists Field.typ ~compute:(fun () ->
        let var = test_function x y z in
        As_prover.read Field.typ var )
  in
  assertion x y z expected ()

let proof_system2 =
  Proof_system.create
    ~public_input:[Field.typ; Field.typ; Field.typ]
    exists_test

let proof2 =
  Proof_system.prove ~public_input:[read x; read y; read z] proof_system2

let () =
  Format.printf "Created proof that test_function %s %s %s = ?@."
    (Field.Constant.to_string (read x))
    (Field.Constant.to_string (read y))
    (Field.Constant.to_string (read z))

let verified2 =
  Proof_system.verify ~public_input:[read x; read y; read z] proof_system2
    proof2

let () =
  if verified2 then
    Format.printf "Verified proof that test_function %s %s %s = ?@."
      (Field.Constant.to_string (read x))
      (Field.Constant.to_string (read y))
      (Field.Constant.to_string (read z))
  else
    Format.printf
      "Could not verify the proof that test_function %s %s %s = ?@."
      (Field.Constant.to_string (read x))
      (Field.Constant.to_string (read y))
      (Field.Constant.to_string (read z))
