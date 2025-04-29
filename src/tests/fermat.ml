(** 
  For a given prime p where p = 2 mod 3, we can find a solution to fermat's equation
  x^3 + y^3 = z^3. 
  
  Indeed the mapping a |-> a^3 is a bijection (since 3 doesn't divide p-1),
  so fixing any (x, y) we can solve for a unique z. To solve 
  for a cubic root, we use the following:

  Say you want a cubic root of x. Define k = (2p - 1) / 3. By Fermat's little theorem, 
  we have x^(p-1) = x mod p. Thus we can compute
    (x^k)^3 = x^(2p - 1)
            = x * x^(2p - 2)
            = x * (x^(p-1))^2
            = x

  So x^k is a cubic root of x.
*)

open Core

module Make (Impl : Snarky.Snark_intf.S) = struct
  type cube_root = Solve_cube_root of Impl.Field.t

  let typ =
    let typ = Impl.Field.typ in
    Impl.Typ.transport typ
      ~there:(fun (Solve_cube_root x) -> x)
      ~back:(fun x -> Solve_cube_root x)

  type _ Snarky_backendless.Request.t +=
    | Cube_root : cube_root Snarky_backendless.Request.t

  let pow (x : Impl.Field.t) k =
    let x = Impl.Bigint.of_field x |> Impl.Bigint.to_bignum_bigint in
    Bigint.(pow x k % Impl.Field.size)
    |> Impl.Bigint.of_bignum_bigint |> Impl.Bigint.to_field

  let cube z = Impl.Field.(z * z * z)

  let cubic_root y_cubed =
    let p = Impl.Field.size in
    let k = Bigint.(((of_int 2 * p) - of_int 1) / of_int 3) in
    pow y_cubed k

  let circuit ((x, y) : Impl.field_var * Impl.field_var) : unit Impl.Checked.t =
    let open Impl.Checked.Let_syntax in
    let open Impl.Field.Checked in
    let%bind z =
      Impl.exists
        ~compute:
          Impl.As_prover.(
            map2 (read_var x) (read_var y) ~f:(fun x y ->
                cubic_root Impl.Field.(cube x + cube y) ))
        Impl.Typ.field
    in
    let cube_var a = Impl.Field.Checked.(mul a a >>= fun a2 -> mul a2 a) in
    let%bind x3 = cube_var x in
    let%bind y3 = cube_var y in
    let%bind z3 = cube_var z in
    Assert.equal (x3 + y3) z3

  let generate_witness (x, y) () =
    let input_typ = Impl.Typ.tuple2 Impl.Field.typ Impl.Field.typ in
    let return_typ = Impl.Typ.unit in
    let compiled = Impl.generate_witness ~input_typ ~return_typ circuit in

    compiled (x, y)
end

open Alcotest

module Snark = struct
  module P = struct
    (* IMPORTANT: Must true be that p == 2 (mod 3) *)
    let characteristic = Backend.Bignum_bigint.of_int 41
  end

  module Backend = Backend.Backend (P)
  module Snarky = Snarky.Snark.Make (Backend)
  module Circuit = Make (Snarky)
end

let fermat_test () =
  let xy = (Snark.Backend.Field.random (), Snark.Backend.Field.random ()) in
  let _ = Snark.Circuit.generate_witness xy () in
  ()

let cube_test () =
  let a = Snark.Backend.Field.random () in
  let a_cubed = Snark.Circuit.cube a in
  let root = Snark.Circuit.cubic_root a_cubed in
  check bool "Cube test" true (Snark.Backend.Field.equal a root)

(* Export the test cases *)
let test_cases =
  [ test_case "Cube test" `Quick cube_test
  ; test_case "Fermat circuit" `Quick fermat_test
  ]
