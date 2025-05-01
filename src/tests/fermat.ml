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

  (* The instance is given by z, the witness is (x,y) such that x^3 + y^3 = z^3 *)
  let circuit (z : Impl.field_var) : unit Impl.Checked.t =
    let open Impl.Checked.Let_syntax in
    let open Impl.Field.Checked in
    (* You have free choice for the first variable, so we just pick randomly *)
    let%bind x =
      Impl.exists
        ~compute:Impl.As_prover.(return (Impl.Field.random ()))
        Impl.Typ.field
    in
    (* Here we use the above trick for computing cube roots *)
    let%bind y =
      Impl.exists
        ~compute:
          Impl.As_prover.(
            map2 (read_var x) (read_var z) ~f:(fun x z ->
                cubic_root Impl.Field.(cube z - cube x) ))
        Impl.Typ.field
    in
    let cube_var a = Impl.Field.Checked.(mul a a >>= mul a) in
    let%bind x3 = cube_var x in
    let%bind y3 = cube_var y in
    let%bind z3 = cube_var z in
    Assert.equal (x3 + y3) z3

  let generate_witness z () =
    let input_typ = Impl.Field.typ in
    let return_typ = Impl.Typ.unit in
    let compiled = Impl.generate_witness ~input_typ ~return_typ circuit in
    compiled z
end

open Alcotest

module Fermat_snark = struct
  module P = struct
    (* IMPORTANT: Must true be that p == 2 (mod 3) *)
    let order = Backend.Bignum_bigint.of_int 41
  end

  module Backend = Backend.Backend (P)
  module S = Snarky.Snark0.Make (Backend)
  module Circuit = Make (S)
end

let fermat_test () =
  let z = Fermat_snark.Backend.Field.random () in
  let _ = Fermat_snark.Circuit.generate_witness z () in
  ()

let cube_test () =
  let a = Fermat_snark.Backend.Field.random () in
  let a_cubed = Fermat_snark.Circuit.cube a in
  let root = Fermat_snark.Circuit.cubic_root a_cubed in
  check bool "Cube test" true (Fermat_snark.Backend.Field.equal a root)

let test_cases =
  [ test_case "Cube test" `Quick cube_test
  ; test_case "Fermat circuit" `Quick fermat_test
  ]
