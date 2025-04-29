(** 
  For a given prime p where p = 2 mod 3, we can find a solution to fermat's equation
  x^3 + y^3 = z^3. 
  
  Indeed the mapping x -> x^3 is a bijection (since 3 doesn't divide p-1),
  so fixing any (a, c) we can solve for a unique y^3 = c^3 - a^3. To solve 
  for a cubic root, we use the following:

  Define k = (2p - 1) / 3. By Fermat's little theorem, we have x^(p-1) = x mod p. 
  Thus we can compute
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

  (* TODO: Why isn't this defined in the field interface ? *)
  let pow x n =
    let k = Impl.Bigint.length_in_bytes * 8 in
    let rec go acc i =
      if Int.(i < 0) then acc
      else
        let acc = Impl.Field.square acc in
        let acc =
          if Impl.Bigint.test_bit n i then Impl.Field.(acc * x) else acc
        in
        go acc Int.(i - 1)
    in
    go Impl.Field.one Int.(k - 1)


  let cube z = Impl.Field.(z * z * z)

  let cubic_root y_cubed =
    let p = Impl.Field.size in
    let k =
      Impl.Bigint.of_bignum_bigint
        Bigint.(((of_int 2 * p) - of_int 1) / of_int 3)
    in
    pow y_cubed k

  let circuit ((a,c) : Impl.field_var * Impl.field_var): unit Impl.Checked.t =
    let open Impl.Checked.Let_syntax in
    let open Impl.Field.Checked in
    let%bind b =
      Impl.exists
        ~compute:
          Impl.As_prover.(
            map2 (read_var a) (read_var c) ~f:(fun a c ->
                cubic_root Impl.Field.(cube c - cube a) ))
        Impl.Typ.field
    in
    let cube z = Impl.Field.Checked.(mul z z >>= fun z2 -> mul z2 z) in
      let%bind a_cubed = cube a in
      let%bind b_cubed = cube b in
      let%bind c_cubed = cube c in
      Assert.equal (a_cubed + b_cubed) c_cubed


  let generate_witness () =
    let input_typ = Impl.Typ.tuple2 Impl.Field.typ Impl.Field.typ in
    let return_typ = Impl.Typ.unit in
    let compiled =
      Impl.generate_witness ~input_typ ~return_typ circuit
    in
  
    compiled (Impl.Field.one, Impl.Field.one) 
end

open Alcotest

module Snark = struct 
  module P = struct
    let characteristic = Backend.Bignum_bigint.of_int 5
  end

  module Backend = Backend.Backend (P)
  module Snarky = Snarky.Snark.Make (Backend)
  module Circuit = Make (Snarky)
end

let fermat_test () =
  let _ = Snark.Circuit.generate_witness () in
  ()

(* Export the test cases *)
let test_cases = [ test_case "Test name" `Quick fermat_test ]