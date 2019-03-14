open Snarky
open Snark

(* Use a module expression *)
let test (type f) ((module I) : f m) x y =
  let open I.Field in
  let z = x / y in
  Assert.equal x (y * z) ;
  z

module T (Intf : Snark_intf.Run) = struct
  open Intf

  let test x y z =
    (* Call a module expression *)
    let a = test ((module Intf) : Field.Constant.t m) x y in
    (* Call within a functor *)
    Field.(x * y * z * a)
end

(* Pass through functors *)
module T1 (Intf : Snark_intf.Run) = struct
  module T = T (Intf)

  let test x = T.test x x x
end

(* Generic result module type *)
module type Res = sig
  type t

  val res : t
end

(* Type wrapping [Res] *)
type 'a res = (module Res with type t = 'a)

(* Call within a functor using a module expression *)
let test2 (type f) ((module I) : f m) x =
  let ((module M) : f Cvar.t res) =
    ( module struct
      module T1 = T1 (I)

      type t = f Cvar.t

      let res = T1.test x
    end )
  in
  M.res

let test3 (type f) ((module I) : f m) x = test (module I) x I.Field.one

let prove () =
  let ((module I) as i) = make (module Backends.Mnt4.GM) in
  let open I in
  let exposing = Data_spec.[Field.typ] in
  let f x () = test2 i x in
  let keys = generate_keypair ~exposing f in
  let proof = prove (Keypair.pk keys) exposing f (Field.Constant.of_int 17) in
  (Proof.to_string proof, Verification_key.to_bigstring (Keypair.vk keys))

let verify proof vk =
  let (module I) = make (module Backends.Mnt4.GM) in
  let open I in
  let exposing = Data_spec.[Field.typ] in
  let proof = Proof.of_string proof in
  let vk = Verification_key.of_bigstring vk in
  verify proof vk exposing (Field.Constant.of_int 17)

module Intf = Snark.Run.Make (Backends.Mnt4.GM)

module Old = struct
  module M = Snark.Make (Backends.Mnt4.GM)
  open M

  let f = Field.Checked.mul

  let foo x y =
    let%bind z = f x y in
    Intf.make_checked (fun () -> test2 (module Intf) z)
end

module As_prover_test = struct
  open Intf

  let test x =
    let y = Field.div x x in
    fun () ->
      let x' = As_prover.read_var x in
      let y' = As_prover.read_var y in
      let z = Field.Constant.Infix.(x' / x') in
      (x, y', z)

  let a, b = Field.Constant.(random (), random ())

  let (x, y, z) =
    run_and_check (fun () ->
    let open Intf in
    let test_2 = test (Field.of_int 2) in
    as_prover (fun () -> ignore (test_2 ()));
    Field.(Assert.equal one one);
    exists ~compute:(fun () -> ()) Typ.unit;
    let c = Field.(constant a * constant b) in
    let as_prover = test c in
    as_prover)
  |> Core.Or_error.ok_exn

  let () =
    (* We are still considered to be the prover at the end of 'run_and_check',
       at least until the next checked computation is run. *)
    let c = Field.Constant.Infix.(a * b) in
    assert (As_prover.in_prover_block ());
    assert Field.Constant.(equal (As_prover.read_var x) c);
    assert Field.Constant.(equal y one);
    assert Field.Constant.(equal z one)

  let test2 =
    run_and_check (fun () ->
      let res = Field.(one * one * one * one * one * one * one) in
      fun () -> As_prover.read_var res)
    |> Core.Or_error.ok_exn

  let () = assert Field.Constant.(equal test2 one)

  let test3 =
    run_and_check (fun () ->
      let two = Field.of_int 2 in
      let res = Field.(two * two * two * two * two * two * two) in
      fun () -> As_prover.read_var res)
    |> Core.Or_error.ok_exn

  let () = assert Field.Constant.(equal test3 (of_int 128))
end

let exposing = Intf.(Data_spec.[Field.typ])

let prove2 () =
  let open Intf in
  let f x () = test3 (module Intf) x in
  let keys = generate_keypair ~exposing f in
  let proof = prove (Keypair.pk keys) exposing f (Field.Constant.of_int 39) in
  (Proof.to_string proof, Verification_key.to_bigstring (Keypair.vk keys))

let verify2 proof vk =
  let open Intf in
  let proof = Proof.of_string proof in
  let vk = Verification_key.of_bigstring vk in
  verify proof vk exposing (Field.Constant.of_int 29)

let main () =
  let proof, vk = prove () in
  let proof2, vk2 = prove2 () in
  Format.printf "expecting true:%B expecting false:%B@." (verify proof vk)
    (verify2 proof2 vk2)

let () = main ()
