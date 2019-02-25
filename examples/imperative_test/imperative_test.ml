open Snarky
open Snark

(* Use a module expression *)
let test (type f v) ((module I) : (f, v) m) x y =
  let open I.Field in
  let z = x / y in
  Assert.equal x (y * z) ;
  z

module T (Intf : Snark_intf.Run) = struct
  open Intf

  let test x y z =
    (* Call a module expression *)
    let a = test ((module Intf) : (Field.Constant.t, Var.t) m) x y in
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
let test2 (type f v) ((module I) : (f, v) m) x =
  let ((module M) : f Cvar.t res) =
    ( module struct
      module T1 = T1 (I)

      type t = f Cvar.t

      let res = T1.test x
    end )
  in
  M.res

let test3 (type f v) ((module I) : (f, v) m) x = test (module I) x I.Field.one

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
  Format.printf "expecting true:%B expecting false:%B" (verify proof vk)
    (verify2 proof2 vk2)

let () = main ()
