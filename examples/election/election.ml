open Core
open Impl
open Import

(* First we declare the type of "votes" and call a library functor [Enumerable] to
   make it possible to refer to values of type [Vote.t] in checked computations. *)
module Vote = struct
  module T = struct
    type t = Pepperoni | Mushroom [@@deriving enum]
  end

  include T
  include Enumerable (T)
end

module Ballot = struct
  module Opened = struct
    module Nonce = Field

    (* An opened ballot is a nonce along with a vote. *)
    type t = Nonce.t * Vote.t

    type var = Nonce.Var.t * Vote.var

    (* A [typ] is a kind of specification of a type of data which makes it possible
       to use values of that type inside checked computations. In a future version of
       the library, [typ]s will be derived automatically with a ppx extension. *)
    let typ = Typ.(Nonce.typ * Vote.typ)

    let to_bits (nonce, vote) = Nonce.to_bits nonce @ Vote.to_bits vote

    (* This is our first explicit example of a checked computation. It simply says that to
   convert an opened ballot into bits, one converts the nonce and the vote into bits and
   concatenates them. Behind the scenes, this function would set up all the constraints
   necessary to certify the correctness of this computation with a snark. *)
    let var_to_bits (nonce, vote) =
      let%map nonce_bits = Nonce.var_to_bits nonce
      and vote_bits = Vote.var_to_bits vote in
      nonce_bits @ vote_bits

    let create vote : t = (Field.random (), vote)
  end

  (* A closed ballot is simply a hash *)
  module Closed = Hash
end

let rec deep_hash_unchecked i bits =
  if i > 0 then
    let bits = Hash.to_bits (Hash.hash bits) in
    let x = Field.project bits in
    let x = Field.Infix.(x * x) in
    let bits = Field.unpack x in
    deep_hash_unchecked (i-1) bits
  else
    Hash.hash bits

let rec deep_hash i bits =
  if i > 0 then
    let%bind x = Hash.hash_var bits in
    let%bind bits = Hash.var_to_bits x in
    let x = Field.Var.project bits in
    let%bind x = Field.Checked.mul x x in
    let%bind bits = Field.Checked.choose_preimage_var ~length:Field.size_in_bits x in
    deep_hash (i-1) bits
  else
    Hash.hash_var bits

let check_deep_hash i x out_expected =
  let%bind out = deep_hash i x in
  Hash.assert_equal out out_expected

let hash_depth = 80

let input_size = 80

let input = List.init input_size ~f:(fun _ -> Random.bool ())


let proof_system =
  Proof_system.create
    ~public_input:[Typ.list ~length:input_size Boolean.typ; Hash.typ]
    (check_deep_hash hash_depth)

let keypair = Proof_system.generate_keypair proof_system

let check_proof_system input =
  let output = deep_hash_unchecked hash_depth input in
  ignore (Proof_system.prove ~proving_key:(Keypair.pk keypair) ~public_input:[input; output] proof_system ())

let%bench_fun "check_proof_system" = fun () -> check_proof_system input

let check_proof_system_precomp input output =
  ignore (Proof_system.prove ~proving_key:(Keypair.pk keypair) ~public_input:[input; output] proof_system ())

let%bench_fun "check_proof_system_precomp" =
  let output = deep_hash_unchecked hash_depth input in
  fun () -> check_proof_system_precomp input output

let proof_system =
  Proof_system.create
    ~public_input:[Typ.list ~length:input_size Boolean.typ]
    ~public_output:{typ= Hash.typ; assert_equal= Hash.assert_equal}
    (deep_hash hash_depth)

let keypair = Proof_system.generate_keypair proof_system

let check_proof_system_output input =
  ignore (Proof_system.prove ~proving_key:(Keypair.pk keypair) ~public_input:[input] proof_system ())

let%bench_fun "check_proof_system_output" = fun () -> check_proof_system input
