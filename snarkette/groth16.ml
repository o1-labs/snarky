open Core_kernel

module type Backend_intf = sig
  module N : Nat_intf.S

  module Fq : Fields.Fp_intf with module Nat := N

  module Fqe : Fields.Extension_intf with type base = Fq.t

  module G1 : sig
    type t [@@deriving sexp, bin_io]

    val zero : t

    val to_affine_exn : t -> Fq.t * Fq.t

    val is_well_formed : t -> bool

    val ( * ) : N.t -> t -> t

    val ( + ) : t -> t -> t
  end

  module G2 : sig
    type t [@@deriving sexp, bin_io]

    val one : t

    val to_affine_exn : t -> Fqe.t * Fqe.t

    val ( + ) : t -> t -> t

    val is_well_formed : t -> bool
  end

  module Fq_target : sig
    include Fields.Degree_2_extension_intf with type base = Fqe.t

    val unitary_inverse : t -> t
  end

  module Pairing :
    Pairing.S
    with module G1 := G1
     and module G2 := G2
     and module Fq_target := Fq_target
end

module Make (Backend : Backend_intf) = struct
  open Backend

  module Verification_key = struct
    type t = {query: G1.t array; delta: G2.t; alpha_beta: Fq_target.t}
    [@@deriving bin_io, sexp]

    type vk = t

    module Processed = struct
      type t =
        { query: G1.t array
        ; alpha_beta: Fq_target.t
        ; delta: Pairing.G2_precomputation.t }
      [@@deriving bin_io, sexp]

      let create ({query; alpha_beta; delta} : vk) =
        {alpha_beta; delta= Pairing.G2_precomputation.create delta; query}
    end
  end

  let check b lab = if b then Ok () else Or_error.error_string lab

  module Proof = struct
    type t = {a: G1.t; b: G2.t; c: G1.t} [@@deriving bin_io, sexp]

    let is_well_formed {a; b; c} =
      let open Or_error.Let_syntax in
      let err x =
        sprintf "proof was not well-formed (%s was off its curve)" x
      in
      let%bind () = check (G1.is_well_formed a) (err "a") in
      let%bind () = check (G2.is_well_formed b) (err "b") in
      check (G1.is_well_formed c) (err "c")
  end

  let one_pc = lazy (Pairing.G2_precomputation.create G2.one)

  let verify (vk : Verification_key.Processed.t) input
      ({Proof.a; b; c} as proof) =
    let open Or_error.Let_syntax in
    let%bind () =
      check
        (Int.equal (List.length input) (Array.length vk.query - 1))
        "Input length was not as expected"
    in
    let%bind () = Proof.is_well_formed proof in
    let input_acc =
      List.foldi input ~init:vk.query.(0) ~f:(fun i acc x ->
          let q = vk.query.(1 + i) in
          G1.(acc + (x * q)) )
    in
    let test1 =
      let l = Pairing.unreduced_pairing a b in
      let r1 = vk.alpha_beta in
      let r2 =
        Pairing.miller_loop
          (Pairing.G1_precomputation.create input_acc)
          (Lazy.force one_pc)
      in
      let r3 =
        Pairing.miller_loop (Pairing.G1_precomputation.create c) vk.delta
      in
      let test =
        let open Fq_target in
        Pairing.final_exponentiation (unitary_inverse l * r2 * r3) * r1
      in
      Fq_target.(equal test one)
    in
    check test1 "Pairing check failed"
end
