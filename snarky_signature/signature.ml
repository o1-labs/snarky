module type Inputs_intf = sig
  module Bool : sig
    type t

    val ( && ) : t -> t -> t
  end

  module Field : sig
    type t

    val equal : t -> t -> Bool.t

    val size_in_bits : int

    val is_even : t -> Bool.t
  end

  module Scalar : sig
    type t

    val of_field : Field.t -> t

    val to_field : t -> Field.t
  end

  module Group : sig
    type t

    val add_exn : t -> t -> t

    val to_affine_exn : t -> Field.t * Field.t

    val scale : ?init:t -> t -> Scalar.t -> t

    val one : t

    val negate : t -> t
  end

  module Hash : sig
    type t = Field.t

    val hash : Field.t array -> t
  end
end

module type Signer_inputs_intf = sig
  module Bool : sig
    type t

    val ( && ) : t -> t -> t
  end

  module Field : sig
    type t

    val equal : t -> t -> Bool.t

    val size_in_bits : int

    val is_even : t -> Bool.t
  end

  module Scalar : sig
    type t

    val of_field : Field.t -> t

    val to_field : t -> Field.t

    val negate : t -> t

    val ( + ) : t -> t -> t

    val ( * ) : t -> t -> t
  end

  module Group : sig
    type t

    val add_exn : t -> t -> t

    val to_affine_exn : t -> Field.t * Field.t

    val scale : ?init:t -> t -> Scalar.t -> t

    val one : t

    val negate : t -> t
  end

  module Hash : sig
    type t = Field.t

    val hash : Field.t array -> t
  end
end

module Make0 (Inputs : Inputs_intf) = struct
  open Inputs

  type message = Field.t array

  module Signature = struct
    type t = Field.t * Scalar.t
  end

  module Private_key = struct
    type t = Scalar.t
  end

  module Public_key = struct
    type t = Group.t

    let of_private_key s = Group.scale Group.one s
  end

  (* TODO: We can just take the first 256 bits of the challenge *)
  let challenge ~public_key ~r message =
    let x, y = Group.to_affine_exn public_key in
    Scalar.of_field (Hash.hash (Array.append [|x; y; r|] message))

  let check ((r, s) : Signature.t) (public_key : Public_key.t) (m : message) =
    let e = challenge m ~public_key ~r in
    (* s * g - e * public_key *)
    let e_pk = Group.scale (Group.negate public_key) e in
    let s_g_e_pk = Group.scale ~init:e_pk Group.one s in
    let rx, ry = Group.to_affine_exn s_g_e_pk in
    let y_even = Field.is_even ry in
    let r_correct = Field.equal r rx in
    Bool.(r_correct && y_even)
end

module Make_signer (Inputs : Signer_inputs_intf with type Bool.t = bool) =
struct
  open Inputs
  include Make0 (Inputs)

  let derive message ~public_key ~private_key =
    let x, y = Group.to_affine_exn public_key in
    Hash.hash (Array.append [|x; y; Scalar.to_field private_key|] message)

  let sign (d_prime : Private_key.t) m =
    let public_key =
      (* TODO: Don't recompute this. *) Group.scale Group.one d_prime
    in
    (* TODO: Once we switch to implicit sign-bit we'll have to conditionally negate d_prime. *)
    let d = d_prime in
    let k_prime = Scalar.of_field (derive m ~public_key ~private_key:d) in
    (* This assertion happens with negligible probability
    assert (not Group.Scalar.(equal k_prime zero)) ; *)
    let r, ry = Group.(to_affine_exn (scale one k_prime)) in
    let k = if Field.is_even ry then k_prime else Scalar.negate k_prime in
    let e = challenge m ~public_key ~r in
    let s = Scalar.(k + (e * d)) in
    (r, s)
end
