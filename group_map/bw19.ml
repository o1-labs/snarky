(** Based on the paper
    {{:https://eprint.iacr.org/2019/403}Fast and simple constant-time hashing to the BLS12-381 elliptic curve}
*)

open Core_kernel

module Spec = struct
  type 'f t = { b : 'f } [@@deriving bin_io]
end

module Params = struct
  type 'f t =
    { u : 'f
    ; fu : 'f
    ; sqrt_neg_three_u_squared_minus_u_over_2 : 'f
    ; sqrt_neg_three_u_squared : 'f
    ; inv_three_u_squared : 'f
    ; b : 'f
    }
  [@@deriving fields, bin_io]

  let spec { b; _ } = { Spec.b }

  let map
      { u
      ; fu
      ; sqrt_neg_three_u_squared_minus_u_over_2
      ; sqrt_neg_three_u_squared
      ; inv_three_u_squared
      ; b
      } ~f =
    { u = f u
    ; fu = f fu
    ; sqrt_neg_three_u_squared_minus_u_over_2 =
        f sqrt_neg_three_u_squared_minus_u_over_2
    ; sqrt_neg_three_u_squared = f sqrt_neg_three_u_squared
    ; inv_three_u_squared = f inv_three_u_squared
    ; b = f b
    }

  (** A deterministic function for constructing a valid choice of parameters for
     a given field. *)
  let create (type t) (module F : Field_intf.S_unchecked with type t = t)
      { Spec.b } =
    let open F in
    let first_map f =
      let rec go i = match f i with Some x -> x | None -> go (i + one) in
      go zero
    in
    let curve_eqn u = (u * u * u) + b in
    let u, fu =
      first_map (fun u ->
          let fu = curve_eqn u in
          if equal u zero || equal fu zero then None else Some (u, fu) )
    in
    let three_u_squared = u * u * of_int 3 in
    let sqrt_neg_three_u_squared = sqrt (negate three_u_squared) in
    { u
    ; fu
    ; sqrt_neg_three_u_squared_minus_u_over_2 =
        (sqrt_neg_three_u_squared - u) / of_int 2
    ; sqrt_neg_three_u_squared
    ; inv_three_u_squared = one / three_u_squared
    ; b
    }
end

module Make
    (Constant : Field_intf.S) (F : sig
      include Field_intf.S

      val constant : Constant.t -> t
    end) (P : sig
      val params : Constant.t Params.t
    end) =
struct
  open F
  open P

  let square x = x * x

  let potential_xs t =
    let t2 = t * t in
    let alpha =
      let alpha_inv = (t2 + constant params.fu) * t2 in
      one / alpha_inv
    in
    let x1 =
      let temp = square t2 * alpha * constant params.sqrt_neg_three_u_squared in
      constant params.sqrt_neg_three_u_squared_minus_u_over_2 - temp
    in
    let x2 = negate (constant params.u) - x1 in
    let x3 =
      let t2_plus_fu = t2 + constant params.fu in
      let t2_inv = alpha * t2_plus_fu in
      let temp =
        square t2_plus_fu * t2_inv * constant params.inv_three_u_squared
      in
      constant params.u - temp
    in
    (x1, x2, x3)

  let field_to_conic _ = failwith "Not implemented"

  let conic_to_s _ = failwith "Not implemented"

  let _s_to_v _ = failwith "Not implemented"
end

let to_group (type t) (module F : Field_intf.S_unchecked with type t = t)
    ~params t =
  let module M =
    Make
      (F)
      (struct
        include F

        let constant = Fn.id
      end)
      (struct
        let params = params
      end)
  in
  let b = params.b in
  let try_decode x =
    let f x = F.((x * x * x) + b) in
    let y = f x in
    if F.is_square y then Some (x, F.sqrt y) else None
  in
  let x1, x2, x3 = M.potential_xs t in
  List.find_map [ x1; x2; x3 ] ~f:try_decode |> Option.value_exn
