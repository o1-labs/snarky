(** {{:https://link.springer.com/chapter/10.1007/11792086_36}Construction of
Rational Points on Elliptic Curves over Finite Fields by Andrew Shallue and
Christiaan E. van de Woestijne.}
*)

module Field_intf = Field_intf

module Conic : sig
  type 'f t = { z : 'f; y : 'f } [@@deriving bin_io]
end

module S : sig
  (* S = S(u, v, y) : y^2(u^2 + uv + v^2 + a) = −f(u)
     from (12)
  *)
  type 'f t = { u : 'f; v : 'f; y : 'f }
end

module V : sig
  (* V = V(x1, x2, x3, x4) : f(x1)f(x2)f(x3) = x4^2
     from (8)
  *)
  type 'f t = 'f * 'f * 'f * 'f
end

module Intf (F : sig
  type t
end) : sig
  module type S = sig
    val to_group : F.t -> F.t * F.t
  end
end

module type S = sig
  module Spec : sig
    type _ t [@@deriving bin_io]
  end

  module Params : sig
    type 'f t [@@deriving bin_io]

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val spec : 'f t -> 'f Spec.t

    val create :
      (module Field_intf.S_unchecked with type t = 'f) -> 'f Spec.t -> 'f t
  end

  module Make
      (Constant : Field_intf.S) (F : sig
        include Field_intf.S

        val constant : Constant.t -> t
      end) (Params : sig
        val params : Constant.t Params.t
      end) : sig
    val potential_xs : F.t -> F.t * F.t * F.t

    val field_to_conic : F.t -> F.t Conic.t

    val conic_to_s : F.t Conic.t -> F.t S.t

    val _s_to_v : F.t S.t -> F.t V.t
  end

  val to_group :
       (module Field_intf.S_unchecked with type t = 'f)
    -> params:'f Params.t
    -> 'f
    -> 'f * 'f
end

module Bw19 : S with module Spec = Bw19.Spec and module Params = Bw19.Params

module Spec : sig
  type 'f t = { a : 'f; b : 'f } [@@deriving fields, bin_io]
end

module Params : sig
  type 'f t =
    { u : 'f
    ; u_over_2 : 'f
    ; projection_point : 'f Conic.t
    ; conic_c : 'f
    ; spec : 'f Spec.t
    }
  [@@deriving fields, bin_io]

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val spec : 'f t -> 'f Spec.t

  val create :
    (module Field_intf.S_unchecked with type t = 'f) -> 'f Spec.t -> 'f t
end

include S with module Spec := Spec and module Params := Params
