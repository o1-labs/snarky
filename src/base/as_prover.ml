module type Extended = sig
  type field

  include As_prover_intf.Basic with type 'f field := field

  type 'a t = ('a, field) As_prover0.t
end

module Make_extended (Env : sig
  type field
end)
(As_prover : As_prover_intf.Basic with type 'f field := Env.field) =
struct
  include Env
  include As_prover

  type 'a t = ('a, field) As_prover0.t
end
