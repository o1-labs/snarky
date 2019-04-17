open Ctypes

val with_prefix : string -> string -> string

module type Prefix_intf = sig
  val prefix : string
end

module type S = sig
  type t

  val typ : t typ

  val func_name : string -> string

  val delete : t -> unit
end

module Make_foreign (M : Prefix_intf) : S

module type Field_prefix_intf = sig
  include Prefix_intf

  type field
end

module type Field_constrained = sig
  type 'field t

  module Make (M : Field_prefix_intf) : S with type t = M.field t
end

module Var : Field_constrained

module Linear_combination : sig
  include Field_constrained

  module Term : Field_constrained
end

module R1CS_constraint : Field_constrained

module R1CS_constraint_system : Field_constrained

module Proving_key : sig
  module Make (M : Prefix_intf) : S
end

module Verification_key : sig
  module Make (M : Prefix_intf) : S
end

module Keypair : sig
  module Make (M : Prefix_intf) : S
end

module Proof : sig
  module Make (M : Prefix_intf) : S
end
