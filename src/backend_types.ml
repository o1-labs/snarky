open Core
open Ctypes
open Foreign

let with_prefix prefix s = sprintf "%s_%s" prefix s

module type Prefix_intf = sig
  val prefix : string
end

module type S = sig
  type t

  val typ : t typ

  val func_name : string -> string

  val delete : t -> unit
end

module Make_foreign (M : Prefix_intf) = struct
  type t = unit ptr

  let typ = ptr void

  let func_name = with_prefix M.prefix

  let delete = foreign (func_name "delete") (typ @-> returning void)
end

module type Field_prefix_intf = sig
  include Prefix_intf

  type field
end

module type Field_constrained = sig
  type 'field t

  module Make (M : Field_prefix_intf) : S with type t = M.field t
end

module Field_constrained = struct
  type 'field t = unit ptr

  module Make (M : Field_prefix_intf) : S with type t = M.field t = Make_foreign (M)
end

module Var = Field_constrained

module Linear_combination = struct
  include Field_constrained

  module Term = Field_constrained
end

module R1CS_constraint = Field_constrained

module R1CS_constraint_system = Field_constrained

module Proving_key = Field_constrained

module Verification_key = Field_constrained

module Keypair = Field_constrained

module Proof = Field_constrained
