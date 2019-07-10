open Core
open Ctypes

let with_prefix prefix s = sprintf "%s_%s" prefix s

module type Prefix_intf = sig
  val prefix : string
end

module type Foreign_types = sig
  type 'a return

  type 'a result
end

module type S = sig
  include Foreign_types

  type t

  val typ : t typ

  val func_name : string -> string

  val delete : (t -> unit return) result
end

module Make_foreign (F : Ctypes.FOREIGN) (M : Prefix_intf) = struct
  include F

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

  module Make (F : Ctypes.FOREIGN) (M : Field_prefix_intf) :
    S
    with type t = M.field t
     and type 'a return := 'a F.return
     and type 'a result := 'a F.result
end

module Field_constrained = struct
  type 'field t = unit ptr

  module Make (F : Ctypes.FOREIGN) (M : Field_prefix_intf) :
    S
    with type t = M.field t
     and type 'a return := 'a F.return
     and type 'a result := 'a F.result =
    Make_foreign (F) (M)
end

module Var = Field_constrained

module Linear_combination = struct
  include Field_constrained
  module Term = Field_constrained
end

module R1CS_constraint = Field_constrained
module R1CS_constraint_system = Field_constrained

module Proving_key = struct
  module Make = Make_foreign
end

module Verification_key = Proving_key
module Keypair = Proving_key
module Proof = Proving_key
