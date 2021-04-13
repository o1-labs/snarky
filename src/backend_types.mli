open Ctypes

val with_prefix : string -> string -> string

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

module Make_foreign (F : Ctypes.FOREIGN) (M : Prefix_intf) :
  S with type 'a return := 'a F.return and type 'a result := 'a F.result

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

module Var :
  Field_constrained with type 'field t = 'field Camlsnark_c.Backend_types.Var.t

module Linear_combination : sig
  include
    Field_constrained
    with type 'field t = 'field Camlsnark_c.Backend_types.Linear_combination.t

  module Term :
    Field_constrained
    with type 'field t =
                'field Camlsnark_c.Backend_types.Linear_combination.Term.t
end

module R1CS_constraint :
  Field_constrained
  with type 'field t = 'field Camlsnark_c.Backend_types.R1CS_constraint.t

module R1CS_constraint_system :
  Field_constrained
  with type 'field t =
              'field Camlsnark_c.Backend_types.R1CS_constraint_system.t

module Proving_key : sig
  module Make (F : Ctypes.FOREIGN) (M : Prefix_intf) :
    S with type 'a return := 'a F.return and type 'a result := 'a F.result
end

module Verification_key : sig
  module Make (F : Ctypes.FOREIGN) (M : Prefix_intf) :
    S with type 'a return := 'a F.return and type 'a result := 'a F.result
end

module Keypair : sig
  module Make (F : Ctypes.FOREIGN) (M : Prefix_intf) :
    S with type 'a return := 'a F.return and type 'a result := 'a F.result
end

module Proof : sig
  module Make (F : Ctypes.FOREIGN) (M : Prefix_intf) :
    S with type 'a return := 'a F.return and type 'a result := 'a F.result
end
