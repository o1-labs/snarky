module Intf = Intf

module Params : sig
  type 'a t = 'a Params.t [@@deriving bin_io]

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bn128 : string t

  val mnt4_298 : string t

  val mnt4_753 : string t
end

module State : sig
  type 'a t = 'a array

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Rescue (Inputs : Intf.Inputs.Rescue) :
  Intf.Permutation with module Field = Inputs.Field

module Poseidon (Inputs : Intf.Inputs.Poseidon) :
  Intf.Permutation with module Field = Inputs.Field

module Make_operations (Field : Intf.Field) :
  Intf.Operations with module Field := Field

module Make (P : Intf.Permutation) :
  Intf.Hash with module State := State and module Field := P.Field
