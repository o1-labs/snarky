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

module Make (P : Intf.Permutation) : sig
  open P

  val update :
       Field.t Params.t
    -> state:Field.t State.t
    -> Field.t array
    -> Field.t State.t

  val digest : Field.t State.t -> Field.t

  val initial_state : Field.t State.t

  val hash :
    ?init:Field.t State.t -> Field.t Params.t -> Field.t array -> Field.t
end
