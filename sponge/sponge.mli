module Intf = Intf

module Params : sig
  type 'a t = 'a Params.t [@@deriving bin_io]

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bn128 : string t

  val mnt4_298 : string t

  val mnt4_753 : string t

  val bn382_p : string t

  val bn382_q : string t
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

module Make_hash (P : Intf.Permutation) :
  Intf.Hash with module State := State and module Field := P.Field

module Make_sponge (P : Intf.Permutation) :
  Intf.Sponge
  with module State := State
   and module Field := P.Field
   and type digest := P.Field.t
   and type input := P.Field.t

module Make_bit_sponge (Bool : sig
  type t
end) (Field : sig
  type t

  val to_bits : t -> Bool.t list
end)
(S : Intf.Sponge
     with module State := State
      and module Field := Field
      and type digest := Field.t
      and type input := Field.t) :
  Intf.Sponge
  with module State := State
   and module Field := Field
   and type digest := length:int -> Bool.t list
   and type input := Field.t
