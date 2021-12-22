module Intf = Intf

module Params : sig
  type 'a t = 'a Params.t [@@deriving bin_io]

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bn128 : string t

  val bn382_p : string t

  val bn382_q : string t

  val mnt4_298 : string t

  val mnt4_753 : string t

  val tweedle_p : string t

  val tweedle_q : string t

  val pasta_p : string t

  val pasta_q : string t

  val pasta_p_3 : string t

  val pasta_q_3 : string t
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

module Bn382_inputs (Field : Intf.Field_mutable) :
  Intf.Inputs.Poseidon with module Field := Field

module Make_hash (P : Intf.Permutation) :
  Intf.Hash with module State := State and module Field := P.Field

type sponge_state = Absorbed of int | Squeezed of int [@@deriving sexp]

type 'f t =
  { mutable state: 'f State.t
  ; params: 'f Params.t
  ; mutable sponge_state: sponge_state }

val make :
  state:'f State.t -> params:'f Params.t -> sponge_state:sponge_state -> 'f t

module Make_sponge (P : Intf.Permutation) : sig
  include
    Intf.Sponge
    with module State := State
     and module Field := P.Field
     and type digest := P.Field.t
     and type input := P.Field.t
     and type t = P.Field.t t

  val make :
       state:P.Field.t State.t
    -> params:P.Field.t Params.t
    -> sponge_state:sponge_state
    -> t
end

module Bit_sponge : sig
  type ('s, 'bool) t

  val map : ('a, 'x) t -> f:('a -> 'b) -> ('b, 'x) t

  val make : ?last_squeezed:'bool list -> 's -> ('s, 'bool) t

  val underlying : ('s, _) t -> 's

  module Make (Bool : sig
    type t
  end) (Field : sig
    type t

    val to_bits : t -> Bool.t list

    val finalize_discarded : Bool.t list -> unit

    val high_entropy_bits : int
  end)
  (Input : Intf.T)
  (S : Intf.Sponge
       with module State := State
        and module Field := Field
        and type digest := Field.t
        and type input := Input.t) : sig
    include
      Intf.Sponge
      with module State := State
       and module Field := Field
       and type digest := length:int -> Bool.t list
       and type input := Input.t
       and type t = (S.t, Bool.t) t

    val squeeze_field : t -> Field.t
  end
end
