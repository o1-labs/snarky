(* Defines the interfaces for sponge constructions. *)
module Intf = Intf

(** Parameters for the sponge construction. *)
module Params : sig
  type 'a t = 'a Params.t [@@deriving bin_io]

  (** Maps over the parameters. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** Legacy parameters for Pasta Fp. *)
  val pasta_p_legacy : string t

  (** Legacy parameters for Pasta Fq. *)
  val pasta_q_legacy : string t

  (** Kimchi parameters for Pasta Fp. *)
  val pasta_p_kimchi : string t

  (** Kimchi parameters for Pasta Fq. *)
  val pasta_q_kimchi : string t
end

(** Represents the internal state of the sponge. *)
module State : sig
  type 'a t = 'a array

  (** Maps over the state. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

(** Instantiation of the Rescue permutation.
    [Inputs] defines the field and other parameters for the Rescue permutation.
*)
module Rescue (Inputs : Intf.Inputs.Rescue) :
  Intf.Permutation with module Field = Inputs.Field

(** Instantiation of the Poseidon permutation.
    [Inputs] defines the field and other parameters for the Poseidon permutation.
*)
module Poseidon (Inputs : Intf.Inputs.Poseidon) :
  Intf.Permutation with module Field = Inputs.Field

(** Functor to create basic operations for sponge constructions. *)
module Make_operations (Field : Intf.Field) :
  Intf.Operations with module Field := Field

(** Functor to create a hash function from a permutation. *)
module Make_hash (P : Intf.Permutation) :
  Intf.Hash with module State := State and module Field := P.Field

(** Represents the state of the sponge: either absorbing inputs or ready for squeezing outputs.
    - [Absorbed n] indicates that [n] elements have been absorbed since the last permutation.
    - [Squeezed n] indicates that [n] elements have been squeezed since the last permutation.
*)
type sponge_state = Absorbed of int | Squeezed of int [@@deriving sexp]

(** The main sponge type.
    - ['f] is the type of the field elements.
    - [state] is the internal state of the sponge.
    - [params] are the parameters for the sponge construction.
    - [sponge_state] tracks whether the sponge is absorbing or squeezing.
    - [id] is a unique identifier for debugging purposes.
*)
type 'f t =
  { mutable state : 'f State.t
  ; params : 'f Params.t
  ; mutable sponge_state : sponge_state
  ; id : int
  }

(** Creates a new sponge instance. *)
val make :
  state:'f State.t -> params:'f Params.t -> sponge_state:sponge_state -> 'f t

(** Functor to create a sponge from a permutation. *)
module Make_sponge (P : Intf.Permutation) : sig
  include
    Intf.Sponge
      with module State := State
       and module Field := P.Field
       and type digest := P.Field.t
       and type input := P.Field.t
       and type t = P.Field.t t

  (** Creates a new sponge instance using the permutation [P]. *)
  val make :
       state:P.Field.t State.t
    -> params:P.Field.t Params.t
    -> sponge_state:sponge_state
    -> t
end

(** Functor to create a sponge with debugging capabilities.
    This sponge will print debug information if the corresponding environment variable is set.
*)
module Make_debug_sponge (P : sig
  include Intf.Permutation

  module Circuit : Snarky_backendless.Snark_intf.Run

  val sponge_name : string

  val debug_helper_fn : Field.t -> string
end) : sig
  include
    Intf.Sponge
      with module State := State
       and module Field := P.Field
       and type digest := P.Field.t
       and type input := P.Field.t
       and type t = P.Field.t t

  (** Creates a new debug sponge instance. *)
  val make :
       state:P.Field.t State.t
    -> params:P.Field.t Params.t
    -> sponge_state:sponge_state
    -> t
end

(** A sponge that operates on bits. *)
module Bit_sponge : sig
  (** The type for a bit sponge.
      - ['s] is the type of the underlying sponge.
      - ['bool] is the type of the booleans representing bits.
  *)
  type ('s, 'bool) t

  (** Maps the underlying sponge of a bit sponge. *)
  val map : ('a, 'x) t -> f:('a -> 'b) -> ('b, 'x) t

  (** Creates a new bit sponge.
      - [last_squeezed] is an optional list of bits that were squeezed in a previous operation but not consumed.
      - [underlying] is the base sponge that operates on field elements.
  *)
  val make : ?last_squeezed:'bool list -> 's -> ('s, 'bool) t

  (** Returns the underlying sponge. *)
  val underlying : ('s, _) t -> 's

  (** Functor to create a bit sponge from a field-based sponge.
      - [Bool] defines the boolean type.
      - [Field] defines the field type and conversion to bits.
      - [Input] defines the input type for the underlying sponge.
      - [S] is the underlying sponge implementation.
  *)
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
         (* The digest is a list of booleans of a specified length. *)
         and type digest := length:int -> Bool.t list
         and type input := Input.t
         and type t = (S.t, Bool.t) t

    (** Squeezes a field element from the sponge, bypassing the bit conversion. *)
    val squeeze_field : t -> Field.t
  end
end
