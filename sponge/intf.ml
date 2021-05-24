module type Field = sig
  type t

  val zero : t

  val ( * ) : t -> t -> t

  val ( + ) : t -> t -> t
end

module type Field_mutable = sig
  include Field

  val square : t -> t

  val ( *= ) : t -> t -> unit

  val ( += ) : t -> t -> unit

  module Mutable : sig
    val square : t -> unit
  end
end

module type Operations = sig
  module Field : sig
    type t
  end

  val add_assign : state:Field.t array -> int -> Field.t -> unit

  val apply_affine_map :
    Field.t array array * Field.t array -> Field.t array -> Field.t array

  val copy : Field.t array -> Field.t array
end

module Inputs = struct
  module type Common = sig
    module Field : sig
      type t

      val zero : t
    end

    val to_the_alpha : Field.t -> Field.t

    module Operations : Operations with module Field := Field
  end

  module type Rescue = sig
    include Common

    val rounds : int

    val alphath_root : Field.t -> Field.t
  end

  module type Poseidon = sig
    include Common

    val rounds_full : int

    val initial_ark : bool

    val rounds_partial : int
  end
end

module type Permutation = sig
  module Field : sig
    type t

    val zero : t
  end

  val add_assign : state:Field.t array -> int -> Field.t -> unit

  val copy : Field.t array -> Field.t array

  val block_cipher : Field.t Params.t -> Field.t array -> Field.t array
end

module type T = sig
  type t
end

module type T1 = sig
  type _ t
end

module type Hash = sig
  module Field : T

  module State : T1

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

module type Sponge = sig
  module Field : T

  module State : T1

  type input

  type digest

  type t

  val create : ?init:Field.t State.t -> Field.t Params.t -> t

  val absorb : t -> input -> unit

  val squeeze : t -> digest

  val copy : t -> t

  val state : t -> Field.t State.t
end
