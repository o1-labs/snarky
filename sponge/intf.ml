module type Field = sig
  type t

  val zero : t

  val ( * ) : t -> t -> t

  val ( + ) : t -> t -> t
end

module type Operations = sig
  module Field : sig
    type t
  end

  val add_block : state:Field.t array -> Field.t array -> unit

  val apply_matrix : Field.t array array -> Field.t array -> Field.t array

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

    val rounds_partial : int
  end
end

module type Permutation = sig
  module Field : sig
    type t

    val zero : t
  end

  val add_block : state:Field.t array -> Field.t array -> unit

  val copy : Field.t array -> Field.t array

  val block_cipher : Field.t Params.t -> Field.t array -> Field.t array
end

module type Hash = sig
  module Field : sig
    type t
  end

  module State : sig
    type _ t
  end

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
