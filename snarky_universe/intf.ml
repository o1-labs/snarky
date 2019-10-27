module type Cond_intf = sig
  type bool

  type t

  type cond

  val ( -? ) : bool -> t -> cond

  val ( -: ) : cond -> t -> t
end

module Cond : sig
  type _ cond

  val ( -? ) : bool -> 'a -> 'a cond

  val ( -: ) : 'a cond -> 'a -> 'a
end = struct
  type 'a cond = bool * 'a

  let ( -? ) x y = (x, y)

  let ( -: ) (b, x) y = if b then x else y
end

module Field = struct
  module type Basic = sig
    type bool

    type t

    val ( = ) : t -> t -> bool

    val equal : t -> t -> bool

    val ( * ) : t -> t -> t

    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( / ) : t -> t -> t

    val mul : t -> t -> t

    val add : t -> t -> t

    val sub : t -> t -> t

    val div : t -> t -> t

    val negate : t -> t

    val sqrt : t -> t

    val square : t -> t

    val invert : t -> t

    val one : t

    val zero : t

    val ofString : string -> t

    val ofInt : int -> t

    val ofBits : bool array -> t
  end

  module type Checked = sig
    include Basic

    include Cond_intf with type bool := bool and type t := t

    val toBits : ?length:int -> t -> bool array

    val assertEqual : t -> t -> unit

    val assertR1 : t -> t -> t -> unit
  end

  module type Constant = sig
    type t [@@deriving yojson]

    include Basic with type bool := bool and type t := t

    val toString : t -> string

    val toBits : t -> bool array
  end
end

module type S = sig
  module Impl : Snarky.Snark_intf.Run with type prover_state = unit

  module Bool : sig
    open Impl

    type t = Boolean.var

    module Constant : sig
      type t = bool [@@deriving yojson]
    end

    val true_ : t

    val false_ : t

    val typ : (t, bool) Typ.t

    val ( || ) : t -> t -> t

    val ( && ) : t -> t -> t

    val not : t -> t

    val ( = ) : t -> t -> t

    val equal : t -> t -> t

    val all : t list -> t

    val any : t list -> t

    val exactlyOne : t list -> t

    val assertAll : t list -> unit

    val assertAny : t list -> unit

    val assertExactlyOne : t list -> unit

    val assertEqual : t -> t -> unit
  end

  module Field : sig
    include Field.Checked with type bool := Bool.t

    module Constant : Field.Constant

    val typ : (t, Constant.t) Impl.Typ.t
  end

  module Hash : sig
    type t = Field.t

    val equal : t -> t -> Bool.t

    val assertEqual : t -> t -> unit

    val hash : Field.t array -> t

    include Cond_intf with type bool := Bool.t and type t := t

    module Constant : sig
      type t = Field.Constant.t [@@deriving yojson]
    end

    val typ : (t, Constant.t) Impl.Typ.t
  end

  module MerkleTree : sig
    open Impl

    type 'a t

    module Index : sig
      type t = Boolean.var array

      val typ : depth:int -> (t, int) Typ.t
    end

    module Path : sig
      type t = Hash.t array

      val typ : depth:int -> (t, Hash.Constant.t array) Typ.t
    end

    module MembershipProof : sig
      type ('index, 'hash) t_ = {index: 'index; path: 'hash array}

      type t = (Index.t, Hash.t) t_

      module Constant : sig
        type t = (int, Hash.Constant.t) t_ [@@deriving yojson]
      end

      val typ : depth:int -> (t, Constant.t) Typ.t

      val check :
        t -> Hash.t (* root hash *) -> Hash.t (* element hash *) -> unit
    end

    val ofRoot : ('a -> Hash.t) -> Hash.t -> 'a t
  end

  module InputSpec : Input_spec.S with module Impl := Impl
end
