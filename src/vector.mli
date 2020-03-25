open Core

type 'a t = 'a Camlsnark_c.Vector.t

val null : 'a t

module type Bound = sig
  type 'a return

  type 'a result

  type elt

  type nonrec t = elt t

  val typ : t Ctypes.typ

  val delete : (t -> unit return) result

  val create : (unit -> t return) result

  val get : (t -> int -> elt return) result

  val length : (t -> int return) result

  val emplace_back : (t -> elt -> unit return) result
end

module type S = sig
  type elt

  type t

  val typ : t Ctypes.typ

  val delete : t -> unit

  val create : unit -> t

  val get : t -> int -> elt

  val emplace_back : t -> elt -> unit

  val length : t -> int
end

module type S_binable = sig
  include S

  include Binable.S with type t := t
end

module type S_binable_sexpable = sig
  include S_binable

  include Sexpable.S with type t := t
end

module Bind
    (F : Ctypes.FOREIGN) (Elt : sig
        type t

        val typ : t Ctypes.typ

        val prefix : string
    end) :
  Bound
  with type 'a return = 'a F.return
   and type 'a result = 'a F.result
   and type elt = Elt.t

module Bindings (F : Ctypes.FOREIGN) : sig
  module Bool :
    Bound
    with type 'a return = 'a F.return
     and type 'a result = 'a F.result
     and type elt = bool

  module Int :
    Bound
    with type 'a return = 'a F.return
     and type 'a result = 'a F.result
     and type elt = int

  module Long :
    Bound
    with type 'a return = 'a F.return
     and type 'a result = 'a F.result
     and type elt = Signed.Long.t
end

module Bound : sig
  module Bool :
    Bound with type 'a return = 'a and type 'a result = 'a and type elt = bool

  module Int :
    Bound with type 'a return = 'a and type 'a result = 'a and type elt = int

  module Long :
    Bound
    with type 'a return = 'a
     and type 'a result = 'a
     and type elt = Signed.Long.t
end

module Make (Elt : sig
  type t

  val schedule_delete : t -> unit
end)
(Bindings : Bound
            with type 'a return = 'a
             and type 'a result = 'a
             and type elt = Elt.t) :
  S with type t = Elt.t t and type elt = Bindings.elt

module Make_binable (Elt : sig
  type t [@@deriving bin_io]

  val schedule_delete : t -> unit
end)
(Bindings : Bound
            with type 'a return = 'a
             and type 'a result = 'a
             and type elt = Elt.t) :
  S_binable with type t = Elt.t t and type elt = Elt.t

module Make_binable_sexpable (Elt : sig
  type t [@@deriving bin_io, sexp]

  val schedule_delete : t -> unit
end)
(Bindings : Bound
            with type 'a return = 'a
             and type 'a result = 'a
             and type elt = Elt.t) :
  S_binable_sexpable with type t = Elt.t t and type elt = Elt.t
