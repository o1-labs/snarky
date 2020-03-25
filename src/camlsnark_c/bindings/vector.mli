type 'a t = unit Ctypes.ptr

val null : 'a t

val typ : 'a t Ctypes.typ

module type Bound = sig
  include Bindings_base.Foreign_types

  type elt

  type nonrec t = elt t

  val typ : t Ctypes.typ

  val delete : (t -> unit return) result

  val create : (unit -> t return) result

  val get : (t -> int -> elt return) result

  val length : (t -> int return) result

  val emplace_back : (t -> elt -> unit return) result
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
