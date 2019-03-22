open Core

type 'a t

module type S = sig
  type elt

  type nonrec t = elt t

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

module Make (Elt : sig
  type t

  val typ : t Ctypes.typ

  val schedule_delete : t -> unit

  val prefix : string
end) : S with type elt = Elt.t

module Make_binable (Elt : sig
  type t [@@deriving bin_io]

  val typ : t Ctypes.typ

  val schedule_delete : t -> unit

  val prefix : string
end) : S_binable with type elt = Elt.t
