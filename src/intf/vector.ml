open Core_kernel

module type S = sig
  type elt

  type t

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
