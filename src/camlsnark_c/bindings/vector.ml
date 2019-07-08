open Ctypes
open Bindings_base

type 'a t = unit ptr

let null = null

let typ = ptr void

module type Bound = sig
  include Foreign_types

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
   and type elt = Elt.t = struct
  include F

  type elt = Elt.t

  type nonrec t = elt t

  let typ = ptr void

  let func_name = with_prefix Elt.prefix

  let delete = foreign (func_name "delete") (typ @-> returning void)

  let create = foreign (func_name "create") (void @-> returning typ)

  let get = foreign (func_name "get") (typ @-> int @-> returning Elt.typ)

  let length = foreign (func_name "length") (typ @-> returning int)

  let emplace_back =
    foreign (func_name "emplace_back") (typ @-> Elt.typ @-> returning void)
end

module Bindings (F : Ctypes.FOREIGN) = struct
  module Bool =
    Bind
      (F)
      (struct
        let prefix = "camlsnark_bool_vector"

        type t = bool

        let typ = bool
      end)

  module Int =
    Bind
      (F)
      (struct
        let prefix = "camlsnark_int_vector"

        type t = int

        let typ = int
      end)

  module Long =
    Bind
      (F)
      (struct
        let prefix = "camlsnark_long_vector"

        type t = Signed.Long.t

        let typ = long
      end)
end
