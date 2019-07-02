open Ctypes

module Bindings =
  Vector.Bind
    (Ctypes_foreign)
    (struct
      let prefix = "camlsnark_bool_vector"

      type t = bool

      let typ = bool

      let schedule_delete _ = ()
    end)

include Vector.Make (Bindings)
