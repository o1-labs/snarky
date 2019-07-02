open Ctypes

module Bindings =
  Vector.Bind
    (Ctypes_foreign)
    (struct
      let prefix = "camlsnark_int_vector"

      type t = int

      let typ = int

      let schedule_delete _ = ()
    end)

include Vector.Make (Bindings)
