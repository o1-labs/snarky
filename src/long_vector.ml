open Ctypes

module Bindings =
  Vector.Bind
    (Ctypes_foreign)
    (struct
      let prefix = "camlsnark_long_vector"

      type t = Signed.Long.t

      let typ = long

      let schedule_delete _ = ()
    end)

include Vector.Make (Bindings)
