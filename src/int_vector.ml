open Ctypes

module Bindings =
  Vector.Bind
    (Ctypes_foreign)
    (struct
      let prefix = "camlsnark_int_vector"

      type t = int

      let typ = int
    end)

include Vector.Make (struct
            type t = int

            let schedule_delete _ = ()
          end)
          (Bindings)
