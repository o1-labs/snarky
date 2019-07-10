open Ctypes

module Bindings =
  Vector.Bind
    (Ctypes_foreign)
    (struct
      let prefix = "camlsnark_long_vector"

      type t = Signed.Long.t

      let typ = long
    end)

include Vector.Make (struct
            type t = Signed.Long.t

            let schedule_delete _ = ()
          end)
          (Bindings)
