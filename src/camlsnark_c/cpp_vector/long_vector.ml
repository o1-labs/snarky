module Bindings = Vector.Bound.Long

include Vector.Make (struct
            type t = Signed.Long.t

            let schedule_delete _ = ()
          end)
          (Bindings)
