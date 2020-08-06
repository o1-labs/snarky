module Bindings = Vector.Bound.Int

include Vector.Make (struct
            type t = int

            let schedule_delete _ = ()
          end)
          (Bindings)
