module Bindings = Vector.Bound.Bool

include Vector.Make (struct
            type t = bool

            let schedule_delete _ = ()
          end)
          (Bindings)
