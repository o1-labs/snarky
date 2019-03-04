open Ctypes

include Vector.Make (struct
  let prefix = "camlsnark_int_vector"

  type t = int

  let typ = int

  let schedule_delete _ = ()
end)
