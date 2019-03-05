open Ctypes

include Vector.Make (struct
  let prefix = "camlsnark_long_vector"

  type t = Signed.Long.t

  let typ = long

  let schedule_delete _ = ()
end)
