module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type nonrec var = field_var

  and t = field

  let typ = Typ.field
end

let (x : field_var -> var) = fun x -> x

include struct
  let (y : field -> t) = fun x -> x
end
