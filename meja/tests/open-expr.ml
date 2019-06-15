module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module X = struct
  let x = Field.constant (Field.Constant.of_string "15")
end

let z =
  let open X in
  ignore (x = x) ;
  x
