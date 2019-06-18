open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

module X = struct
  let x = Field.constant (Field.Constant.of_string "15")
end

let z =
  let open X in
  ignore (x = x) ;
  x
