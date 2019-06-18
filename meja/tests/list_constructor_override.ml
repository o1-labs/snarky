open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

type nil

type 'a t = [] : nil t | ( :: ) : 'hd * 'tl t -> ('hd -> 'tl) t

let x = []

let y = [Field.constant (Field.Constant.of_string "12")]

let z = [Field.constant (Field.Constant.of_string "1"); true; ()]

let z = [Field.constant (Field.Constant.of_string "1"); true; ()]

module A = struct
  type ('a, 'b) u = [] of 'a * 'b

  let x = []
end
