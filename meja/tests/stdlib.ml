open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let test_plus = 1 + 2

let test_times = 3 * 4

let test_print_int = print_int 15

let fold_list (l : bool list) = List.fold_left (fun x y -> x && y) true l
