module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let test_plus = 1 + 2

let test_times = 3 * 4

let test_print_int = print_int 15

let fold_list (l : bool list) =
  List.fold_left (fun x y -> x && y) Boolean.true_ l
