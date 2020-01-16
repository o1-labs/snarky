module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let (x : unit -> int) = fun () -> try failwith "oh dear" with Failure _ -> 15

let (y : bool -> string) =
 fun b -> try if b then "" else failwith "" with Failure _ -> ""

exception Other of int * bool

let (z : unit -> int) =
 fun () -> try 15 with Failure _ -> 12 | Other (i, _) -> i

let a () = try raise (Other (15, true)) with Other (i, b) -> (i, b)
