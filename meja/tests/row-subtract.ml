module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let subtract_fixed (x : [ `A | `B | `C ]) = match x with `C -> `C

let subtract_fixed_test () = subtract_fixed `C

let subtract_closed (x : [< `A | `B | `C > `A `B ]) =
  match x with `B -> `B | `C -> `C

let subtract_closed_test () = (subtract_closed `B, subtract_closed `C)

let subtract_open (x : [> `A ]) = match x with x -> x

let subtract_open_test : [> ] -> [> ] = subtract_open

let subtract_open_test2 : [> ] -> [> ] = subtract_open

let subtract_open_test3 () = (subtract_open `B, subtract_open `C)

let refine (x : 'a) (f_A : 'a -> 'a) (f_B : 'b -> 'b) (f_C : 'c -> 'c) =
  f_C (f_B (f_A x))

let refine_test : 'a -> _ -> _ -> _ -> 'a = refine

let refine_test2 (x : [> `A | `B | `C ]) f g h = refine x f g h

let refine_test3 (x : [ `B | `C ]) f g h = refine x f g h
