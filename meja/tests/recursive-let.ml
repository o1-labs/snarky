module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x = 15

and y = 25

and z = true

let rec f () = 15

and g () = f () + 20

and h i = if i > 0 then g () + h (i - 1) else g ()

let rec k b = if b then k false else b

let int_find (x : int) = x

let rec f __implicit5__ () = int_find __implicit5__

and g __implicit6__ () = f __implicit6__ () + f __implicit6__ ()

let int_instance = 15

let h = g int_instance ()

let rec while_ pred body = if pred () then ( body () ; while_ pred body )
