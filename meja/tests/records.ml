module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type ('a, 'b, 'c) t = {a: 'a; b: 'b; c: 'c}

let x = {a= 15; b= 20; c= 25}

let y = {a= true; b= false; c= true}

let z = {a= x.a; b= y.b; c= ()}

module X = struct
  type 'a t = {a: 'a; b: 'a; c: 'a}

  let x = {a= 1; b= 1; c= 1}
end

let a = {X.x with b= 12}

let b = {X.a= 1; b= 1; c= 1}

let c = {x with a= 35}

let d = (a.b, b.b)

let e = a.X.a

let f = {a= true; b= (); c= 15}.a

let g = {X.a= (); b= (); c= ()}.a

let h = {X.a= (); b= (); c= ()}.X.a
