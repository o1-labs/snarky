module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x (y : int) (z : bool) = match y with i -> z

let y (f : int -> int -> int -> _) = match (1, 2, 3) with i, j, k -> f i j k

type t = {a: int; b: bool}

type u = {f: int -> int}

type ('a, 'b) v = {x: 'a; y: 'b; g: 'a -> 'b}

let z x {f; _} = match x with {a= x; b; _} -> f x

let a (x : t) {f; _} = match x with {a= x; b; _} -> f x

let b = match {f= (fun x -> 12)} with {f; _} -> f

let (c : _ -> (int, bool) v) =
 fun (x : ('a, 'b) v) ->
  match x with {x; y; g= f; _} -> {x= 12; y; g= (fun _ -> f x)}

type 'a w = A | B : int * int -> int w | C : 'b w -> 'b w

let d x =
  match x with A -> 1 | B (x, y) -> y | C A -> 2 | C (B (x, y)) -> y | _ -> 3

let if_ (x : bool) (y : 'a) (z : 'a) = match x with true -> y | false -> z
