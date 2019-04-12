module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x = [1; 2; 3]

let y = [(1, 2, 3); (4, 5, 6); (7, 8, 9)]

let f (l : bool list) : bool * bool * bool =
  match l with
  | [] -> (false, false, false)
  | [a] -> (a, false, false)
  | [a; b] -> (a, b, false)
  | a :: b :: c :: _ -> (a, b, c)

let g = f [true; false; true]

let ( ! ) (x : bool) = match x with true -> false | false -> true

let ( + ) x y = match (x, y) with false, false -> false | _ -> true

let h = [!true; !false; true + !false]

let i = [true + false; !false]
