module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x (y : int) (z : bool) = match y with i -> z

let y (f : int -> int -> int -> _) = match (1, 2, 3) with i, j, k -> f i j k

type t = {a: int; b: bool}

type u = {f: int -> int}

include struct
  type ('a, 'b) v = {x: 'a; y: 'b; g: 'a -> 'b}

  let v_typ : (_, (_, _) v) Typ.t =
    { Typ.store=
        (fun {x; y; g; _} ->
          Typ.Store.bind (Typ.store g) (fun g ->
              Typ.Store.bind (Typ.store y) (fun y ->
                  Typ.Store.bind (Typ.store x) (fun x ->
                      Typ.Store.return {x; y; g} ) ) ) )
    ; Typ.read=
        (fun {x; y; g; _} ->
          Typ.Read.bind (Snarky.read g) (fun g ->
              Typ.Read.bind (Snarky.read y) (fun y ->
                  Typ.Read.bind (Snarky.read x) (fun x ->
                      Typ.Read.return {x; y; g} ) ) ) )
    ; Typ.alloc=
        (fun {x; y; g; _} ->
          Typ.Alloc.bind Typ.alloc (fun g ->
              Typ.Alloc.bind Typ.alloc (fun y ->
                  Typ.Alloc.bind Typ.alloc (fun x -> Typ.Alloc.return {x; y; g})
              ) ) )
    ; Typ.check=
        (fun {x; y; g; _} ->
          (fun f x -> f x) (Typ.check g) (fun g ->
              (fun f x -> f x) (Typ.check y) (fun y ->
                  (fun f x -> f x) (Typ.check x) (fun x -> ()) ) ) ) }
end

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
