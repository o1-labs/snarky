module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x (y : int) (z : bool) = match y with i -> z

let y (f : int -> int -> int -> _) = match (1, 2, 3) with i, j, k -> f i j k

type nonrec t = {a: int; b: bool}

type nonrec u = {f: int -> int}

include struct
  type nonrec ('a, 'b) v_var = {x: 'a; y: 'b; g: 'a -> 'b}

  and ('a, 'b) v = {x: 'a; y: 'b; g: 'a -> 'b}

  let v_typ x___2 x___1 =
    { Snarky.Types.Typ.store=
        (fun {x; y; g} ->
          Snarky.Typ_monads.Store.bind (x___2.Snarky.Types.Typ.store x)
            ~f:(fun x ->
              Snarky.Typ_monads.Store.bind (x___1.Snarky.Types.Typ.store y)
                ~f:(fun y ->
                  Snarky.Typ_monads.Store.bind
                    ((Typ.fn x___2 x___1).Snarky.Types.Typ.store g)
                    ~f:(fun g -> Snarky.Typ_monads.Store.return {x; y; g}) ) )
          )
    ; Snarky.Types.Typ.read=
        (fun {x; y; g} ->
          Snarky.Typ_monads.Read.bind (x___2.Snarky.Types.Typ.read x)
            ~f:(fun x ->
              Snarky.Typ_monads.Read.bind (x___1.Snarky.Types.Typ.read y)
                ~f:(fun y ->
                  Snarky.Typ_monads.Read.bind
                    ((Typ.fn x___2 x___1).Snarky.Types.Typ.read g) ~f:(fun g ->
                      Snarky.Typ_monads.Read.return {x; y; g} ) ) ) )
    ; Snarky.Types.Typ.alloc=
        Snarky.Typ_monads.Alloc.bind x___2.Snarky.Types.Typ.alloc ~f:(fun x ->
            Snarky.Typ_monads.Alloc.bind x___1.Snarky.Types.Typ.alloc
              ~f:(fun y ->
                Snarky.Typ_monads.Alloc.bind
                  (Typ.fn x___2 x___1).Snarky.Types.Typ.alloc ~f:(fun g ->
                    Snarky.Typ_monads.Alloc.return {x; y; g} ) ) )
    ; Snarky.Types.Typ.check=
        (fun {x; y; g} ->
          Snarky.Checked.bind (x___2.Snarky.Types.Typ.check x) ~f:(fun () ->
              Snarky.Checked.bind (x___1.Snarky.Types.Typ.check y)
                ~f:(fun () ->
                  Snarky.Checked.bind
                    ((Typ.fn x___2 x___1).Snarky.Types.Typ.check g)
                    ~f:(fun () -> Snarky.Checked.return ()) ) ) ) }
end

let z x {f; _} = match x with {a= x; b; _} -> f x

let a (x : t) {f; _} = match x with {a= x; b; _} -> f x

let b = match {f= (fun x -> 12)} with {f; _} -> f

let (c : _ -> (int, bool) v_var) =
 fun (x : ('a, 'b) v_var) ->
  match x with {x; y; g= f; _} -> {x= 12; y; g= (fun _ -> f x)}

type 'a w = A | B : int * int -> int w | C : 'b w -> 'b w

let d x =
  match x with A -> 1 | B (x, y) -> y | C A -> 2 | C (B (x, y)) -> y | _ -> 3

let if_ (x : bool) (y : 'a) (z : 'a) = match x with true -> y | false -> z
