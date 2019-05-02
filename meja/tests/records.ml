module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type ('a, 'b, 'c) t = {a: 'a; b: 'b; c: 'c}

  let typ : (_, (_, _, _) t) Typ.t =
    { Typ.store=
        (fun {a; b; c; _} ->
          Typ.Store.bind (Typ.store c) (fun c ->
              Typ.Store.bind (Typ.store b) (fun b ->
                  Typ.Store.bind (Typ.store a) (fun a ->
                      Typ.Store.return {a; b; c} ) ) ) )
    ; Typ.read=
        (fun {a; b; c; _} ->
          Typ.Read.bind (Snarky.read c) (fun c ->
              Typ.Read.bind (Snarky.read b) (fun b ->
                  Typ.Read.bind (Snarky.read a) (fun a ->
                      Typ.Read.return {a; b; c} ) ) ) )
    ; Typ.alloc=
        (fun {a; b; c; _} ->
          Typ.Alloc.bind Typ.alloc (fun c ->
              Typ.Alloc.bind Typ.alloc (fun b ->
                  Typ.Alloc.bind Typ.alloc (fun a -> Typ.Alloc.return {a; b; c})
              ) ) )
    ; Typ.check=
        (fun {a; b; c; _} ->
          (fun f x -> f x) (Typ.check c) (fun c ->
              (fun f x -> f x) (Typ.check b) (fun b ->
                  (fun f x -> f x) (Typ.check a) (fun a -> ()) ) ) ) }
end

let x = {a= 15; b= 20; c= 25}

let y = {a= true; b= false; c= true}

let z = {a= x.a; b= y.b; c= ()}

module X = struct
  include struct
    type 'a t = {a: 'a; b: 'a; c: 'a}

    let typ : (_, _ t) Typ.t =
      { Typ.store=
          (fun {a; b; c; _} ->
            Typ.Store.bind (Typ.store c) (fun c ->
                Typ.Store.bind (Typ.store b) (fun b ->
                    Typ.Store.bind (Typ.store a) (fun a ->
                        Typ.Store.return {a; b; c} ) ) ) )
      ; Typ.read=
          (fun {a; b; c; _} ->
            Typ.Read.bind (Snarky.read c) (fun c ->
                Typ.Read.bind (Snarky.read b) (fun b ->
                    Typ.Read.bind (Snarky.read a) (fun a ->
                        Typ.Read.return {a; b; c} ) ) ) )
      ; Typ.alloc=
          (fun {a; b; c; _} ->
            Typ.Alloc.bind Typ.alloc (fun c ->
                Typ.Alloc.bind Typ.alloc (fun b ->
                    Typ.Alloc.bind Typ.alloc (fun a ->
                        Typ.Alloc.return {a; b; c} ) ) ) )
      ; Typ.check=
          (fun {a; b; c; _} ->
            (fun f x -> f x) (Typ.check c) (fun c ->
                (fun f x -> f x) (Typ.check b) (fun b ->
                    (fun f x -> f x) (Typ.check a) (fun a -> ()) ) ) ) }
  end

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
