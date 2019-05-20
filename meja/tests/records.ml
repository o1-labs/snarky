module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type ('a, 'b, 'c) t = {a: 'a; b: 'b; c: 'c}

  let typ __implicit10__ __implicit7__ __implicit1__ __implicit11__
      __implicit8__ __implicit2__ __implicit12__ __implicit9__ __implicit3__ :
      (('a2, 'b2, 'c2) t, ('a1, 'b1, 'c1) t) Typ.t =
    { Typ.store=
        (fun {a; b; c; _} ->
          Typ.Store.bind
            ((Typ.store __implicit1__) c)
            (fun c ->
              Typ.Store.bind
                ((Typ.store __implicit2__) b)
                (fun b ->
                  Typ.Store.bind
                    ((Typ.store __implicit3__) a)
                    (fun a -> Typ.Store.return {a; b; c}) ) ) )
    ; Typ.read=
        (fun {a; b; c; _} ->
          Typ.Read.bind
            ((Typ.read __implicit1__) c)
            (fun c ->
              Typ.Read.bind
                ((Typ.read __implicit2__) b)
                (fun b ->
                  Typ.Read.bind
                    ((Typ.read __implicit3__) a)
                    (fun a -> Typ.Read.return {a; b; c}) ) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit7__) (fun c ->
            Typ.Alloc.bind (Typ.alloc __implicit8__) (fun b ->
                Typ.Alloc.bind (Typ.alloc __implicit9__) (fun a ->
                    Typ.Alloc.return {a; b; c} ) ) )
    ; Typ.check=
        (fun {a; b; c; _} ->
          (fun x f -> f x)
            ((Typ.check __implicit10__) c)
            (fun c ->
              (fun x f -> f x)
                ((Typ.check __implicit11__) b)
                (fun b ->
                  (fun x f -> f x) ((Typ.check __implicit12__) a) (fun a -> ())
                  ) ) ) }
end

let x = {a= 15; b= 20; c= 25}

let y = {a= true; b= false; c= true}

let z = {a= x.a; b= y.b; c= ()}

module X = struct
  include struct
    type 'a t = {a: 'a; b: 'a; c: 'a}

    let typ __implicit24__ __implicit23__ __implicit22__ __implicit21__
        __implicit20__ __implicit19__ __implicit13__ : ('a2 t, 'a1 t) Typ.t =
      { Typ.store=
          (fun {a; b; c; _} ->
            Typ.Store.bind
              ((Typ.store __implicit13__) c)
              (fun c ->
                Typ.Store.bind
                  ((Typ.store __implicit13__) b)
                  (fun b ->
                    Typ.Store.bind
                      ((Typ.store __implicit13__) a)
                      (fun a -> Typ.Store.return {a; b; c}) ) ) )
      ; Typ.read=
          (fun {a; b; c; _} ->
            Typ.Read.bind
              ((Typ.read __implicit13__) c)
              (fun c ->
                Typ.Read.bind
                  ((Typ.read __implicit13__) b)
                  (fun b ->
                    Typ.Read.bind
                      ((Typ.read __implicit13__) a)
                      (fun a -> Typ.Read.return {a; b; c}) ) ) )
      ; Typ.alloc=
          Typ.Alloc.bind (Typ.alloc __implicit19__) (fun c ->
              Typ.Alloc.bind (Typ.alloc __implicit20__) (fun b ->
                  Typ.Alloc.bind (Typ.alloc __implicit21__) (fun a ->
                      Typ.Alloc.return {a; b; c} ) ) )
      ; Typ.check=
          (fun {a; b; c; _} ->
            (fun x f -> f x)
              ((Typ.check __implicit22__) c)
              (fun c ->
                (fun x f -> f x)
                  ((Typ.check __implicit23__) b)
                  (fun b ->
                    (fun x f -> f x)
                      ((Typ.check __implicit24__) a)
                      (fun a -> ()) ) ) ) }
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
