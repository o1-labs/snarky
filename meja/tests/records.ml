module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type nonrec ('a, 'b, 'c) t = {a: 'a; b: 'b; c: 'c}

  let typ __implicit1__ __implicit2__ __implicit3__ :
      (('a2, 'b2, 'c2) t, ('a1, 'b1, 'c1) t) Typ.t =
    { Typ.store=
        (fun {a; b; c; _} ->
          Typ.Store.bind (Typ.store __implicit1__ c) (fun c ->
              Typ.Store.bind (Typ.store __implicit2__ b) (fun b ->
                  Typ.Store.bind (Typ.store __implicit3__ a) (fun a ->
                      Typ.Store.return {a; b; c} ) ) ) )
    ; Typ.read=
        (fun {a; b; c; _} ->
          Typ.Read.bind (Typ.read __implicit1__ c) (fun c ->
              Typ.Read.bind (Typ.read __implicit2__ b) (fun b ->
                  Typ.Read.bind (Typ.read __implicit3__ a) (fun a ->
                      Typ.Read.return {a; b; c} ) ) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit1__) (fun c ->
            Typ.Alloc.bind (Typ.alloc __implicit2__) (fun b ->
                Typ.Alloc.bind (Typ.alloc __implicit3__) (fun a ->
                    Typ.Alloc.return {a; b; c} ) ) )
    ; Typ.check=
        (fun {a; b; c; _} ->
          Typ.check __implicit1__ c ;
          Typ.check __implicit2__ b ;
          Typ.check __implicit3__ a ;
          () ) }
end

let x = {a= 15; b= 20; c= 25}

let y = {a= true; b= false; c= true}

let z = {a= x.a; b= y.b; c= ()}

module X = struct
  include struct
    type nonrec 'a t = {a: 'a; b: 'a; c: 'a}

    let typ __implicit13__ : ('a2 t, 'a1 t) Typ.t =
      { Typ.store=
          (fun {a; b; c; _} ->
            Typ.Store.bind (Typ.store __implicit13__ c) (fun c ->
                Typ.Store.bind (Typ.store __implicit13__ b) (fun b ->
                    Typ.Store.bind (Typ.store __implicit13__ a) (fun a ->
                        Typ.Store.return {a; b; c} ) ) ) )
      ; Typ.read=
          (fun {a; b; c; _} ->
            Typ.Read.bind (Typ.read __implicit13__ c) (fun c ->
                Typ.Read.bind (Typ.read __implicit13__ b) (fun b ->
                    Typ.Read.bind (Typ.read __implicit13__ a) (fun a ->
                        Typ.Read.return {a; b; c} ) ) ) )
      ; Typ.alloc=
          Typ.Alloc.bind (Typ.alloc __implicit13__) (fun c ->
              Typ.Alloc.bind (Typ.alloc __implicit13__) (fun b ->
                  Typ.Alloc.bind (Typ.alloc __implicit13__) (fun a ->
                      Typ.Alloc.return {a; b; c} ) ) )
      ; Typ.check=
          (fun {a; b; c; _} ->
            Typ.check __implicit13__ c ;
            Typ.check __implicit13__ b ;
            Typ.check __implicit13__ a ;
            () ) }
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
