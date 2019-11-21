module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type nonrec ('a, 'b, 'c) t = {a: 'a; b: 'b; c: 'c}

  and ('a, 'b, 'c) t = {a: 'a; b: 'b; c: 'c}

  let typ x___1018 x___1017 x___1016 =
    { Snarky.Types.Typ.store=
        (fun {a; b; c} ->
          Snarky.Typ_monads.Store.bind (x___1018.Snarky.Types.Typ.store a)
            ~f:(fun a ->
              Snarky.Typ_monads.Store.bind (x___1017.Snarky.Types.Typ.store b)
                ~f:(fun b ->
                  Snarky.Typ_monads.Store.bind
                    (x___1016.Snarky.Types.Typ.store c) ~f:(fun c ->
                      Snarky.Typ_monads.Store.return {a; b; c} ) ) ) )
    ; Snarky.Types.Typ.read=
        (fun {a; b; c} ->
          Snarky.Typ_monads.Read.bind (x___1018.Snarky.Types.Typ.read a)
            ~f:(fun a ->
              Snarky.Typ_monads.Read.bind (x___1017.Snarky.Types.Typ.read b)
                ~f:(fun b ->
                  Snarky.Typ_monads.Read.bind
                    (x___1016.Snarky.Types.Typ.read c) ~f:(fun c ->
                      Snarky.Typ_monads.Read.return {a; b; c} ) ) ) )
    ; Snarky.Types.Typ.alloc=
        Snarky.Typ_monads.Alloc.bind x___1018.Snarky.Types.Typ.alloc
          ~f:(fun a ->
            Snarky.Typ_monads.Alloc.bind x___1017.Snarky.Types.Typ.alloc
              ~f:(fun b ->
                Snarky.Typ_monads.Alloc.bind x___1016.Snarky.Types.Typ.alloc
                  ~f:(fun c -> Snarky.Typ_monads.Alloc.return {a; b; c}) ) )
    ; Snarky.Types.Typ.check=
        (fun {a; b; c} ->
          Snarky.Checked.bind (x___1018.Snarky.Types.Typ.check a) ~f:(fun () ->
              Snarky.Checked.bind (x___1017.Snarky.Types.Typ.check b)
                ~f:(fun () ->
                  Snarky.Checked.bind (x___1016.Snarky.Types.Typ.check c)
                    ~f:(fun () -> Snarky.Checked.return ()) ) ) ) }
end

let x = {a= 15; b= 20; c= 25}

let y = {a= true; b= false; c= true}

let z = {a= x.a; b= y.b; c= ()}

module X = struct
  include struct
    type nonrec 'a t = {a: 'a; b: 'a; c: 'a}

    and 'a t = {a: 'a; b: 'a; c: 'a}

    let typ x___1031 =
      { Snarky.Types.Typ.store=
          (fun {a; b; c} ->
            Snarky.Typ_monads.Store.bind (x___1031.Snarky.Types.Typ.store a)
              ~f:(fun a ->
                Snarky.Typ_monads.Store.bind
                  (x___1031.Snarky.Types.Typ.store b) ~f:(fun b ->
                    Snarky.Typ_monads.Store.bind
                      (x___1031.Snarky.Types.Typ.store c) ~f:(fun c ->
                        Snarky.Typ_monads.Store.return {a; b; c} ) ) ) )
      ; Snarky.Types.Typ.read=
          (fun {a; b; c} ->
            Snarky.Typ_monads.Read.bind (x___1031.Snarky.Types.Typ.read a)
              ~f:(fun a ->
                Snarky.Typ_monads.Read.bind (x___1031.Snarky.Types.Typ.read b)
                  ~f:(fun b ->
                    Snarky.Typ_monads.Read.bind
                      (x___1031.Snarky.Types.Typ.read c) ~f:(fun c ->
                        Snarky.Typ_monads.Read.return {a; b; c} ) ) ) )
      ; Snarky.Types.Typ.alloc=
          Snarky.Typ_monads.Alloc.bind x___1031.Snarky.Types.Typ.alloc
            ~f:(fun a ->
              Snarky.Typ_monads.Alloc.bind x___1031.Snarky.Types.Typ.alloc
                ~f:(fun b ->
                  Snarky.Typ_monads.Alloc.bind x___1031.Snarky.Types.Typ.alloc
                    ~f:(fun c -> Snarky.Typ_monads.Alloc.return {a; b; c}) ) )
      ; Snarky.Types.Typ.check=
          (fun {a; b; c} ->
            Snarky.Checked.bind (x___1031.Snarky.Types.Typ.check a)
              ~f:(fun () ->
                Snarky.Checked.bind (x___1031.Snarky.Types.Typ.check b)
                  ~f:(fun () ->
                    Snarky.Checked.bind (x___1031.Snarky.Types.Typ.check c)
                      ~f:(fun () -> Snarky.Checked.return ()) ) ) ) }
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
