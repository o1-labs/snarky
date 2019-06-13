module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type ('a, 'b, 'c) t = {a: 'a; b: 'b; c: 'c}

  let typ __implicit1__ __implicit2__ __implicit3__ :
      (('a2, 'b2, 'c2) t, ('a1, 'b1, 'c1) t) Typ.t =
    { Typ.store=
        (fun {a; b; c; _} ->
          Typ.Store.bind
            ((Typ.store __implicit1__) c)
            ~f:(fun c ->
              Typ.Store.bind
                ((Typ.store __implicit2__) b)
                ~f:(fun b ->
                  Typ.Store.bind
                    ((Typ.store __implicit3__) a)
                    ~f:(fun a -> Typ.Store.return {a; b; c}) ) ) )
    ; Typ.read=
        (fun {a; b; c; _} ->
          Typ.Read.bind
            ((Typ.read __implicit1__) c)
            ~f:(fun c ->
              Typ.Read.bind
                ((Typ.read __implicit2__) b)
                ~f:(fun b ->
                  Typ.Read.bind
                    ((Typ.read __implicit3__) a)
                    ~f:(fun a -> Typ.Read.return {a; b; c}) ) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit1__) ~f:(fun c ->
            Typ.Alloc.bind (Typ.alloc __implicit2__) ~f:(fun b ->
                Typ.Alloc.bind (Typ.alloc __implicit3__) ~f:(fun a ->
                    Typ.Alloc.return {a; b; c} ) ) )
    ; Typ.check=
        (fun {a; b; c; _} ->
          (Typ.check __implicit1__) c ;
          (Typ.check __implicit2__) b ;
          (Typ.check __implicit3__) a ;
          () ) }
end

let x =
  { a= Field.constant (Field.Constant.of_string "15")
  ; b= Field.constant (Field.Constant.of_string "20")
  ; c= Field.constant (Field.Constant.of_string "25") }

let y = {a= true; b= false; c= true}

let z = {a= x.a; b= y.b; c= ()}

module X = struct
  include struct
    type 'a t = {a: 'a; b: 'a; c: 'a}

    let typ __implicit13__ : ('a2 t, 'a1 t) Typ.t =
      { Typ.store=
          (fun {a; b; c; _} ->
            Typ.Store.bind
              ((Typ.store __implicit13__) c)
              ~f:(fun c ->
                Typ.Store.bind
                  ((Typ.store __implicit13__) b)
                  ~f:(fun b ->
                    Typ.Store.bind
                      ((Typ.store __implicit13__) a)
                      ~f:(fun a -> Typ.Store.return {a; b; c}) ) ) )
      ; Typ.read=
          (fun {a; b; c; _} ->
            Typ.Read.bind
              ((Typ.read __implicit13__) c)
              ~f:(fun c ->
                Typ.Read.bind
                  ((Typ.read __implicit13__) b)
                  ~f:(fun b ->
                    Typ.Read.bind
                      ((Typ.read __implicit13__) a)
                      ~f:(fun a -> Typ.Read.return {a; b; c}) ) ) )
      ; Typ.alloc=
          Typ.Alloc.bind (Typ.alloc __implicit13__) ~f:(fun c ->
              Typ.Alloc.bind (Typ.alloc __implicit13__) ~f:(fun b ->
                  Typ.Alloc.bind (Typ.alloc __implicit13__) ~f:(fun a ->
                      Typ.Alloc.return {a; b; c} ) ) )
      ; Typ.check=
          (fun {a; b; c; _} ->
            (Typ.check __implicit13__) c ;
            (Typ.check __implicit13__) b ;
            (Typ.check __implicit13__) a ;
            () ) }
  end

  let x =
    { a= Field.constant (Field.Constant.of_string "1")
    ; b= Field.constant (Field.Constant.of_string "1")
    ; c= Field.constant (Field.Constant.of_string "1") }
end

let a = {X.x with b= Field.constant (Field.Constant.of_string "12")}

let b =
  { X.a= Field.constant (Field.Constant.of_string "1")
  ; b= Field.constant (Field.Constant.of_string "1")
  ; c= Field.constant (Field.Constant.of_string "1") }

let c = {x with a= Field.constant (Field.Constant.of_string "35")}

let d = (a.b, b.b)

let e = a.X.a

let f = {a= true; b= (); c= Field.constant (Field.Constant.of_string "15")}.a

let g = {X.a= (); b= (); c= ()}.a

let h = {X.a= (); b= (); c= ()}.X.a
