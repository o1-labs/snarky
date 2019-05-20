module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type ('a, 'b) poly = {a: 'a; b: 'b}

  type 'field t = (bool, 'field) poly

  type var = (Boolean.var, Field.Var.t) poly

  let typ __implicit7__ __implicit5__ __implicit1__ __implicit8__ __implicit6__
      __implicit2__ : (('a1, 'b1) poly, ('a, 'b) poly) Typ.t =
    { Typ.store=
        (fun {a; b; _} ->
          Typ.Store.bind
            ((Typ.store __implicit1__) b)
            (fun b ->
              Typ.Store.bind
                ((Typ.store __implicit2__) a)
                (fun a -> Typ.Store.return {a; b}) ) )
    ; Typ.read=
        (fun {a; b; _} ->
          Typ.Read.bind
            ((Typ.read __implicit1__) b)
            (fun b ->
              Typ.Read.bind
                ((Typ.read __implicit2__) a)
                (fun a -> Typ.Read.return {a; b}) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit5__) (fun b ->
            Typ.Alloc.bind (Typ.alloc __implicit6__) (fun a ->
                Typ.Alloc.return {a; b} ) )
    ; Typ.check=
        (fun {a; b; _} ->
          (fun x f -> f x)
            ((Typ.check __implicit7__) b)
            (fun b ->
              (fun x f -> f x) ((Typ.check __implicit8__) a) (fun a -> ()) ) )
    }
end

include struct
  type ('a1, 'b1) u_poly = {a1: 'a1; b1: 'b1}

  type 'a u = ('a, bool) u_poly

  type 'a u_var = ('a, Boolean.var) u_poly

  let u_typ __implicit15__ __implicit13__ __implicit9__ __implicit16__
      __implicit14__ __implicit10__ :
      (('a11, 'b11) u_poly, ('a1, 'b1) u_poly) Typ.t =
    { Typ.store=
        (fun {a1; b1; _} ->
          Typ.Store.bind
            ((Typ.store __implicit9__) b1)
            (fun b1 ->
              Typ.Store.bind
                ((Typ.store __implicit10__) a1)
                (fun a1 -> Typ.Store.return {a1; b1}) ) )
    ; Typ.read=
        (fun {a1; b1; _} ->
          Typ.Read.bind
            ((Typ.read __implicit9__) b1)
            (fun b1 ->
              Typ.Read.bind
                ((Typ.read __implicit10__) a1)
                (fun a1 -> Typ.Read.return {a1; b1}) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit13__) (fun b1 ->
            Typ.Alloc.bind (Typ.alloc __implicit14__) (fun a1 ->
                Typ.Alloc.return {a1; b1} ) )
    ; Typ.check=
        (fun {a1; b1; _} ->
          (fun x f -> f x)
            ((Typ.check __implicit15__) b1)
            (fun b1 ->
              (fun x f -> f x) ((Typ.check __implicit16__) a1) (fun a1 -> ())
              ) ) }
end

include struct
  type ('a, 'b) v = {a2: 'a; b2: 'b}

  let v_typ __implicit23__ __implicit21__ __implicit17__ __implicit24__
      __implicit22__ __implicit18__ : (('a1, 'b1) v, ('a, 'b) v) Typ.t =
    { Typ.store=
        (fun {a2; b2; _} ->
          Typ.Store.bind
            ((Typ.store __implicit17__) b2)
            (fun b2 ->
              Typ.Store.bind
                ((Typ.store __implicit18__) a2)
                (fun a2 -> Typ.Store.return {a2; b2}) ) )
    ; Typ.read=
        (fun {a2; b2; _} ->
          Typ.Read.bind
            ((Typ.read __implicit17__) b2)
            (fun b2 ->
              Typ.Read.bind
                ((Typ.read __implicit18__) a2)
                (fun a2 -> Typ.Read.return {a2; b2}) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit21__) (fun b2 ->
            Typ.Alloc.bind (Typ.alloc __implicit22__) (fun a2 ->
                Typ.Alloc.return {a2; b2} ) )
    ; Typ.check=
        (fun {a2; b2; _} ->
          (fun x f -> f x)
            ((Typ.check __implicit23__) b2)
            (fun b2 ->
              (fun x f -> f x) ((Typ.check __implicit24__) a2) (fun a2 -> ())
              ) ) }
end
