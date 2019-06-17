open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

include struct
  type ('a, 'b) poly = {a: 'a; b: 'b}

  type t = (bool, Field.Constant.t) poly

  type var = (bool, Field.t) poly

  let typ __implicit1__ __implicit2__ : (('a1, 'b1) poly, ('a, 'b) poly) Typ.t
      =
    { Typ.store=
        (fun {a; b; _} ->
          Typ.Store.bind (Typ.store __implicit1__ b) ~f:(fun b ->
              Typ.Store.bind (Typ.store __implicit2__ a) ~f:(fun a ->
                  Typ.Store.return {a; b} ) ) )
    ; Typ.read=
        (fun {a; b; _} ->
          Typ.Read.bind (Typ.read __implicit1__ b) ~f:(fun b ->
              Typ.Read.bind (Typ.read __implicit2__ a) ~f:(fun a ->
                  Typ.Read.return {a; b} ) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit1__) ~f:(fun b ->
            Typ.Alloc.bind (Typ.alloc __implicit2__) ~f:(fun a ->
                Typ.Alloc.return {a; b} ) )
    ; Typ.check=
        (fun {a; b; _} ->
          Typ.check __implicit1__ b ; Typ.check __implicit2__ a ; () ) }
end

include struct
  type ('a1, 'b1) u_poly = {a1: 'a1; b1: 'b1}

  type 'a u = ('a, bool) u_poly

  type 'a u_var = ('a, bool) u_poly

  let u_typ __implicit9__ __implicit10__ :
      (('a11, 'b11) u_poly, ('a1, 'b1) u_poly) Typ.t =
    { Typ.store=
        (fun {a1; b1; _} ->
          Typ.Store.bind (Typ.store __implicit9__ b1) ~f:(fun b1 ->
              Typ.Store.bind (Typ.store __implicit10__ a1) ~f:(fun a1 ->
                  Typ.Store.return {a1; b1} ) ) )
    ; Typ.read=
        (fun {a1; b1; _} ->
          Typ.Read.bind (Typ.read __implicit9__ b1) ~f:(fun b1 ->
              Typ.Read.bind (Typ.read __implicit10__ a1) ~f:(fun a1 ->
                  Typ.Read.return {a1; b1} ) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit9__) ~f:(fun b1 ->
            Typ.Alloc.bind (Typ.alloc __implicit10__) ~f:(fun a1 ->
                Typ.Alloc.return {a1; b1} ) )
    ; Typ.check=
        (fun {a1; b1; _} ->
          Typ.check __implicit9__ b1 ;
          Typ.check __implicit10__ a1 ;
          () ) }
end

include struct
  type ('a, 'b) v = {a2: 'a; b2: 'b}

  let v_typ __implicit17__ __implicit18__ : (('a1, 'b1) v, ('a, 'b) v) Typ.t =
    { Typ.store=
        (fun {a2; b2; _} ->
          Typ.Store.bind (Typ.store __implicit17__ b2) ~f:(fun b2 ->
              Typ.Store.bind (Typ.store __implicit18__ a2) ~f:(fun a2 ->
                  Typ.Store.return {a2; b2} ) ) )
    ; Typ.read=
        (fun {a2; b2; _} ->
          Typ.Read.bind (Typ.read __implicit17__ b2) ~f:(fun b2 ->
              Typ.Read.bind (Typ.read __implicit18__ a2) ~f:(fun a2 ->
                  Typ.Read.return {a2; b2} ) ) )
    ; Typ.alloc=
        Typ.Alloc.bind (Typ.alloc __implicit17__) ~f:(fun b2 ->
            Typ.Alloc.bind (Typ.alloc __implicit18__) ~f:(fun a2 ->
                Typ.Alloc.return {a2; b2} ) )
    ; Typ.check=
        (fun {a2; b2; _} ->
          Typ.check __implicit17__ b2 ;
          Typ.check __implicit18__ a2 ;
          () ) }
end
