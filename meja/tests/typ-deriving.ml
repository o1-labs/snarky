module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type ('a1, 'field) poly = {a: 'a1; b: 'field}

  type 'field t = (bool, 'field) poly

  type 'field t = (Boolean.var, 'field) poly

  let typ : (_, t) Typ.t =
    { Typ.store=
        (fun {a; b; _} ->
          Typ.Store.bind (Typ.store b) (fun b ->
              Typ.Store.bind (Typ.store a) (fun a -> Typ.Store.return {a; b})
          ) )
    ; Typ.read=
        (fun {a; b; _} ->
          Typ.Read.bind (Snarky.read b) (fun b ->
              Typ.Read.bind (Snarky.read a) (fun a -> Typ.Read.return {a; b})
          ) )
    ; Typ.alloc=
        (fun {a; b; _} ->
          Typ.Alloc.bind Typ.alloc (fun b ->
              Typ.Alloc.bind Typ.alloc (fun a -> Typ.Alloc.return {a; b}) ) )
    ; Typ.check=
        (fun {a; b; _} ->
          (fun f x -> f x) (Typ.check b) (fun b ->
              (fun f x -> f x) (Typ.check a) (fun a -> ()) ) ) }
end

include struct
  type ('a, 'a1) u_poly = {a1: 'a; b1: 'a1}

  type 'a u = ('a, bool) u_poly

  type 'a u = ('a, Boolean.var) u_poly

  let u_typ : (_, _ u) Typ.t =
    { Typ.store=
        (fun {a1; b1; _} ->
          Typ.Store.bind (Typ.store b1) (fun b1 ->
              Typ.Store.bind (Typ.store a1) (fun a1 -> Typ.Store.return {a1; b1})
          ) )
    ; Typ.read=
        (fun {a1; b1; _} ->
          Typ.Read.bind (Snarky.read b1) (fun b1 ->
              Typ.Read.bind (Snarky.read a1) (fun a1 -> Typ.Read.return {a1; b1})
          ) )
    ; Typ.alloc=
        (fun {a1; b1; _} ->
          Typ.Alloc.bind Typ.alloc (fun b1 ->
              Typ.Alloc.bind Typ.alloc (fun a1 -> Typ.Alloc.return {a1; b1}) )
          )
    ; Typ.check=
        (fun {a1; b1; _} ->
          (fun f x -> f x) (Typ.check b1) (fun b1 ->
              (fun f x -> f x) (Typ.check a1) (fun a1 -> ()) ) ) }
end

include struct
  type ('a, 'b) v = {a2: 'a; b2: 'b}

  let v_typ : (_, (_, _) v) Typ.t =
    { Typ.store=
        (fun {a2; b2; _} ->
          Typ.Store.bind (Typ.store b2) (fun b2 ->
              Typ.Store.bind (Typ.store a2) (fun a2 -> Typ.Store.return {a2; b2})
          ) )
    ; Typ.read=
        (fun {a2; b2; _} ->
          Typ.Read.bind (Snarky.read b2) (fun b2 ->
              Typ.Read.bind (Snarky.read a2) (fun a2 -> Typ.Read.return {a2; b2})
          ) )
    ; Typ.alloc=
        (fun {a2; b2; _} ->
          Typ.Alloc.bind Typ.alloc (fun b2 ->
              Typ.Alloc.bind Typ.alloc (fun a2 -> Typ.Alloc.return {a2; b2}) )
          )
    ; Typ.check=
        (fun {a2; b2; _} ->
          (fun f x -> f x) (Typ.check b2) (fun b2 ->
              (fun f x -> f x) (Typ.check a2) (fun a2 -> ()) ) ) }
end
