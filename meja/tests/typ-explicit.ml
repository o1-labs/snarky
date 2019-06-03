module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let typ __implicit8__ __implicit5__ __implicit1__ __implicit7__ __implicit6__
    __implicit2__ : ('var_a * 'var_b, 'value_a * 'value_b) Typ.t =
  { Typ.store=
      (fun (a, b) ->
        Typ.Store.bind
          ((Typ.store __implicit1__) b)
          (fun b ->
            Typ.Store.bind
              ((Typ.store __implicit2__) a)
              (fun a -> Typ.Store.return (a, b)) ) )
  ; Typ.read=
      (fun (a, b) ->
        Typ.Read.bind
          ((Typ.read __implicit1__) b)
          (fun b ->
            Typ.Read.bind
              ((Typ.read __implicit2__) a)
              (fun a -> Typ.Read.return (a, b)) ) )
  ; Typ.alloc=
      Typ.Alloc.bind (Typ.alloc __implicit5__) (fun b ->
          Typ.Alloc.bind (Typ.alloc __implicit6__) (fun a ->
              Typ.Alloc.return (a, b) ) )
  ; Typ.check=
      (fun (a, b) ->
        (Typ.check __implicit7__) a ;
        (Typ.check __implicit8__) b ) }
