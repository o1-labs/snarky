module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let typ __implicit1__ __implicit2__ :
    ('var_a * 'var_b, 'value_a * 'value_b) Typ.t =
  { Typ.store =
      (fun (a, b) ->
        Typ.Store.bind (Typ.store __implicit1__ b) (fun b ->
            Typ.Store.bind (Typ.store __implicit2__ a) (fun a ->
                Typ.Store.return (a, b))))
  ; Typ.read =
      (fun (a, b) ->
        Typ.Read.bind (Typ.read __implicit1__ b) (fun b ->
            Typ.Read.bind (Typ.read __implicit2__ a) (fun a ->
                Typ.Read.return (a, b))))
  ; Typ.alloc =
      Typ.Alloc.bind (Typ.alloc __implicit1__) (fun b ->
          Typ.Alloc.bind (Typ.alloc __implicit2__) (fun a ->
              Typ.Alloc.return (a, b)))
  ; Typ.check =
      (fun (a, b) -> Typ.check __implicit2__ a ; Typ.check __implicit1__ b)
  }
