module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type nonrec t = {a: bool; b: field}

type nonrec 'a u = {a1: 'a; b1: bool}

include struct
  type nonrec ('a, 'b) v = {a2: 'a; b2: 'b}

  and ('a, 'b) v = {a2: 'a; b2: 'b}

  let v_typ x___1031 x___1030 =
    { Snarky.Types.Typ.store=
        (fun {a2; b2} ->
          Snarky.Typ_monads.Store.bind (x___1031.Snarky.Types.Typ.store a2)
            ~f:(fun a2 ->
              Snarky.Typ_monads.Store.bind (x___1030.Snarky.Types.Typ.store b2)
                ~f:(fun b2 -> Snarky.Typ_monads.Store.return {a2; b2}) ) )
    ; Snarky.Types.Typ.read=
        (fun {a2; b2} ->
          Snarky.Typ_monads.Read.bind (x___1031.Snarky.Types.Typ.read a2)
            ~f:(fun a2 ->
              Snarky.Typ_monads.Read.bind (x___1030.Snarky.Types.Typ.read b2)
                ~f:(fun b2 -> Snarky.Typ_monads.Read.return {a2; b2}) ) )
    ; Snarky.Types.Typ.alloc=
        Snarky.Typ_mondas.Alloc.bind x___1031.Snarky.Types.Typ.alloc
          ~f:(fun a2 ->
            Snarky.Typ_mondas.Alloc.bind x___1030.Snarky.Types.Typ.alloc
              ~f:(fun b2 -> Snarky.Typ_monads.Alloc.return {a2; b2}) )
    ; Snarky.Types.Typ.check=
        (fun {a2; b2} ->
          Snarky.Checked.bind (x___1031.Snarky.Types.Typ.check a2)
            ~f:(fun () ->
              Snarky.Checked.bind (x___1030.Snarky.Types.Typ.check b2)
                ~f:(fun () -> Snarky.Checked.return ()) ) ) }
end
