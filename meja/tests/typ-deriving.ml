module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type nonrec t = {a: bool; b: field_var}

type nonrec 'a u = {a1: 'a; b1: bool}

include struct
  type nonrec ('a, 'b) v_var = {a2: 'a; b2: 'b}

  and ('a, 'b) v = {a2: 'a; b2: 'b}

  let v_typ x___3 x___2 =
    { Snarky.Types.Typ.store=
        (fun {a2; b2} ->
          Snarky.Typ_monads.Store.bind (x___3.Snarky.Types.Typ.store a2)
            ~f:(fun a2 ->
              Snarky.Typ_monads.Store.bind (x___2.Snarky.Types.Typ.store b2)
                ~f:(fun b2 -> Snarky.Typ_monads.Store.return {a2; b2}) ) )
    ; Snarky.Types.Typ.read=
        (fun {a2; b2} ->
          Snarky.Typ_monads.Read.bind (x___3.Snarky.Types.Typ.read a2)
            ~f:(fun a2 ->
              Snarky.Typ_monads.Read.bind (x___2.Snarky.Types.Typ.read b2)
                ~f:(fun b2 -> Snarky.Typ_monads.Read.return {a2; b2}) ) )
    ; Snarky.Types.Typ.alloc=
        Snarky.Typ_monads.Alloc.bind x___3.Snarky.Types.Typ.alloc ~f:(fun a2 ->
            Snarky.Typ_monads.Alloc.bind x___2.Snarky.Types.Typ.alloc
              ~f:(fun b2 -> Snarky.Typ_monads.Alloc.return {a2; b2}) )
    ; Snarky.Types.Typ.check=
        (fun {a2; b2} ->
          Snarky.Checked.bind (x___3.Snarky.Types.Typ.check a2) ~f:(fun () ->
              Snarky.Checked.bind (x___2.Snarky.Types.Typ.check b2)
                ~f:(fun () -> Snarky.Checked.return ()) ) ) }
end

let x __implicit3__ __implicit1__ x =
  let typ x___6 = v_typ x___6 x___6 in
  Snarky.exists (typ __implicit3__)
    ~compute:
      (let open As_prover in
      fun () ->
        { a2=
            (let typ x___4 = x___4 in
             As_prover.read (typ __implicit1__) x)
        ; b2=
            (let typ x___5 = x___5 in
             As_prover.read (typ __implicit1__) x) })

let y () =
  x Typ.boolean
    { Snarky.Types.Typ.store= (fun x -> Snarky.Typ_monads.Store.return x)
    ; Snarky.Types.Typ.read= (fun x -> Snarky.Typ_monads.Read.return x)
    ; Snarky.Types.Typ.alloc=
        (let open Snarky.Typ_monads.Alloc in
        map alloc ~f:(fun _ -> failwith "cannot allocate this type."))
    ; Snarky.Types.Typ.check= (fun _ -> Snarky.Checked.return ()) }
    true
