module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module type P = sig
  include
    sig
      val x : int

      val f : bool -> bool
  end
end

module P = struct
  include struct
    let x = 15

    let f b = b
  end
end

let f __implicit1__ =
  let typ x___1 = Typ.fn x___1 x___1 in
  Snarky.exists (typ __implicit1__)
    ~compute:
      (let open As_prover in
      fun () x -> x)

include struct
  let g __implicit2__ x =
    (let typ x___2 = Typ.fn x___2 x___2 in
     As_prover.read (typ __implicit2__) (f __implicit2__))
      x
end

let h __implicit6__ __implicit5__ __implicit4__ x =
  let typ x___4 = x___4 in
  Snarky.exists (typ __implicit6__)
    ~compute:
      (let open As_prover in
      fun () ->
        g __implicit4__
          (let typ x___3 = x___3 in
           As_prover.read (typ __implicit5__) x))

let i () =
  let f __implicit7__ =
    let typ x___5 = Typ.fn x___5 x___5 in
    Snarky.exists (typ __implicit7__)
      ~compute:
        (let open As_prover in
        fun () x -> x)
  in
  let i =
    let typ = Snarky.Typ.Internal.ref () in
    Snarky.exists typ
      ~compute:
        (let open As_prover in
        fun () ->
          (let typ x___6 = Typ.fn x___6 x___6 in
           As_prover.read
             (typ (Snarky.Typ.Internal.ref ()))
             (f (Snarky.Typ.Internal.ref ())))
            15)
  in
  i

type nonrec ('a, 'b) either = Fst of 'a | Snd of 'b

let j __implicit12__ __implicit13__ __implicit14__ __implicit10__ b x =
  let f __implicit11__ =
    let typ x___10 = Typ.fn x___10 (Snarky.Typ.Internal.ref ()) in
    Snarky.exists (typ __implicit11__)
      ~compute:
        (let open As_prover in
        fun () x ->
          if
            let typ x___9 = x___9 in
            As_prover.read (typ __implicit10__) b
          then Fst x
          else Snd 15)
  in
  let i =
    let typ = Snarky.Typ.Internal.ref () in
    Snarky.exists typ
      ~compute:
        (let open As_prover in
        fun () ->
          (let typ x___11 = Typ.fn x___11 (Snarky.Typ.Internal.ref ()) in
           As_prover.read (typ __implicit13__) (f __implicit12__))
            (let typ x___12 = x___12 in
             As_prover.read (typ __implicit14__) x))
  in
  i

let k __implicit15__ __implicit16__ (f : field) =
  j __implicit15__ __implicit16__ Typ.field
    { Snarky.Types.Typ.store= (fun x -> Snarky.Typ_monads.Store.return x)
    ; Snarky.Types.Typ.read= (fun x -> Snarky.Typ_monads.Read.return x)
    ; Snarky.Types.Typ.alloc=
        (let open Snarky.Typ_monads.Alloc in
        map alloc ~f:(fun _ -> failwith "cannot allocate this type."))
    ; Snarky.Types.Typ.check= (fun _ -> Snarky.Checked.return ()) }
    true f

let l __implicit19__ __implicit20__ (b : boolean) (b' : boolean) =
  j __implicit19__ __implicit20__ Typ.boolean Typ.boolean b b'
