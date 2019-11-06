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
  let typ x___1015 = Typ.fn x___1015 x___1015 in
  Snarky.exists (typ __implicit1__)
    ~compute:
      (let open As_prover in
      fun () x -> x)

include struct
  let g __implicit2__ x =
    (let typ x___1020 = Typ.fn x___1020 x___1020 in
     As_prover.read (typ __implicit2__) (f __implicit2__))
      x
end

let h __implicit6__ __implicit5__ __implicit4__ x =
  let typ x___1027 = x___1027 in
  Snarky.exists (typ __implicit6__)
    ~compute:
      (let open As_prover in
      fun () ->
        g __implicit4__
          (let typ x___1025 = x___1025 in
           As_prover.read (typ __implicit5__) x))

let i () =
  let f __implicit7__ =
    let typ x___1032 = Typ.fn x___1032 x___1032 in
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
          (let typ x___1036 = Typ.fn x___1036 x___1036 in
           As_prover.read
             (typ (Snarky.Typ.Internal.ref ()))
             (f (Snarky.Typ.Internal.ref ())))
            15)
  in
  i

type nonrec ('a, 'b) either = Fst of 'a | Snd of 'b

let j __implicit12__ __implicit13__ __implicit14__ __implicit10__ b x =
  let f __implicit11__ =
    let typ x___1057 = Typ.fn x___1057 (Snarky.Typ.Internal.ref ()) in
    Snarky.exists (typ __implicit11__)
      ~compute:
        (let open As_prover in
        fun () x ->
          if
            let typ x___1055 = x___1055 in
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
          (let typ x___1061 = Typ.fn x___1061 (Snarky.Typ.Internal.ref ()) in
           As_prover.read (typ __implicit13__) (f __implicit12__))
            (let typ x___1063 = x___1063 in
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
