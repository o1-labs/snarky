module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module A = struct
  let x = 15

  let y = 20

  include struct
    let x = true

    let y = false
  end
end

let z = A.x + A.y

include struct
  let z = A.x && A.y
end

let (z : int Snarky.As_prover.Ref.t) =
  let x = A.x in
  let y = A.y in
  let typ = Snarky.Typ.Internal.ref () in
  Snarky.exists typ
    ~compute:
      (let open As_prover in
      fun () -> if A.x && A.y then x else y)

let (z : boolean) =
  let typ = Typ.boolean in
  Snarky.exists typ
    ~compute:
      (let open As_prover in
      fun () -> A.x && A.y)

include struct
  type nonrec t = {a: field; b: boolean}

  and t = {a: field; b: bool}

  let typ =
    { Snarky.Types.Typ.store=
        (fun {a; b} ->
          Snarky.Typ_monads.Store.bind (Typ.field.Snarky.Types.Typ.store a)
            ~f:(fun a ->
              Snarky.Typ_monads.Store.bind
                (Typ.boolean.Snarky.Types.Typ.store b) ~f:(fun b ->
                  Snarky.Typ_monads.Store.return {a; b} ) ) )
    ; Snarky.Types.Typ.read=
        (fun {a; b} ->
          Snarky.Typ_monads.Read.bind (Typ.field.Snarky.Types.Typ.read a)
            ~f:(fun a ->
              Snarky.Typ_monads.Read.bind (Typ.boolean.Snarky.Types.Typ.read b)
                ~f:(fun b -> Snarky.Typ_monads.Read.return {a; b}) ) )
    ; Snarky.Types.Typ.alloc=
        Snarky.Typ_monads.Alloc.bind Typ.field.Snarky.Types.Typ.alloc
          ~f:(fun a ->
            Snarky.Typ_monads.Alloc.bind Typ.boolean.Snarky.Types.Typ.alloc
              ~f:(fun b -> Snarky.Typ_monads.Alloc.return {a; b}) )
    ; Snarky.Types.Typ.check=
        (fun {a; b} ->
          Snarky.Checked.bind (Typ.field.Snarky.Types.Typ.check a)
            ~f:(fun () ->
              Snarky.Checked.bind (Typ.boolean.Snarky.Types.Typ.check b)
                ~f:(fun () -> Snarky.Checked.return ()) ) ) }
end

let (failwith_field : string -> field) = failwith

include struct
  let field_plus (_ : field) =
    (let typ =
       Typ.fn
         { Snarky.Types.Typ.store= (fun x -> Snarky.Typ_monads.Store.return x)
         ; Snarky.Types.Typ.read= (fun x -> Snarky.Typ_monads.Read.return x)
         ; Snarky.Types.Typ.alloc=
             (let open Snarky.Typ_monads.Alloc in
             map alloc ~f:(fun _ -> failwith "cannot allocate this type."))
         ; Snarky.Types.Typ.check= (fun _ -> Snarky.Checked.return ()) }
         Typ.field
     in
     As_prover.read typ failwith_field)
      "no."

  let field_plus __implicit1__ (_ : field) =
    (let typ x___1028 =
       Typ.fn
         { Snarky.Types.Typ.store= (fun x -> Snarky.Typ_monads.Store.return x)
         ; Snarky.Types.Typ.read= (fun x -> Snarky.Typ_monads.Read.return x)
         ; Snarky.Types.Typ.alloc=
             (let open Snarky.Typ_monads.Alloc in
             map alloc ~f:(fun _ -> failwith "cannot allocate this type."))
         ; Snarky.Types.Typ.check= (fun _ -> Snarky.Checked.return ()) }
         x___1028
     in
     As_prover.read (typ __implicit1__) failwith)
      "no."
end

let in_out __implicit2__ (x : t) =
  let typ = typ in
  Snarky.exists typ
    ~compute:
      (let open As_prover in
      fun () ->
        let {a; b; _} =
          let typ = typ in
          As_prover.read typ x
        in
        {a= field_plus __implicit2__ a; b= not b})

let a __implicit4__ __implicit3__ a b =
  let typ = typ in
  Snarky.exists typ
    ~compute:
      (let open As_prover in
      fun () ->
        { a=
            (let typ x___1038 = x___1038 in
             As_prover.read (typ __implicit3__) a)
        ; b=
            (let typ x___1040 = x___1040 in
             As_prover.read (typ __implicit4__) b) })

let a_1 (x : field) (b : boolean) = a Typ.boolean Typ.field x b

let a_2 (x : field) =
  a
    { Snarky.Types.Typ.store= (fun x -> Snarky.Typ_monads.Store.return x)
    ; Snarky.Types.Typ.read= (fun x -> Snarky.Typ_monads.Read.return x)
    ; Snarky.Types.Typ.alloc=
        (let open Snarky.Typ_monads.Alloc in
        map alloc ~f:(fun _ -> failwith "cannot allocate this type."))
    ; Snarky.Types.Typ.check= (fun _ -> Snarky.Checked.return ()) }
    Typ.field x true
