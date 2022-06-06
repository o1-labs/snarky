module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let f __implicit2__ __implicit1__ (x : 'x * 'y) (y : 'b) b =
  let y =
    let typ x___2 = x___2 in
    Snarky.exists (typ __implicit2__)
      ~compute:
        (let open As_prover in
        fun () ->
          let typ x___1 = x___1 in
          As_prover.read (typ __implicit1__) y)
  in
  if b then x else y

let g __implicit4__ (b : boolean) (x : field_var) =
  ignore
    (f
       { Snarky.Types.Typ.store =
           (fun (x0, x1) ->
             Snarky.Typ_monads.Store.bind
               (Typ.boolean.Snarky.Types.Typ.store x0) ~f:(fun x0 ->
                 Snarky.Typ_monads.Store.bind
                   (Typ.boolean.Snarky.Types.Typ.store x1) ~f:(fun x1 ->
                     Snarky.Typ_monads.Store.return (x0, x1) ) ) )
       ; Snarky.Types.Typ.read =
           (fun (x0, x1) ->
             Snarky.Typ_monads.Read.bind (Typ.boolean.Snarky.Types.Typ.read x0)
               ~f:(fun x0 ->
                 Snarky.Typ_monads.Read.bind
                   (Typ.boolean.Snarky.Types.Typ.read x1) ~f:(fun x1 ->
                     Snarky.Typ_monads.Read.return (x0, x1) ) ) )
       ; Snarky.Types.Typ.alloc =
           Snarky.Typ_monads.Alloc.bind Typ.boolean.Snarky.Types.Typ.alloc
             ~f:(fun x0 ->
               Snarky.Typ_monads.Alloc.bind Typ.boolean.Snarky.Types.Typ.alloc
                 ~f:(fun x1 -> Snarky.Typ_monads.Alloc.return (x0, x1)) )
       ; Snarky.Types.Typ.check =
           (fun (x0, x1) ->
             Snarky.Checked.bind (Typ.boolean.Snarky.Types.Typ.check x0)
               ~f:(fun () ->
                 Snarky.Checked.bind (Typ.boolean.Snarky.Types.Typ.check x1)
                   ~f:(fun () -> Snarky.Checked.return ()) ) )
       }
       __implicit4__ (b, b) (true, false) true ) ;
  f
    { Snarky.Types.Typ.store =
        (fun (x0, x1) ->
          Snarky.Typ_monads.Store.bind (Typ.field.Snarky.Types.Typ.store x0)
            ~f:(fun x0 ->
              Snarky.Typ_monads.Store.bind (Typ.field.Snarky.Types.Typ.store x1)
                ~f:(fun x1 -> Snarky.Typ_monads.Store.return (x0, x1)) ) )
    ; Snarky.Types.Typ.read =
        (fun (x0, x1) ->
          Snarky.Typ_monads.Read.bind (Typ.field.Snarky.Types.Typ.read x0)
            ~f:(fun x0 ->
              Snarky.Typ_monads.Read.bind (Typ.field.Snarky.Types.Typ.read x1)
                ~f:(fun x1 -> Snarky.Typ_monads.Read.return (x0, x1)) ) )
    ; Snarky.Types.Typ.alloc =
        Snarky.Typ_monads.Alloc.bind Typ.field.Snarky.Types.Typ.alloc
          ~f:(fun x0 ->
            Snarky.Typ_monads.Alloc.bind Typ.field.Snarky.Types.Typ.alloc
              ~f:(fun x1 -> Snarky.Typ_monads.Alloc.return (x0, x1)) )
    ; Snarky.Types.Typ.check =
        (fun (x0, x1) ->
          Snarky.Checked.bind (Typ.field.Snarky.Types.Typ.check x0)
            ~f:(fun () ->
              Snarky.Checked.bind (Typ.field.Snarky.Types.Typ.check x1)
                ~f:(fun () -> Snarky.Checked.return ()) ) )
    }
    { Snarky.Types.Typ.store =
        (fun (x0, x1) ->
          Snarky.Typ_monads.Store.bind (Typ.field.Snarky.Types.Typ.store x0)
            ~f:(fun x0 ->
              Snarky.Typ_monads.Store.bind (Typ.field.Snarky.Types.Typ.store x1)
                ~f:(fun x1 -> Snarky.Typ_monads.Store.return (x0, x1)) ) )
    ; Snarky.Types.Typ.read =
        (fun (x0, x1) ->
          Snarky.Typ_monads.Read.bind (Typ.field.Snarky.Types.Typ.read x0)
            ~f:(fun x0 ->
              Snarky.Typ_monads.Read.bind (Typ.field.Snarky.Types.Typ.read x1)
                ~f:(fun x1 -> Snarky.Typ_monads.Read.return (x0, x1)) ) )
    ; Snarky.Types.Typ.alloc =
        Snarky.Typ_monads.Alloc.bind Typ.field.Snarky.Types.Typ.alloc
          ~f:(fun x0 ->
            Snarky.Typ_monads.Alloc.bind Typ.field.Snarky.Types.Typ.alloc
              ~f:(fun x1 -> Snarky.Typ_monads.Alloc.return (x0, x1)) )
    ; Snarky.Types.Typ.check =
        (fun (x0, x1) ->
          Snarky.Checked.bind (Typ.field.Snarky.Types.Typ.check x0)
            ~f:(fun () ->
              Snarky.Checked.bind (Typ.field.Snarky.Types.Typ.check x1)
                ~f:(fun () -> Snarky.Checked.return ()) ) )
    }
    (x, x) (x, x) false
