module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type nonrec 'a t = int

  and 'a t = int

  let typ x___1009 =
    { Snarky.Types.Typ.store= (fun x -> Snarky.Typ_monads.Store.return x)
    ; Snarky.Types.Typ.read= (fun x -> Snarky.Typ_monads.Read.return x)
    ; Snarky.Types.Typ.alloc=
        (let open Snarky.Typ_monads.Alloc in
        map alloc ~f:(fun _ -> failwith "cannot allocate this type."))
    ; Snarky.Types.Typ.check= (fun _ -> Snarky.Checked.return ()) }
end

let x : bool t = 15

let f (y : int t) = y

let g = f x
