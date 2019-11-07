module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type nonrec t = int Snarky.As_prover.Ref.t

  and t = int

  let typ = Snarky.Typ.Internal.ref ()
end

include struct
  type nonrec 'a u = 'a option Snarky.As_prover.Ref.t

  and 'a u = 'a option

  let u_typ x___1012 = Snarky.Typ.Internal.ref ()
end

include struct
  type nonrec ('a, 'b) v = ('a * 'b) Snarky.As_prover.Ref.t

  and ('a, 'b) v = 'a * 'b

  let v_typ x___1017 x___1016 = Snarky.Typ.Internal.ref ()
end

include struct
  type nonrec prover = A
end

include struct
  type nonrec w = prover Snarky.As_prover.Ref.t

  and w = prover

  let w_typ = Snarky.Typ.Internal.ref ()
end

module A = struct
  include struct
    type nonrec ('a, 'b) opaque = 'a * 'b

    and ('a, 'b) opaque = 'a * 'b

    let opaque_typ x___1027 x___1026 =
      { Snarky.Types.Typ.store=
          (fun (x0, x1) ->
            Snarky.Typ_monads.Store.bind (x___1027.Snarky.Types.Typ.store x0)
              ~f:(fun x0 ->
                Snarky.Typ_monads.Store.bind
                  (x___1026.Snarky.Types.Typ.store x1) ~f:(fun x1 ->
                    Snarky.Typ_monads.Store.return (x0, x1) ) ) )
      ; Snarky.Types.Typ.read=
          (fun (x0, x1) ->
            Snarky.Typ_monads.Read.bind (x___1027.Snarky.Types.Typ.read x0)
              ~f:(fun x0 ->
                Snarky.Typ_monads.Read.bind (x___1026.Snarky.Types.Typ.read x1)
                  ~f:(fun x1 -> Snarky.Typ_monads.Read.return (x0, x1)) ) )
      ; Snarky.Types.Typ.alloc=
          Snarky.Typ_monads.Alloc.bind x___1027.Snarky.Types.Typ.alloc
            ~f:(fun x0 ->
              Snarky.Typ_monads.Alloc.bind x___1026.Snarky.Types.Typ.alloc
                ~f:(fun x1 -> Snarky.Typ_monads.Alloc.return (x0, x1)) )
      ; Snarky.Types.Typ.check=
          (fun (x0, x1) ->
            Snarky.Checked.bind (x___1027.Snarky.Types.Typ.check x0)
              ~f:(fun () ->
                Snarky.Checked.bind (x___1026.Snarky.Types.Typ.check x1)
                  ~f:(fun () -> Snarky.Checked.return ()) ) ) }
  end

  type nonrec t = (int, bool) opaque

  include struct
    type nonrec u = (int, bool) opaque
  end

  let opaque = ()
end

include struct
  type nonrec x = prover Snarky.As_prover.Ref.t

  and x = prover

  let x_typ = w_typ
end
