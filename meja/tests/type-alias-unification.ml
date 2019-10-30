module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module Alias_alias = struct
  type nonrec ('a, 'b) u = 'a -> 'a

  type nonrec ('a, 'b) v = ('a, 'a) u

  let f (x : (int, bool) u) : (int, int) u = x

  let g (x : (int, int) v) : (int, bool) v = x

  let h (x : (int, bool) v) : (int, int) u = x

  let i (x : (bool, bool) u) : (bool, unit) v = x
end

module Alias_opaque = struct
  type nonrec ('a, 'b) u

  type nonrec ('a, 'b) v = ('a, 'a) u

  let f (x : (int, int) v) : (int, bool) v = x

  let g (x : (int, bool) v) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v = x
end

module Alias_record = struct
  include struct
    type nonrec ('a, 'b) u = {a: 'a; b: 'b}

    and ('a, 'b) u = {a: 'a; b: 'b}

    let u_typ x___1050 x___1049 =
      { Snarky.Types.Typ.store=
          (fun {a; b} ->
            Snarky.Typ_monads.Store.bind (x___1050.Snarky.Types.Typ.store a)
              ~f:(fun a ->
                Snarky.Typ_monads.Store.bind
                  (x___1049.Snarky.Types.Typ.store b) ~f:(fun b ->
                    Snarky.Typ_monads.Store.return {a; b} ) ) )
      ; Snarky.Types.Typ.read=
          (fun {a; b} ->
            Snarky.Typ_monads.Read.bind (x___1050.Snarky.Types.Typ.read a)
              ~f:(fun a ->
                Snarky.Typ_monads.Read.bind (x___1049.Snarky.Types.Typ.read b)
                  ~f:(fun b -> Snarky.Typ_monads.Read.return {a; b}) ) )
      ; Snarky.Types.Typ.alloc=
          Snarky.Typ_mondas.Alloc.bind x___1050.Snarky.Types.Typ.alloc
            ~f:(fun a ->
              Snarky.Typ_mondas.Alloc.bind x___1049.Snarky.Types.Typ.alloc
                ~f:(fun b -> Snarky.Typ_monads.Alloc.return {a; b}) )
      ; Snarky.Types.Typ.check=
          (fun {a; b} ->
            Snarky.Checked.bind (x___1050.Snarky.Types.Typ.check a)
              ~f:(fun () ->
                Snarky.Checked.bind (x___1049.Snarky.Types.Typ.check b)
                  ~f:(fun () -> Snarky.Checked.return ()) ) ) }
  end

  include struct
    type nonrec ('a, 'b) v = ('a, 'a) u

    and ('a, 'b) v = ('a, 'a) u

    let v_typ x___1055 x___1054 = u_typ x___1055 x___1055
  end

  let f (x : (int, int) v) : (int, bool) v = x

  let g (x : (int, bool) v) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v = x
end

module Alias_variant = struct
  type nonrec ('a, 'b) u = A | B | C of 'a | D of 'b

  type nonrec ('a, 'b) v = ('a, 'a) u

  let f (x : (int, int) v) : (int, bool) v = x

  let g (x : (int, bool) v) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v = x
end
