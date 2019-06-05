module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module Alias_alias = struct
  type ('a, 'b) u = 'a -> 'a

  type ('a, 'b) v = ('a, 'a) u

  let f (x : (int, bool) u) : (int, int) u = x

  let g (x : (int, int) v) : (int, bool) v = x

  let h (x : (int, bool) v) : (int, int) u = x

  let i (x : (bool, bool) u) : (bool, unit) v = x
end

module Alias_opaque = struct
  type ('a, 'b) u

  type ('a, 'b) v = ('a, 'a) u

  let f (x : (int, int) v) : (int, bool) v = x

  let g (x : (int, bool) v) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v = x
end

module Alias_record = struct
  include struct
    type ('a, 'b) u = {a: 'a; b: 'b}

    let u_typ : (_, (_, _) u) Typ.t =
      { Typ.store=
          (fun {a; b; _} ->
            Typ.Store.bind (Typ.store b) (fun b ->
                Typ.Store.bind (Typ.store a) (fun a -> Typ.Store.return {a; b})
            ) )
      ; Typ.read=
          (fun {a; b; _} ->
            Typ.Read.bind (Snarky.read b) (fun b ->
                Typ.Read.bind (Snarky.read a) (fun a -> Typ.Read.return {a; b})
            ) )
      ; Typ.alloc=
          (fun {a; b; _} ->
            Typ.Alloc.bind Typ.alloc (fun b ->
                Typ.Alloc.bind Typ.alloc (fun a -> Typ.Alloc.return {a; b}) )
            )
      ; Typ.check=
          (fun {a; b; _} ->
            (fun f x -> f x) (Typ.check b) (fun b ->
                (fun f x -> f x) (Typ.check a) (fun a -> ()) ) ) }
  end

  type ('a, 'b) v = ('a, 'a) u

  let f (x : (int, int) v) : (int, bool) v = x

  let g (x : (int, bool) v) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v = x
end

module Alias_variant = struct
  type ('a, 'b) u = A | B | C of 'a | D of 'b

  type ('a, 'b) v = ('a, 'a) u

  let f (x : (int, int) v) : (int, bool) v = x

  let g (x : (int, bool) v) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v = x
end
