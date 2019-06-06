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

    let u_typ __implicit1__ __implicit2__ : (('a2, 'b2) u, ('a1, 'b1) u) Typ.t
        =
      { Typ.store=
          (fun {a; b; _} ->
            Typ.Store.bind
              ((Typ.store __implicit1__) b)
              (fun b ->
                Typ.Store.bind
                  ((Typ.store __implicit2__) a)
                  (fun a -> Typ.Store.return {a; b}) ) )
      ; Typ.read=
          (fun {a; b; _} ->
            Typ.Read.bind
              ((Typ.read __implicit1__) b)
              (fun b ->
                Typ.Read.bind
                  ((Typ.read __implicit2__) a)
                  (fun a -> Typ.Read.return {a; b}) ) )
      ; Typ.alloc=
          Typ.Alloc.bind (Typ.alloc __implicit1__) (fun b ->
              Typ.Alloc.bind (Typ.alloc __implicit2__) (fun a ->
                  Typ.Alloc.return {a; b} ) )
      ; Typ.check=
          (fun {a; b; _} ->
            (fun x f -> f x)
              ((Typ.check __implicit1__) b)
              (fun b ->
                (fun x f -> f x) ((Typ.check __implicit2__) a) (fun a -> ()) )
            ) }
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
