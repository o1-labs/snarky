open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

module Alias_alias = struct
  include struct
    type ('a, 'b) u = 'a -> 'a

    type ('a, 'b) u_var = 'a -> 'a
  end

  include struct
    type ('a, 'b) v = ('a, 'a) u

    type ('a, 'b) v_var = ('a, 'a) u_var
  end

  let f (x : (int, bool) u_var) : (int, int) u_var = x

  let g (x : (int, int) v_var) : (int, bool) v_var = x

  let h (x : (int, bool) v_var) : (int, int) u_var = x

  let i (x : (bool, bool) u_var) : (bool, unit) v_var = x
end

module Alias_opaque = struct
  type ('a, 'b) u

  include struct
    type ('a, 'b) v = ('a, 'a) u

    type ('a, 'b) v_var = ('a, 'a) u
  end

  let f (x : (int, int) v_var) : (int, bool) v_var = x

  let g (x : (int, bool) v_var) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v_var = x
end

module Alias_record = struct
  include struct
    type ('a, 'b) u = {a: 'a; b: 'b}

    let u_typ __implicit1__ __implicit2__ : (('a2, 'b2) u, ('a1, 'b1) u) Typ.t
        =
      { Typ.store=
          (fun {a; b; _} ->
            Typ.Store.bind (Typ.store __implicit1__ b) ~f:(fun b ->
                Typ.Store.bind (Typ.store __implicit2__ a) ~f:(fun a ->
                    Typ.Store.return {a; b} ) ) )
      ; Typ.read=
          (fun {a; b; _} ->
            Typ.Read.bind (Typ.read __implicit1__ b) ~f:(fun b ->
                Typ.Read.bind (Typ.read __implicit2__ a) ~f:(fun a ->
                    Typ.Read.return {a; b} ) ) )
      ; Typ.alloc=
          Typ.Alloc.bind (Typ.alloc __implicit1__) ~f:(fun b ->
              Typ.Alloc.bind (Typ.alloc __implicit2__) ~f:(fun a ->
                  Typ.Alloc.return {a; b} ) )
      ; Typ.check=
          (fun {a; b; _} ->
            make_checked (fun () ->
                Typ.check __implicit1__ b ; Typ.check __implicit2__ a ; () ) )
      }
  end

  include struct
    type ('a, 'b) v = ('a, 'a) u

    type ('a, 'b) v_var = ('a, 'a) u
  end

  let f (x : (int, int) v_var) : (int, bool) v_var = x

  let g (x : (int, bool) v_var) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v_var = x
end

module Alias_variant = struct
  type ('a, 'b) u = A | B | C of 'a | D of 'b

  include struct
    type ('a, 'b) v = ('a, 'a) u

    type ('a, 'b) v_var = ('a, 'a) u
  end

  let f (x : (int, int) v_var) : (int, bool) v_var = x

  let g (x : (int, bool) v_var) : (int, int) u = x

  let h (x : (bool, bool) u) : (bool, unit) v_var = x
end
