module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module Alias_alias = struct
  include struct
    type nonrec ('a, 'b) u = 'a -> 'a

    and ('a, 'b) u = 'a -> 'a

    let u_typ x___1010 x___1009 = Typ.fn x___1010 x___1010
  end

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
  type nonrec ('a, 'b) u = {a: 'a; b: 'b}

  type nonrec ('a, 'b) v = ('a, 'a) u

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
