type ('a, 'f, 's) t = ('f Cvar.t -> 'f) -> 's -> 's * 'a

module T = struct
  let map t ~f tbl s =
    let s', x = t tbl s in
    (s', f x)

  let bind t ~f tbl s =
    let s', x = t tbl s in
    f x tbl s'

  let return x _ s = (s, x)

  let run t tbl s = t tbl s

  let get_state _tbl s = (s, s)

  let set_state s _tbl _ = (s, ())

  let modify_state f _tbl s = (f s, ())

  let map2 x y ~f tbl s =
    let s, x = x tbl s in
    let s, y = y tbl s in
    (s, f x y)

  let read_var (v : 'var) : ('field, 'field, 's) t = fun tbl s -> (s, tbl v)

  include Monad_let.Make3 (struct
    type nonrec ('a, 'e, 's) t = ('a, 'e, 's) t

    let map = `Custom map

    let bind = bind

    let return = return
  end)

  let with_lens (lens : ('whole, 'view) Lens.t) as_prover tbl s =
    let s' = Lens.get lens s in
    let s', a = as_prover tbl s' in
    (Lens.set lens s s', a)
end

include T
