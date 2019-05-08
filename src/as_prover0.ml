open Core_kernel

type ('a, 'f, 's) t = ('a, 'f, 's) Types.As_prover.t

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

  let read_var v tbl s = (s, tbl v)

  let set_state s tbl _ = (s, ())

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

  module Handle = struct
    let value (t : ('var, 'value) Handle.t) : ('value, 'field, 's) t =
     fun _ s -> (s, Option.value_exn t.value)
  end
end

include T
