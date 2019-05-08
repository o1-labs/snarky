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

  let read ({read; _} : ('var, 'value, 'field, _) Types.Typ.t) (var : 'var) :
      ('value, 'field, 'prover_state) t =
   fun tbl s -> (s, Typ_monads.Read.run (read var) tbl)

  include Monad_let.Make3 (struct
    type nonrec ('a, 'e, 's) t = ('a, 'e, 's) t

    let map = `Custom map

    let bind = bind

    let return = return
  end)

  module Provider = struct
    type nonrec ('a, 'f, 's) t =
      (('a Request.t, 'f, 's) t, ('a, 'f, 's) t) Types.Provider.t

    open Types.Provider

    let run t stack tbl s (handler : Request.Handler.t) =
      match t with
      | Request rc ->
          let s', r = run rc tbl s in
          (s', Request.Handler.run handler stack r)
      | Compute c ->
          run c tbl s
      | Both (rc, c) -> (
          let s', r = run rc tbl s in
          match Request.Handler.run handler stack r with
          | exception _ ->
              run c tbl s
          | x ->
              (s', x) )
  end

  module Handle = struct
    let value (t : ('var, 'value) Handle.t) : ('value, 'field, 's) t =
     fun _ s -> (s, Option.value_exn t.value)
  end
end

include T
