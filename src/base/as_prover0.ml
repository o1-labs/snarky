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

  let set_state s _tbl _ = (s, ())

  let modify_state f _tbl s = (f s, ())

  let map2 x y ~f tbl s =
    let s, x = x tbl s in
    let s, y = y tbl s in
    (s, f x y)

  let read_var (v : 'var) : ('field, 'field, 's) t = fun tbl s -> (s, tbl v)

  let read
      (Typ { var_to_fields; value_of_fields; _ } :
        ('var, 'value, 'field, _) Types.Typ.t) (var : 'var) :
      ('value, 'field, 'prover_state) t =
   fun tbl s ->
    let field_vars, aux = var_to_fields var in
    let fields = Array.map ~f:tbl field_vars in
    let value = value_of_fields (fields, aux) in
    (s, value)

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

    let with_lens lens t =
      match t with
      | Request r ->
          Request (with_lens lens r)
      | Compute c ->
          Compute (with_lens lens c)
      | Both (r, c) ->
          Both (with_lens lens r, with_lens lens c)
  end

  module Handle = struct
    let value (t : ('var, 'value) Handle.t) : ('value, 'field, 's) t =
     fun _ s -> (s, Option.value_exn t.value)
  end
end

include T
