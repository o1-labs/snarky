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

  let wrap f _ = f

  let with_read f tbl s = (s, f tbl)
end

include T

module Provider = struct
  include Provider.T

  let run t stack tbl s (handler : Request.Handler.t) =
    match t with
    | Request rc ->
        let s', r = run rc tbl s in
        (s', Request.Handler.run handler stack r)
    | Compute c -> run c tbl s
    | Both (rc, c) -> (
        let s', r = run rc tbl s in
        match Request.Handler.run handler stack r with
        | exception _ -> run c tbl s
        | x -> (s', x) )
end
