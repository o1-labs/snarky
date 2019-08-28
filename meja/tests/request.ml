module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type _ Snarky.Request.t += Request : 'x list -> 'x Snarky.Request.t
end

include struct
  type _ Snarky.Request.t += Request2 : 'x Snarky.Request.t

  let handle_Request2 = function
    | With {request= Request2; respond} ->
        let unhandled = Snarky.Request.unhandled in
        let _ = () in
        unhandled
    | _ ->
        Snarky.Request.unhandled
end

include struct
  type _ Snarky.Request.t += Request3 : 'x option -> 'x Snarky.Request.t

  let handle_Request3 = function
    | With {request= Request3 x; respond} -> (
        let unhandled = Snarky.Request.unhandled in
        match x with
        | None ->
            unhandled
        | Some x ->
            respond (Snarky.Request.Response.Provide x) )
    | _ ->
        Snarky.Request.unhandled
end
