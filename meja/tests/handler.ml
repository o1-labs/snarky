module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type _ Snarky.Request.t += A : 'a Request.t
end

let handle_A = function
  | With {request= A; respond} ->
      unhandled
  | _ ->
      Request.Unhandled

include struct
  type _ Snarky.Request.t += B : 'a -> 'a Request.t
end

let handle_B = function
  | With {request= B y; respond} ->
      respond (Request.Response.Provide y)
  | _ ->
      Request.Unhandled
