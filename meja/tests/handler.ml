open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

include struct
  type _ Snarky.Request.t += A : 'a Request.t
end

let handle_A = function
  | With {request= A; respond} ->
      unhandled
  | _ ->
      Request.unhandled

include struct
  type _ Snarky.Request.t += B : 'a -> 'a Request.t
end

let handle_B = function
  | With {request= B y; respond} ->
      respond (Request.Response.Provide y)
  | _ ->
      Request.unhandled
