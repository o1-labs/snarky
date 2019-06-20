open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

include struct
  type _ Snarky.Request.t += Request : bool Request.t

  let handle_Request = function
    | With {request= Request _; respond} ->
        let unhandled = Snarky.Request.unhandled in
        respond (Provide Boolean.false_)
    | _ ->
        Snarky.Request.unhandled
end

include struct
  type _ Snarky.Request.t += Request2 : Field.Constant.t Request.t

  let handle_Request2 = function
    | With {request= Request2 _; respond} ->
        let unhandled = Snarky.Request.unhandled in
        respond (Provide (Field.Constant.of_string "0"))
    | _ ->
        Snarky.Request.unhandled
end
