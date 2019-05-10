module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type _ Snarky.Request.t += A : 'a Snarky.Request.t
end

let handle_A = function
  | With {request= A; respond} ->
      let unhandled = Snarky.Request.unhandled in
      unhandled
  | _ ->
      Snarky.Request.unhandled

include struct
  type _ Snarky.Request.t += B : 'a -> 'a Snarky.Request.t
end

let x =
  let handle_B = function
    | With {request= B y; respond} ->
        let unhandled = Snarky.Request.unhandled in
        y
    | _ ->
        Snarky.Request.unhandled
  in
  ()
