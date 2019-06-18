open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

type 'a tuple = 'a * 'a * 'a

include struct
  type _ Snarky.Request.t +=
    | Get_path : bool tuple -> Field.Constant.t tuple Request.t
end

type foo = Foo

let get_path __implicit1__ __implicit2__ (addr : Boolean.var tuple) : foo =
  exists __implicit1__ ~request:(fun () ->
      Get_path ((As_prover.read __implicit2__) addr) )

let (get_path :
         (_, Field.Constant.t tuple) Typ.t
      -> (Boolean.var tuple, bool tuple) Typ.t
      -> Boolean.var tuple
      -> unit) =
 fun __implicit3__ __implicit4__ (addr : Boolean.var tuple) ->
  ( let path =
      exists __implicit3__ ~request:(fun () ->
          Get_path ((As_prover.read __implicit4__) addr) )
    in
    ()
    : unit )

include struct
  type _ Snarky.Request.t += Get_bit : bool Request.t
end

let (get_bit : Field.t -> Boolean.var) =
 fun (x : Field.t) -> exists Boolean.typ ~request:(fun () -> Get_bit)

let (get_bit : Field.t -> Boolean.var) =
 fun (x : Field.t) ->
  exists Boolean.typ ~request:(fun () ->
      let x = (As_prover.read Field.typ) x in
      Get_bit )
