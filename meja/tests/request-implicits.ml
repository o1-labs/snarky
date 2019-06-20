open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

include struct
  type 'a tuple = 'a * 'a * 'a

  type 'a tuple_var = 'a * 'a * 'a
end

include struct
  type _ Snarky.Request.t +=
    | Get_path : bool tuple -> Field.Constant.t tuple Request.t
end

type foo = Foo

let get_path __implicit1__ (addr : Boolean.var tuple_var) : foo =
  exists __implicit1__ ~request:(fun () ->
      Get_path
        ((As_prover.read (Typ.tuple3 Boolean.typ Boolean.typ Boolean.typ)) addr)
  )

let (get_path :
      (_, Field.Constant.t tuple) Typ.t -> Boolean.var tuple_var -> unit) =
 fun __implicit6__ (addr : Boolean.var tuple_var) ->
  ( let path =
      exists __implicit6__ ~request:(fun () ->
          Get_path
            ((As_prover.read (Typ.tuple3 Boolean.typ Boolean.typ Boolean.typ))
               addr) )
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
