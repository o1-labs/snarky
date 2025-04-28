open Core_kernel

module Make (Backend : sig
  module Field : sig
    type t
  end

  module Cvar : sig
    type t
  end
end)
(Types : Types.Types
           with type field = Backend.Field.t
            and type field_var = Backend.Cvar.t
            and type 'a As_prover.t = (Backend.Cvar.t -> Backend.Field.t) -> 'a) :
  As_prover_intf.S with module Types := Types = struct
  type 'a t = 'a Types.As_prover.t

  let map t ~f tbl =
    let x = t tbl in
    f x

  let bind t ~f tbl =
    let x = t tbl in
    f x tbl

  let return x _ = x

  let run t tbl = t tbl

  let map2 x y ~f tbl =
    let x = x tbl in
    let y = y tbl in
    f x y

  let read_var (v : 'var) : 'field t = fun tbl -> tbl v

  let read
      (Typ { var_to_fields; value_of_fields; _ } : ('var, 'value) Types.Typ.t)
      (var : 'var) : 'value t =
   fun tbl ->
    let field_vars, aux = var_to_fields var in
    let fields = Array.map ~f:tbl field_vars in
    value_of_fields (fields, aux)

  include Monad_let.Make (struct
    type nonrec 'a t = 'a t

    let map = `Custom map

    let bind = bind

    let return = return
  end)

  module Provider = struct
    include Types.Provider

    let run t tbl (handler : Request.Handler.t) =
      match t with
      | Request rc ->
          let r = run rc tbl in
          Request.Handler.run handler r
      | Compute c ->
          Some (run c tbl)
      | Both (rc, c) -> (
          let r = run rc tbl in
          match Request.Handler.run handler r with
          | None | (exception _) ->
              Some (run c tbl)
          | x ->
              x )
  end

  module Handle = struct
    let value (t : ('var, 'value) Handle.t) : 'value t =
     fun _ -> Option.value_exn t.value
  end
end
