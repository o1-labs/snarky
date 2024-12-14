open Core_kernel

module Make (Backend : sig
  module Field : sig
    type t
  end
end)
(Types : Types.Types with type ('a, 'f) As_prover.t = ('f Cvar.t -> 'f) -> 'a) =
struct
  type 'a t = ('a, Backend.Field.t) Types.As_prover.t

  let map t ~f tbl =
    let x = t tbl in
    f x

  let bind t ~f tbl =
    let x = t tbl in
    f x tbl

  let return x _ = x

  let run t tbl = t tbl

  let get_state _tbl s = (s, s)

  let set_state s _tbl _ = (s, ())

  let modify_state f _tbl s = (f s, ())

  let map2 x y ~f tbl =
    let x = x tbl in
    let y = y tbl in
    f x y

  let read_var (v : 'var) : 'field t = fun tbl -> tbl v

  let read
      (Typ { var_to_fields; value_of_fields; _ } :
        ('var, 'value, 'field) Types.Typ.t ) (var : 'var) : 'value t =
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
    (** The different ways to generate a value of type ['a] for a circuit
        witness over field ['f].

        This is one of:
        * a [Request], dispatching an ['a Request.t];
        * [Compute], running a computation to generate the value;
        * [Both], attempting to dispatch an ['a Request.t], and falling back to
          the computation if the request is unhandled or raises an exception.
    *)
    type nonrec ('a, 'f) t = ('a, 'f) Types.Provider.t

    open Types.Provider

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

module Make_extended (Env : sig
  type field
end)
(As_prover : As_prover_intf.Basic with type field := Env.field) =
struct
  include Env
  include As_prover
end
