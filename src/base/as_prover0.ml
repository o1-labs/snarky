open Core_kernel

type ('a, 'f, 'cvar) t = ('a, 'f, 'cvar) Types.As_prover.t

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

let read_var (v : 'var) : ('field, 'field, 'cvar) t = fun tbl -> tbl v

let read
    (Typ { var_to_fields; value_of_fields; _ } :
      ('var, 'value, 'field, 'cvar, _) Types.Typ.t ) (var : 'var) :
    ('value, 'field, 'cvar) t =
 fun tbl ->
  let field_vars, aux = var_to_fields var in
  let fields = Array.map ~f:tbl field_vars in
  value_of_fields (fields, aux)

include Monad_let.Make3 (struct
  type nonrec ('a, 'e, 'cvar) t = ('a, 'e, 'cvar) t

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
  type nonrec ('a, 'f, 'cvar) t =
    (('a Request.t, 'f, 'cvar) t, ('a, 'f, 'cvar) t) Types.Provider.t

  open Types.Provider

  let run t stack tbl (handler : Request.Handler.t) =
    match t with
    | Request rc ->
        let r = run rc tbl in
        Request.Handler.run handler stack r
    | Compute c ->
        run c tbl
    | Both (rc, c) -> (
        let r = run rc tbl in
        match Request.Handler.run handler stack r with
        | exception _ ->
            run c tbl
        | x ->
            x )
end

module Handle = struct
  let value (t : ('var, 'value) Handle.t) : ('value, 'field, 'cvar) t =
   fun _ -> Option.value_exn t.value
end

(* functor to instantiate field and cvar *)

module type Extended = sig
  type field

  type cvar

  include As_prover_intf.Basic with type 'f field := field and type cvar := cvar

  type nonrec 'a t = ('a, field, cvar) t
end

module Make_extended
    (Field : T)
    (Cvar : T)
    (As_prover : As_prover_intf.Basic
                   with type 'f field := Field.t
                    and type cvar := Cvar.t) :
  Extended with type field := Field.t and type cvar := Cvar.t = struct
  include Field
  include As_prover

  type nonrec 'a t = ('a, Field.t, Cvar.t) t
end
