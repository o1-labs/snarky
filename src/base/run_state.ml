module Vector = struct
  open Core_kernel

  type 'elt t =
    | T :
        (module Snarky_intf.Vector.S with type elt = 'elt and type t = 't)
        * 't Type_equal.Id.t
        * 't
        -> 'elt t

  let unit = Type_equal.Id.create ~name:"unit" Unit.sexp_of_t

  let null : type a. a t =
    let module T = struct
      type elt = a

      type t = unit

      let create () = ()

      let get _ _ = failwith "Vector.null: get"

      let emplace_back _ _ = failwith "Vector.null: emplace_back"

      let length () = 0
    end in
    T ((module T), unit, ())

  let get (type x) (T ((module T), _, t) : x t) i = T.get t i

  let emplace_back (type x) (T ((module T), _, t) : x t) x = T.emplace_back t x
end

(** The internal state used to run a checked computation. *)
type 'field t =
  { system : 'field Constraint_system.t option
  ; input : 'field Vector.t
  ; aux : 'field Vector.t
  ; eval_constraints : bool
  ; num_inputs : int
  ; next_auxiliary : int ref
  ; has_witness : bool
  ; stack : string list
  ; handler : Request.Handler.t
  ; is_running : bool
  ; as_prover : bool ref
  ; log_constraint :
      (   ?at_label_boundary:[ `Start | `End ] * string
       -> ('field Cvar.t, 'field) Constraint.t option
       -> unit )
      option
  }
