open Core_kernel

module Vector = struct
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

module Make
    (Field : T) (Cvar : sig
      type t

      val of_int_unsafe : int -> t
    end) =
struct
  (** The internal state used to run a checked computation. *)
  type t =
    { system : Field.t Constraint_system.t option
    ; input : Field.t Vector.t
    ; aux : Field.t Vector.t
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
         -> (Cvar.t, Field.t) Constraint.t option
         -> unit )
        option
    }

  let make ~num_inputs ~input ~next_auxiliary ~aux ?system ~eval_constraints
      ?log_constraint ?handler ~with_witness ?(stack = []) ?(is_running = true)
      () =
    next_auxiliary := 1 + num_inputs ;
    (* We can't evaluate the constraints if we are not computing over a value. *)
    let eval_constraints = eval_constraints && with_witness in
    { system
    ; input
    ; aux
    ; eval_constraints
    ; num_inputs
    ; next_auxiliary
    ; has_witness = with_witness
    ; stack
    ; handler = Option.value handler ~default:Request.Handler.fail
    ; is_running
    ; as_prover = ref false
    ; log_constraint
    }

  let get_variable_value { num_inputs; input; aux; _ } : int -> 'field =
   fun i ->
    if i <= num_inputs then Vector.get input (i - 1)
    else Vector.get aux (i - num_inputs - 1)

  let store_field_elt { next_auxiliary; aux; _ } x =
    let v = !next_auxiliary in
    incr next_auxiliary ; Vector.emplace_back aux x ; Cvar.of_int_unsafe v

  let alloc_var { next_auxiliary; _ } () =
    let v = !next_auxiliary in
    incr next_auxiliary ; Cvar.of_int_unsafe v

  let has_witness { has_witness; _ } = has_witness

  let as_prover { as_prover; _ } = !as_prover

  let set_as_prover t as_prover = t.as_prover := as_prover

  let stack { stack; _ } = stack

  let set_stack t stack = { t with stack }

  let log_constraint { log_constraint; _ } = log_constraint

  let eval_constraints { eval_constraints; _ } = eval_constraints

  let system { system; _ } = system

  let handler { handler; _ } = handler

  let set_handler t handler = { t with handler }

  let is_running { is_running; _ } = is_running

  let set_is_running t is_running = { t with is_running }

  let next_auxiliary { next_auxiliary; _ } = !next_auxiliary
end