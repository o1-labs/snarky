open Core_kernel
module Constraint0 = Constraint

exception Runtime_error of string * string list * exn * string

(* Register a printer for [Runtime_error], so that the user sees a useful,
   well-formatted message. This will contain all of the information that
   raising the original error would have raised, along with the extra
   information added to [Runtime_error].

   NOTE: The message in its entirety is included in [Runtime_error], so that
         all of the information will be visible to the user in some form even
         if they don't use the [Print_exc] pretty-printer.
*)
let () =
  Stdlib.Printexc.register_printer (fun exn ->
      match exn with
      | Runtime_error (message, _, _, _) ->
          Some
            (Printf.sprintf
               "Snarky.Checked_runner.Runtime_error(_, _, _, _)\n\n%s" message )
      | _ ->
          None )

let eval_constraints = ref true

module Make_checked
    (Backend : Backend_extended.S)
    (As_prover : As_prover_intf.S with type 'f field := Backend.Field.t) =
struct
  type run_state = Backend.Field.t Run_state.t

  module Types = struct
    module Checked = struct
      type ('a, 'f) t = run_state -> run_state * 'a
    end

    module As_prover = struct
      type ('a, 'f) t = ('a, 'f) As_prover.t
    end

    module Typ = struct
      include Types.Typ.T

      type ('var, 'value, 'f) t = ('var, 'value, 'f, (unit, 'f) Checked.t) typ
    end

    module Provider = struct
      include Types.Provider.T

      type ('a, 'f) t =
        (('a Request.t, 'f) As_prover.t, ('a, 'f) As_prover.t) provider
    end
  end

  type 'f field = Backend.Field.t

  type ('a, 'f) t = ('a, 'f field) Types.Checked.t

  include Monad_let.Make2 (struct
    type ('a, 'f) t = ('a, 'f field) Types.Checked.t

    let return x s = (s, x)

    let map =
      `Custom
        (fun x ~f s ->
          let s, a = x s in
          (s, f a) )

    let bind x ~f s =
      let s, a = x s in
      f a s
  end)

  open Constraint
  open Backend
  open Run_state
  open Checked_ast

  let get_value { num_inputs; input; aux; _ } : Cvar.t -> Field.t =
    let get_one i =
      if i <= num_inputs then Run_state.Vector.get input (i - 1)
      else Run_state.Vector.get aux (i - num_inputs - 1)
    in
    Cvar.eval (`Return_values_will_be_mutated get_one)

  let store_field_elt { next_auxiliary; aux; _ } x =
    let v = !next_auxiliary in
    incr next_auxiliary ;
    Run_state.Vector.emplace_back aux x ;
    Cvar.Unsafe.of_index v

  let alloc_var { next_auxiliary; _ } () =
    let v = !next_auxiliary in
    incr next_auxiliary ; Cvar.Unsafe.of_index v

  let run_as_prover x state =
    match (x, state.has_witness) with
    | Some x, true ->
        let old = !(state.as_prover) in
        state.as_prover := true ;
        let y = As_prover.run x (get_value state) in
        state.as_prover := old ;
        (state, Some y)
    | _, _ ->
        (state, None)

  let as_prover x s =
    let s', (_ : unit option) = run_as_prover (Some x) s in
    (s', ())

  let mk_lazy x s =
    let old_stack = s.stack in
    ( s
    , Lazy.from_fun (fun () ->
          let { stack; _ } = s in

          (* Add a label to indicate that the new stack is the point at which
             this was forced. When printed for errors, this will split the
             stack into

             ...
             stack to lazy
             ...

             Lazy value forced at:
             ...
             stack to lazy forcing point
             ...
          *)
          let label = "\nLazy value forced at:" in
          let _s', y = x () { s with stack = old_stack @ (label :: stack) } in
          y ) )

  let with_label lab t s =
    let { stack; _ } = s in
    let s', y = t () { s with stack = lab :: stack } in
    ({ s' with stack }, y)

  let log_constraint { basic; _ } s =
    match basic with
    | Boolean var ->
        Format.(asprintf "Boolean %s" (Field.to_string (get_value s var)))
    | Equal (var1, var2) ->
        Format.(
          asprintf "Equal %s %s"
            (Field.to_string (get_value s var1))
            (Field.to_string (get_value s var2)))
    | Square (var1, var2) ->
        Format.(
          asprintf "Square %s %s"
            (Field.to_string (get_value s var1))
            (Field.to_string (get_value s var2)))
    | R1CS (var1, var2, var3) ->
        Format.(
          asprintf "R1CS %s %s %s"
            (Field.to_string (get_value s var1))
            (Field.to_string (get_value s var2))
            (Field.to_string (get_value s var3)))
    | _ ->
        Format.asprintf
          !"%{sexp:(Field.t, Field.t) Constraint0.basic}"
          (Constraint0.Basic.map basic ~f:(get_value s))

  let stack_to_string = String.concat ~sep:"\n"

  let add_constraint ~stack ({ basic; annotation } : Constraint.t)
      (Constraint_system.T ((module C), system) : Field.t Constraint_system.t) =
    let label = Option.value annotation ~default:"<unknown>" in
    C.add_constraint system basic ~label:(stack_to_string (label :: stack))

  let add_constraint c s =
    if !(s.as_prover) then
      (* Don't add constraints as the prover, or the constraint system won't match! *)
      (s, ())
    else (
      Option.iter s.log_constraint ~f:(fun f -> f (Some c)) ;
      if s.eval_constraints && not (Constraint.eval c (get_value s)) then
        failwithf
          "Constraint unsatisfied (unreduced):\n\
           %s\n\
           %s\n\n\
           Constraint:\n\
           %s\n\
           Data:\n\
           %s"
          (Constraint.annotation c) (stack_to_string s.stack)
          (Sexp.to_string (Constraint.sexp_of_t c))
          (log_constraint c s) () ;
      if not !(s.as_prover) then
        Option.iter s.system ~f:(fun system ->
            add_constraint ~stack:s.stack c system ) ;
      (s, ()) )

  let with_handler h t s =
    let { handler; _ } = s in
    let s', y = t () { s with handler = Request.Handler.push handler h } in
    ({ s' with handler }, y)

  let exists
      (Types.Typ.Typ
        { Types.Typ.var_of_fields
        ; value_to_fields
        ; size_in_field_elements
        ; check
        ; constraint_system_auxiliary
        ; _
        } ) p s =
    if s.has_witness then (
      let old = !(s.as_prover) in
      s.as_prover := true ;
      let value = As_prover.Provider.run p s.stack (get_value s) s.handler in
      s.as_prover := old ;
      let var =
        let store_value =
          if !(s.as_prover) then
            (* If we're nested in a prover block, create constants instead of
               storing.
            *)
            Cvar.constant
          else store_field_elt s
        in
        let fields, aux = value_to_fields value in
        let field_vars = Array.map ~f:store_value fields in
        var_of_fields (field_vars, aux)
      in
      (* TODO: Push a label onto the stack here *)
      let s, () = check var s in
      (s, { Handle.var; value = Some value }) )
    else
      let var =
        var_of_fields
          ( Array.init size_in_field_elements ~f:(fun _ -> alloc_var s ())
          , constraint_system_auxiliary () )
      in
      (* TODO: Push a label onto the stack here *)
      let s, () = check var s in
      (s, { Handle.var; value = None })

  let next_auxiliary () s = (s, !(s.next_auxiliary))

  let constraint_count ?(weight = Fn.const 1)
      ?(log = fun ?start:_ _lab _pos -> ()) t =
    (* TODO: Integrate log with log_constraint *)
    let count = ref 0 in
    let log_constraint ?at_label_boundary c =
      ( match at_label_boundary with
      | None ->
          ()
      | Some (pos, lab) ->
          let start = match pos with `Start -> true | _ -> false in
          log ~start lab !count ) ;
      count := !count + Option.value_map ~default:0 ~f:weight c
    in
    let state =
      Run_state.
        { system = None
        ; input = Vector.null
        ; aux = Vector.null
        ; eval_constraints = false
        ; num_inputs = 0
        ; next_auxiliary = ref 1
        ; has_witness = false
        ; stack = []
        ; handler = Request.Handler.fail
        ; is_running = true
        ; as_prover = ref false
        ; log_constraint = Some log_constraint
        }
    in
    let _ = t () state in
    !count
end

module type Run_extras = sig
  type field

  type cvar

  module Types : Types.Types

  val get_value : field Run_state.t -> cvar -> field

  val store_field_elt : field Run_state.t -> field -> cvar

  val alloc_var : 'b Run_state.t -> unit -> cvar

  val run_as_prover :
       ('a, field) Types.As_prover.t option
    -> field Run_state.t
    -> field Run_state.t * 'a option
end

module Make (Backend : Backend_extended.S) = struct
  open Backend
  open Run_state

  let constraint_logger = ref None

  let set_constraint_logger f = constraint_logger := Some f

  let clear_constraint_logger () = constraint_logger := None

  module Checked_runner = Make_checked (Backend) (As_prover)
  open Checked_runner

  type run_state = Checked_runner.run_state

  type state = run_state

  type ('a, 't) run = 't -> run_state -> run_state * 'a

  include (
    Checked_runner :
      sig
        include
          Checked_intf.Basic
            with module Types := Checked_runner.Types
            with type 'f field := 'f Checked_runner.field

        include
          Run_extras
            with module Types := Checked_runner.Types
            with type field := Backend.Field.t
             and type cvar := Backend.Cvar.t
      end )

  module Types = Checked_ast.Types

  let handle_error s f =
    try f () with
    | Runtime_error (message, stack, exn, bt) ->
        (* NOTE: We create a new [Runtime_error] instead of re-using the old
                 one. Re-using the old one will fill the backtrace with call
                 and re-raise messages, one per iteration of this function,
                 which are irrelevant to the user.
        *)
        raise (Runtime_error (message, stack, exn, bt))
    | exn ->
        let bt = Printexc.get_backtrace () in
        raise
          (Runtime_error
             ( Printf.sprintf
                 "Encountered an error while evaluating the checked computation:\n\
                 \  %s\n\n\
                  Label stack trace:\n\
                  %s\n\n\n\
                  %s"
                 (Exn.to_string exn) (stack_to_string s.stack) bt
             , s.stack
             , exn
             , bt ) )

  (* INVARIANT: run _ s = (s', _) gives
       (s'.prover_state = Some _) iff (s.prover_state = Some _) *)
  let rec run : type a. (a, Field.t) Checked_ast.t -> run_state -> run_state * a
      =
   fun t s ->
    match t with
    | As_prover (x, k) ->
        let s, () = handle_error s (fun () -> as_prover x s) in
        run k s
    | Pure x ->
        (s, x)
    | Direct (d, k) ->
        let s, y = handle_error s (fun () -> d s) in
        let k = handle_error s (fun () -> k y) in
        run k s
    | Lazy (x, k) ->
        let s, y = mk_lazy (fun () -> run x) s in
        let k = handle_error s (fun () -> k y) in
        run k s
    | With_label (lab, t, k) ->
        Option.iter s.log_constraint ~f:(fun f ->
            f ~at_label_boundary:(`Start, lab) None ) ;
        let s, y = with_label lab (fun () -> run t) s in
        Option.iter s.log_constraint ~f:(fun f ->
            f ~at_label_boundary:(`End, lab) None ) ;
        let k = handle_error s (fun () -> k y) in
        run k s
    | Add_constraint (c, t) ->
        let s, () = handle_error s (fun () -> add_constraint c s) in
        run t s
    | With_handler (h, t, k) ->
        let s, y = with_handler h (fun () -> run t) s in
        let k = handle_error s (fun () -> k y) in
        run k s
    | Exists
        ( Typ
            { var_to_fields
            ; var_of_fields
            ; value_to_fields
            ; value_of_fields
            ; size_in_field_elements
            ; constraint_system_auxiliary
            ; check
            }
        , p
        , k ) ->
        let typ =
          Types.Typ.Typ
            { var_to_fields
            ; var_of_fields
            ; value_to_fields
            ; value_of_fields
            ; size_in_field_elements
            ; constraint_system_auxiliary
            ; check = (fun var -> run (check var))
            }
        in
        let s, y = handle_error s (fun () -> exists typ p s) in
        let k = handle_error s (fun () -> k y) in
        run k s
    | Next_auxiliary k ->
        let s, y = next_auxiliary () s in
        let k = handle_error s (fun () -> k y) in
        run k s

  let dummy_vector = Run_state.Vector.null

  let fake_state next_auxiliary stack =
    { system = None
    ; input = dummy_vector
    ; aux = dummy_vector
    ; eval_constraints = false
    ; num_inputs = 0
    ; next_auxiliary
    ; has_witness = false
    ; stack
    ; handler = Request.Handler.fail
    ; is_running = true
    ; as_prover = ref false
    ; log_constraint = None
    }

  module State = struct
    let make ~num_inputs ~input ~next_auxiliary ~aux ?system
        ?(eval_constraints = !eval_constraints) ?handler ~with_witness () =
      next_auxiliary := 1 + num_inputs ;
      (* We can't evaluate the constraints if we are not computing over a value. *)
      let eval_constraints = eval_constraints && with_witness in
      Option.iter
        (system : R1CS_constraint_system.t option)
        ~f:(fun system ->
          R1CS_constraint_system.set_primary_input_size system num_inputs ) ;
      let system =
        Option.map system ~f:(fun sys ->
            let module M = struct
              module Field = struct
                type nonrec t = Field.t
              end

              include R1CS_constraint_system
            end in
            Constraint_system.T ((module M), sys) )
      in
      { system
      ; input
      ; aux
      ; eval_constraints
      ; num_inputs
      ; next_auxiliary
      ; has_witness = with_witness
      ; stack = []
      ; handler = Option.value handler ~default:Request.Handler.fail
      ; is_running = true
      ; as_prover = ref false
      ; log_constraint = !constraint_logger
      }
  end
end

module type S = sig
  include Run_extras

  type constr

  type r1cs

  val set_constraint_logger :
    (?at_label_boundary:[ `Start | `End ] * string -> constr -> unit) -> unit

  val clear_constraint_logger : unit -> unit

  type run_state = field Run_state.t

  type state = run_state

  type ('a, 't) run = 't -> run_state -> run_state * 'a

  val run : ('a, field) Types.Checked.t -> run_state -> run_state * 'a

  module State : sig
    val make :
         num_inputs:int
      -> input:field Run_state.Vector.t
      -> next_auxiliary:int ref
      -> aux:field Run_state.Vector.t
      -> ?system:r1cs
      -> ?eval_constraints:bool
      -> ?handler:Request.Handler.t
      -> with_witness:bool
      -> unit
      -> field Run_state.t
  end
end
