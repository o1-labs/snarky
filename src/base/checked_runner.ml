open Core_kernel
module Constraint0 = Constraint

let stack_to_string = String.concat ~sep:"\n"

let eval_constraints = ref true

let eval_constraints_ref = eval_constraints

module Simple = struct
  module Types = struct
    module Checked = struct
      type ('a, 'run_state) t =
        | Pure of 'a
        | Function of ('run_state -> 'run_state * 'a)
    end

    module Typ = struct
      include Types.Typ.T

      type ('var, 'value, 'f, 'field_var, 'run_state) t =
        ('var, 'value, 'f, 'field_var, (unit, 'run_state) Checked.t) typ
    end

    module Provider = struct
      include Types.Provider.T

      type ('a, 'f, 'field_var) t =
        ( ('a Request.t, 'f, 'field_var) As_prover0.t
        , ('a, 'f, 'field_var) As_prover0.t )
        provider
    end
  end

  type 'f field = 'f

  type ('a, 'run_state) t = ('a, 'run_state) Types.Checked.t

  let eval (t : ('a, 'run_state) t) : 'run_state -> 'run_state * 'a =
    match t with Pure a -> fun s -> (s, a) | Function g -> g

  include Monad_let.Make2 (struct
    type ('a, 'run_state) t = ('a, 'run_state) Types.Checked.t

    let return x : _ t = Pure x

    let map =
      `Custom
        (fun (x : _ t) ~f : _ t ->
          match x with
          | Pure a ->
              Pure (f a)
          | Function g ->
              Function
                (fun s ->
                  let s, a = g s in
                  (s, f a) ) )

    let bind (x : _ t) ~f : _ t =
      match x with
      | Pure a ->
          f a
      | Function g ->
          Function
            (fun s ->
              let s, a = g s in
              eval (f a) s )
  end)
end

module Make_checked
    (Backend : Backend_extended.S)
    (As_prover : As_prover_intf.Basic
                   with type 'f field := Backend.Field.t
                    and type 'f field_var := Backend.Cvar.t) =
struct
  type run_state = Backend.Run_state.t

  type state = run_state

  module Types = struct
    module Checked = struct
      type ('a, 'run_state) t = ('a, Backend.Run_state.t) Simple.Types.Checked.t
    end

    module Typ = struct
      include Types.Typ.T

      type ('var, 'value, 'f, 'field_var, 'run_state) t =
        ('var, 'value, 'f, 'field_var, (unit, 'run_state) Checked.t) typ
    end

    module Provider = struct
      include Types.Provider.T

      type ('a, 'f, 'field_var) t =
        ( ('a Request.t, 'f, 'field_var) As_prover.t
        , ('a, 'f, 'field_var) As_prover.t )
        provider
    end
  end

  type 'f field = Backend.Field.t

  type 'f field_var = Backend.Cvar.t

  include Types.Checked

  let eval : ('a, 'run_state) t -> 'run_state -> 'run_state * 'a = Simple.eval

  include Monad_let.Make2 (struct
    include Types.Checked

    let map = `Custom Simple.map

    let bind = Simple.bind

    let return = Simple.return
  end)

  open Constraint
  open Backend

  let get_value (t : run_state) : Cvar.t -> Field.t =
    let get_one i = Run_state.get_variable_value t i in
    Cvar.eval (`Return_values_will_be_mutated get_one)

  let run_as_prover x state =
    match (x, Run_state.has_witness state) with
    | Some x, true ->
        let old = Run_state.as_prover state in
        Run_state.set_as_prover state true ;
        let y = As_prover.run x (get_value state) in
        Run_state.set_as_prover state old ;
        (state, Some y)
    | _, _ ->
        (state, None)

  let as_prover x : _ Simple.t =
    Function
      (fun s ->
        let s', (_ : unit option) = run_as_prover (Some x) s in
        (s', ()) )

  let mk_lazy x : _ Simple.t =
    Function
      (fun s ->
        let old_stack = Run_state.stack s in
        ( s
        , Lazy.from_fun (fun () ->
              let stack = Run_state.stack s in

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
              let _s', y =
                Simple.eval (x ())
                  (Run_state.set_stack s (old_stack @ (label :: stack)))
              in
              y ) ) )

  let with_label lab t : _ Simple.t =
    Function
      (fun s ->
        let stack = Run_state.stack s in
        Option.iter (Run_state.log_constraint s) ~f:(fun f ->
            f ~at_label_boundary:(`Start, lab) None ) ;
        let s', y = Simple.eval (t ()) (Run_state.set_stack s (lab :: stack)) in
        Option.iter (Run_state.log_constraint s) ~f:(fun f ->
            f ~at_label_boundary:(`End, lab) None ) ;
        (Run_state.set_stack s' stack, y) )

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

  let add_constraint ~stack ({ basic; annotation } : Constraint.t) state =
    let label = Option.value annotation ~default:"<unknown>" in
    Run_state.add_constraint state basic
      ~label:(stack_to_string (label :: stack))

  let add_constraint c : _ Simple.t =
    Function
      (fun s ->
        if Run_state.as_prover s then
          (* Don't add constraints as the prover, or the constraint system won't match! *)
          (s, ())
        else (
          Option.iter (Run_state.log_constraint s) ~f:(fun f -> f (Some c)) ;
          if
            Run_state.eval_constraints s
            && not (Constraint.eval c (get_value s))
          then
            failwithf
              "Constraint unsatisfied (unreduced):\n\
               %s\n\
               %s\n\n\
               Constraint:\n\
               %s\n\
               Data:\n\
               %s"
              (Constraint.annotation c)
              (stack_to_string (Run_state.stack s))
              (Sexp.to_string (Constraint.sexp_of_t c))
              (log_constraint c s) () ;
          if not (Run_state.as_prover s) then
            add_constraint ~stack:(Run_state.stack s) c s ;
          (s, ()) ) )

  let with_handler h t : _ Simple.t =
    Function
      (fun s ->
        let handler = Run_state.handler s in
        let s', y =
          Simple.eval (t ())
            (Run_state.set_handler s (Request.Handler.push handler h))
        in
        (Run_state.set_handler s' handler, y) )

  let exists
      (Types.Typ.Typ
         { Types.Typ.var_of_fields
         ; value_to_fields
         ; size_in_field_elements
         ; check
         ; constraint_system_auxiliary
         ; _
         } :
        (_, _, _, _, _ Simple.t) Types.Typ.typ ) p : _ Simple.t =
    Function
      (fun s ->
        if Run_state.has_witness s then (
          let old = Run_state.as_prover s in
          Run_state.set_as_prover s true ;
          let value =
            As_prover.Provider.run p (Run_state.stack s) (get_value s)
              (Run_state.handler s)
          in
          Run_state.set_as_prover s old ;
          let var =
            let store_value =
              if Run_state.as_prover s then
                (* If we're nested in a prover block, create constants instead of
                   storing.
                *)
                Cvar.constant
              else Run_state.store_field_elt s
            in
            let fields, aux = value_to_fields value in
            let field_vars = Array.map ~f:store_value fields in
            var_of_fields (field_vars, aux)
          in
          (* TODO: Push a label onto the stack here *)
          let s, () = Simple.eval (check var) s in
          (s, { Handle.var; value = Some value }) )
        else
          let var =
            var_of_fields
              ( Array.init size_in_field_elements ~f:(fun _ ->
                    Run_state.alloc_var s )
              , constraint_system_auxiliary () )
          in
          (* TODO: Push a label onto the stack here *)
          let s, () = Simple.eval (check var) s in
          (s, { Handle.var; value = None }) )

  let next_auxiliary () : _ Simple.t =
    Function (fun s -> (s, Run_state.next_auxiliary s))

  let direct f : _ Simple.t = Function f

  let constraint_count ?(weight = Fn.const 1)
      ?(log = fun ?start:_ _lab _pos -> ()) (t : unit -> _ Simple.t) =
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
      Run_state.make ~num_inputs:0 ~input:(Field.Vector.create ())
        ~aux:(Field.Vector.create ()) ~system:false ~eval_constraints:false
        ~log_constraint ~with_witness:false ()
    in
    let _ = Simple.eval (t ()) state in
    !count
end

module type Run_extras = sig
  type field

  type cvar

  module Types : Types.Types

  type run_state

  type state = run_state

  val get_value : run_state -> cvar -> field

  val run_as_prover :
    ('a, field, cvar) As_prover0.t option -> run_state -> run_state * 'a option
end

module Make (Backend : Backend_extended.S) = struct
  open Backend

  type 'f field = 'f

  let constraint_logger = ref None

  let set_constraint_logger f = constraint_logger := Some f

  let clear_constraint_logger () = constraint_logger := None

  module Checked_runner = Make_checked (Backend) (As_prover0)

  type run_state = Checked_runner.run_state

  type ('a, 't) run = 't -> run_state -> run_state * 'a

  include (
    Checked_runner :
      sig
        include
          Checked_intf.Basic
            with module Types := Checked_runner.Types
            with type 'f field := 'f Checked_runner.field
             and type 'f field_var := 'f Checked_runner.field_var
             and type run_state := Backend.Run_state.t

        include
          Run_extras
            with module Types := Checked_runner.Types
            with type field := Backend.Field.t
             and type cvar := Backend.Cvar.t
             and type run_state := Backend.Run_state.t
      end )

  module Types = Checked_runner.Types

  let run = Simple.eval

  module State = struct
    let make :
           num_inputs:int
        -> input:Field.Vector.t
        -> aux:Field.Vector.t
        -> system:bool
        -> ?eval_constraints:bool
        -> ?handler:Request.Handler.t
        -> with_witness:bool
        -> ?log_constraint:
             (   ?at_label_boundary:[ `End | `Start ] * string
              -> Constraint.t option
              -> unit )
        -> unit
        -> run_state =
     fun ~num_inputs ~input ~aux ~system
         ?(eval_constraints = !eval_constraints_ref) ?handler ~with_witness
         ?log_constraint () ->
      let log_constraint =
        match log_constraint with
        | Some _ ->
            log_constraint
        | None ->
            !constraint_logger
      in
      (* We can't evaluate the constraints if we are not computing over a value. *)
      let eval_constraints = eval_constraints && with_witness in

      Run_state.make ~num_inputs ~input ~aux ~system ~eval_constraints
        ?log_constraint ?handler ~with_witness ()
  end
end

module type S = sig
  include Run_extras

  type constr

  type r1cs

  type field_vector

  val set_constraint_logger :
    (?at_label_boundary:[ `Start | `End ] * string -> constr -> unit) -> unit

  val clear_constraint_logger : unit -> unit

  type ('a, 't) run = 't -> run_state -> run_state * 'a

  val run : ('a, run_state) Types.Checked.t -> run_state -> run_state * 'a

  module State : sig
    val make :
         num_inputs:int
      -> input:field_vector
      -> aux:field_vector
      -> system:bool
      -> ?eval_constraints:bool
      -> ?handler:Request.Handler.t
      -> with_witness:bool
      -> ?log_constraint:
           (   ?at_label_boundary:[ `End | `Start ] * string
            -> (cvar, field) Constraint.t option
            -> unit )
      -> unit
      -> run_state
  end
end
