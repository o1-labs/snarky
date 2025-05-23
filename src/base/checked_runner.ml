open Core_kernel

let stack_to_string = String.concat ~sep:"\n"

let eval_constraints = ref true

let eval_constraints_ref = eval_constraints

module T (Backend : Backend_extended.S) = struct
  type 'a t =
    | Pure of 'a
    | Function of (Backend.Run_state.t -> Backend.Run_state.t * 'a)
end

module Simple_types (Backend : Backend_extended.S) = Types.Make_types (struct
  type field = Backend.Field.t

  type field_var = Backend.Cvar.t

  type 'a checked = 'a T(Backend).t

  type 'a as_prover = (field_var -> field) -> 'a
end)

module Make_checked
    (Backend : Backend_extended.S)
    (Types : Types.Types
               with type field = Backend.Field.t
                and type field_var = Backend.Cvar.t
                and type 'a Checked.t = 'a Simple_types(Backend).Checked.t
                and type 'a As_prover.t = 'a Simple_types(Backend).As_prover.t
                and type ('var, 'value, 'aux) Typ.typ' =
                 ('var, 'value, 'aux) Simple_types(Backend).Typ.typ'
                and type ('var, 'value) Typ.typ =
                 ('var, 'value) Simple_types(Backend).Typ.typ)
    (As_prover : As_prover_intf.S with module Types := Types) =
struct
  type run_state = Backend.Run_state.t

  type constraint_ = Backend.Constraint.t

  type field = Backend.Field.t

  type 'a t = 'a T(Backend).t =
    | Pure of 'a
    | Function of (Backend.Run_state.t -> Backend.Run_state.t * 'a)

  let eval (t : 'a t) : run_state -> run_state * 'a =
    match t with Pure a -> fun s -> (s, a) | Function g -> g

  include Snarky_monad_lib.Monad_let.Make (struct
    type nonrec 'a t = 'a t

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

  open Backend

  let get_value (t : Run_state.t) : Cvar.t -> Field.t =
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

  let as_prover x : _ t =
    Function
      (fun s ->
        let s', (_ : unit option) = run_as_prover (Some x) s in
        (s', ()) )

  let mk_lazy x : _ t =
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
                eval (x ())
                  (Run_state.set_stack s (old_stack @ (label :: stack)))
              in
              y ) ) )

  let with_label lab t : _ t =
    Function
      (fun s ->
        let stack = Run_state.stack s in
        Option.iter (Run_state.log_constraint s) ~f:(fun f ->
            f ~at_label_boundary:(`Start, lab) None ) ;
        let s', y = eval (t ()) (Run_state.set_stack s (lab :: stack)) in
        Option.iter (Run_state.log_constraint s) ~f:(fun f ->
            f ~at_label_boundary:(`End, lab) None ) ;
        (Run_state.set_stack s' stack, y) )

  let add_constraint (basic : Constraint.t)
      (Constraint_system.T ((module C), system) :
        (Field.t, Constraint.t) Constraint_system.t ) =
    C.add_constraint system basic

  let add_constraint c : _ t =
    Function
      (fun s ->
        if Run_state.as_prover s then
          (* Don't add constraints as the prover, or the constraint system won't match! *)
          (s, ())
        else (
          Option.iter (Run_state.log_constraint s) ~f:(fun f -> f (Some c)) ;
          if
            Run_state.eval_constraints s
            && !eval_constraints
            && not (Constraint.eval c (get_value s))
          then
            failwithf
              "Constraint unsatisfied (unreduced):\n\
               %s\n\n\
               Constraint:\n\
               %s\n\
               Data:\n\
               %s"
              (stack_to_string (Run_state.stack s))
              (Sexp.to_string (Constraint.sexp_of_t c))
              (Backend.Constraint.log_constraint c (get_value s))
              () ;
          if not (Run_state.as_prover s) then
            Option.iter (Run_state.system s) ~f:(fun system ->
                add_constraint c system ) ;
          (s, ()) ) )

  let with_handler h t : _ t =
    Function
      (fun s ->
        let handler = Run_state.handler s in
        let s', y =
          eval (t ()) (Run_state.set_handler s (Request.Handler.push handler h))
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
        (_, _) Types.Typ.typ ) p : _ t =
    Function
      (fun s ->
        if Run_state.has_witness s then (
          let old = Run_state.as_prover s in
          Run_state.set_as_prover s true ;
          let value =
            match
              As_prover.Provider.run p (get_value s) (Run_state.handler s)
            with
            | Some x ->
                x
            | None ->
                failwith
                  ( "Unhandled request: "
                  ^ Core_kernel.String.concat ~sep:"\n" (Run_state.stack s) )
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
          let s, () = eval (check var) s in
          (s, { Handle.var; value = Some value }) )
        else
          let var =
            var_of_fields
              ( Array.init size_in_field_elements ~f:(fun _ ->
                    Run_state.alloc_var s () )
              , constraint_system_auxiliary () )
          in
          (* TODO: Push a label onto the stack here *)
          let s, () = eval (check var) s in
          (s, { Handle.var; value = None }) )

  let next_auxiliary () : _ t =
    Function (fun s -> (s, Run_state.next_auxiliary s))

  let direct f : _ t = Function f

  let constraint_count ?(weight = Fn.const 1)
      ?(log = fun ?start:_ _lab _pos -> ()) (t : unit -> _ t) =
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
      Run_state.make ~num_inputs:0 ~input:Run_state.Vector.null
        ~next_auxiliary:(ref 1) ~aux:Run_state.Vector.null
        ~eval_constraints:false ~log_constraint ~with_witness:false ()
    in
    let _ = eval (t ()) state in
    !count
end

module type Run_extras = sig
  type run_state

  module Types : Types.Types

  val get_value : run_state -> Types.field_var -> Types.field

  val run_as_prover :
    'a Types.As_prover.t option -> run_state -> run_state * 'a option
end

module Make
    (Backend : Backend_extended.S)
    (Types : Types.Types
               with type field = Backend.Field.t
                and type field_var = Backend.Cvar.t
                and type 'a Checked.t = 'a Simple_types(Backend).Checked.t
                and type 'a As_prover.t = 'a Simple_types(Backend).As_prover.t
                and type ('var, 'value, 'aux) Typ.typ' =
                 ('var, 'value, 'aux) Simple_types(Backend).Typ.typ'
                and type ('var, 'value) Typ.typ =
                 ('var, 'value) Simple_types(Backend).Typ.typ
                and type ('request, 'compute) Provider.provider =
                 ('request, 'compute) Simple_types(Backend).Provider.provider)
    (As_prover : As_prover_intf.S with module Types := Types) =
struct
  open Backend

  type 'f field = 'f

  let constraint_logger = ref None

  let set_constraint_logger f = constraint_logger := Some f

  let clear_constraint_logger () = constraint_logger := None

  module Checked_runner = Make_checked (Backend) (Types) (As_prover)

  type run_state = Checked_runner.run_state

  type state = run_state

  type ('a, 't) run = 't -> run_state -> run_state * 'a

  include (
    Checked_runner :
      sig
        include
          Checked_intf.Basic
            with module Types := Types
            with type run_state := run_state

        include
          Run_extras with module Types := Types with type run_state := run_state
      end )

  let run = Checked_runner.eval

  let dummy_vector = Run_state.Vector.null

  let fake_state next_auxiliary stack =
    Run_state.make ~num_inputs:0 ~input:Run_state.Vector.null ~next_auxiliary
      ~aux:Run_state.Vector.null ~eval_constraints:false ~stack
      ~with_witness:false ()

  module State = struct
    let make ~num_inputs ~input ~next_auxiliary ~aux ?system
        ?(eval_constraints = !eval_constraints_ref) ?handler ~with_witness
        ?log_constraint () =
      let log_constraint =
        match log_constraint with
        | Some _ ->
            log_constraint
        | None ->
            !constraint_logger
      in
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
      Run_state.make ~num_inputs ~input ~next_auxiliary ~aux ?system
        ~eval_constraints ?log_constraint ?handler ~with_witness ()
  end
end

module type S = sig
  include Run_extras

  type constr

  type r1cs

  val set_constraint_logger :
    (?at_label_boundary:[ `Start | `End ] * string -> constr -> unit) -> unit

  val clear_constraint_logger : unit -> unit

  type state = run_state

  type ('a, 't) run = 't -> run_state -> run_state * 'a

  val run : 'a Types.Checked.t -> run_state -> run_state * 'a

  module State : sig
    val make :
         num_inputs:int
      -> input:Types.field Run_state_intf.Vector.t
      -> next_auxiliary:int ref
      -> aux:Types.field Run_state_intf.Vector.t
      -> ?system:r1cs
      -> ?eval_constraints:bool
      -> ?handler:Request.Handler.t
      -> with_witness:bool
      -> ?log_constraint:
           (?at_label_boundary:[ `End | `Start ] * string -> constr -> unit)
      -> unit
      -> run_state
  end
end
