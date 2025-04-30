open Core_kernel

module Make
    (Backend : Backend_extended.S)
    (Types : Types.Types
               with type field = Backend.Field.t
                and type field_var = Backend.Cvar.t)
    (Checked : Checked_intf.Extended
                 with module Types := Types
                 with type run_state = Backend.Run_state.t
                  and type constraint_ = Backend.Constraint.t)
    (As_prover : As_prover_intf.S with module Types := Types)
    (Runner : Checked_runner.S
                with module Types := Types
                with type constr := Backend.Constraint.t option
                 and type r1cs := Backend.R1CS_constraint_system.t
                 and type run_state = Backend.Run_state.t) =
struct
  open Backend

  let set_constraint_logger = Runner.set_constraint_logger

  let clear_constraint_logger = Runner.clear_constraint_logger

  type field = Field.t

  let field_vec_id : Field.Vector.t Type_equal.Id.t =
    Type_equal.Id.create ~name:"field-vector" sexp_of_opaque

  let pack_field_vec v =
    Run_state.Vector.T ((module Field.Vector), field_vec_id, v)

  let field_vec () = pack_field_vec (Field.Vector.create ())

  module Proof_inputs = struct
    type t =
      { public_inputs : Field.Vector.t; auxiliary_inputs : Field.Vector.t }
  end

  module Bigint = Bigint
  module Field0 = Field
  module Cvar = Cvar
  module Constraint = Constraint

  module Handler = struct
    type t = Request.request -> Request.response
  end

  module Runner = Runner

  let run_and_check_exn' ~run t0 =
    let num_inputs = 0 in
    let input = field_vec () in
    let next_auxiliary = ref 0 in
    let aux = Field.Vector.create () in
    let system = R1CS_constraint_system.create () in
    let get_value : Cvar.t -> Field.t =
      let get_one v = Field.Vector.get aux v in
      Cvar.eval (`Return_values_will_be_mutated get_one)
    in
    let state =
      Runner.State.make ~num_inputs ~input ~next_auxiliary
        ~aux:(pack_field_vec aux) ~system ~eval_constraints:true
        ~with_witness:true ()
    in
    let id = Run_state.id state in
    let state, x = run t0 state in
    let final_id = Run_state.id state in
    if id <> final_id then
      failwith "Snarky's internal state has been clobbered." ;
    (x, get_value)

  let run_and_check' ~run t0 =
    match run_and_check_exn' ~run t0 with
    | exception e ->
        Or_error.of_exn ~backtrace:`Get e
    | res ->
        Ok res

  let run_and_check_deferred_exn' ~map ~run t0 =
    let num_inputs = 0 in
    let input = field_vec () in
    let next_auxiliary = ref 0 in
    let aux = Field.Vector.create () in
    let system = R1CS_constraint_system.create () in
    let get_value : Cvar.t -> Field.t =
      let get_one v = Field.Vector.get aux v in
      Cvar.eval (`Return_values_will_be_mutated get_one)
    in
    let state =
      Runner.State.make ~num_inputs ~input ~next_auxiliary
        ~aux:(pack_field_vec aux) ~system ~eval_constraints:true
        ~with_witness:true ()
    in
    let id = Run_state.id state in
    let res = run t0 state in
    map res ~f:(function state, x ->
        let final_id = Run_state.id state in
        if id <> final_id then
          failwith "Snarky's internal state has been clobbered." ;
        (x, get_value) )

  let run_and_check_deferred' ~map ~return ~run t0 =
    match
      run_and_check_deferred_exn'
        ~map:(fun x ~f -> map x ~f:(fun x -> Ok (f x)))
        ~run t0
    with
    | exception e ->
        return (Or_error.of_exn ~backtrace:`Get e)
    | res ->
        res

  let run_unchecked ~run t0 =
    let num_inputs = 0 in
    let input = field_vec () in
    let next_auxiliary = ref 0 in
    let aux = field_vec () in
    let state =
      Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux
        ~with_witness:true ()
    in
    let id = Run_state.id state in
    let state, x = run t0 state in
    let final_id = Run_state.id state in
    if id <> final_id then
      failwith "Snarky's internal state has been clobbered." ;
    x

  let run_and_check_exn ~run t =
    let x, get_value = run_and_check_exn' ~run t in
    let x = As_prover.run x get_value in
    x

  let run_and_check ~run t =
    Or_error.map (run_and_check' ~run t) ~f:(fun (x, get_value) ->
        let x = As_prover.run x get_value in
        x )

  let check_exn ~run t = run_and_check_exn' ~run t |> Fn.const ()

  let check ~run t = run_and_check' ~run t |> Result.map ~f:(Fn.const ())

  module Run = struct
    let alloc_var next_input () =
      let v = !next_input in
      incr next_input ; Cvar.Unsafe.of_index v

    let store_field_elt primary_input next_input x =
      let v = alloc_var next_input () in
      Field.Vector.emplace_back primary_input x ;
      v

    module Constraint_system_builder : sig
      type ('input_var, 'return_var, 'checked) t =
        { run_computation : 'a. ('input_var -> Run_state.t -> 'a) -> 'a
        ; finish_computation :
            Run_state.t * 'return_var -> R1CS_constraint_system.t
        }

      val build :
           input_typ:('input_var, 'input_value) Types.Typ.typ
        -> return_typ:('retvar, 'retval) Types.Typ.typ
        -> ('input_var, 'retvar, 'checked) t
    end = struct
      let allocate_public_inputs :
          type input_var input_value output_var output_value.
             int ref
          -> input_typ:(input_var, input_value) Types.Typ.typ
          -> return_typ:(output_var, output_value) Types.Typ.t
          -> input_var * output_var =
       fun next_input ~input_typ:(Typ input_typ) ~return_typ:(Typ return_typ) ->
        (* allocate variables for the public input and the public output *)
        let alloc_input
            { Types.Typ.var_of_fields
            ; size_in_field_elements
            ; constraint_system_auxiliary
            ; _
            } =
          var_of_fields
            ( Core_kernel.Array.init size_in_field_elements ~f:(fun _ ->
                  alloc_var next_input () )
            , constraint_system_auxiliary () )
        in
        let var = alloc_input input_typ in
        let retval = alloc_input return_typ in
        (var, retval)

      type ('input_var, 'return_var, 'checked) t =
        { run_computation : 'a. ('input_var -> Run_state.t -> 'a) -> 'a
        ; finish_computation :
            Run_state.t * 'return_var -> R1CS_constraint_system.t
        }

      let build :
          type checked input_var input_value retvar retval.
             input_typ:(input_var, input_value) Types.Typ.typ
          -> return_typ:(retvar, retval) Types.Typ.t
          -> (input_var, retvar, checked) t =
       fun ~input_typ ~return_typ ->
        let next_input = ref 0 in
        (* allocate variables for the public input and the public output *)
        let var, retvar =
          allocate_public_inputs next_input ~input_typ ~return_typ
        in
        let (Typ return_typ) = return_typ in
        let num_inputs = !next_input in
        let input = field_vec () in
        let next_auxiliary = ref num_inputs in
        let aux = field_vec () in
        let system = R1CS_constraint_system.create () in
        let state =
          Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux ~system
            ~with_witness:false ()
        in
        let id = Backend.Run_state.id state in
        let state, () =
          (* create constraints to validate the input (using the input [Typ]'s [check]) *)
          let checked =
            let (Typ input_typ) = input_typ in
            input_typ.check var
          in
          Checked.run checked state
        in
        let run_computation k = k var state in
        let finish_computation (state, res) =
          let final_id = Backend.Run_state.id state in
          if id <> final_id then
            failwith "Snarky's internal state has been clobbered." ;
          let res, _ = return_typ.var_to_fields res in
          let retvar, _ = return_typ.var_to_fields retvar in
          let _state =
            Array.fold2_exn ~init:state res retvar ~f:(fun state res retvar ->
                fst @@ Checked.run (Checked.assert_equal res retvar) state )
          in
          let auxiliary_input_size = !next_auxiliary - num_inputs in
          R1CS_constraint_system.set_auxiliary_input_size system
            auxiliary_input_size ;
          system
        in
        { run_computation; finish_computation }
    end

    let constraint_system (type a checked input_var) :
           run:(a, checked) Runner.run
        -> input_typ:(input_var, _) Types.Typ.typ
        -> return_typ:_
        -> (input_var -> checked)
        -> R1CS_constraint_system.t =
     fun ~run ~input_typ ~return_typ k ->
      let builder = Constraint_system_builder.build ~input_typ ~return_typ in
      let state, res =
        builder.run_computation (fun var state -> run (k var) state)
      in
      builder.finish_computation (state, res)

    let generate_public_input :
           ('input_var, 'input_value) Types.Typ.typ
        -> 'input_value
        -> Field.Vector.t =
     fun (Typ { value_to_fields; _ }) value ->
      let primary_input = Field.Vector.create () in
      let next_input = ref 0 in
      let store_field_elt = store_field_elt primary_input next_input in
      let fields, _aux = value_to_fields value in
      let _fields = Array.map ~f:store_field_elt fields in
      primary_input

    module Conv = struct
      type ('input_var, 'output_var) t =
        { input_var : 'input_var
        ; output_var : 'output_var
        ; first_auxiliary : int
        ; primary_input : Field.Vector.t
        }

      let receive_public_input :
             ('input_var, 'input_value) Types.Typ.t
          -> _ Types.Typ.t
          -> 'input_value
          -> _ =
       fun input_typ (Typ return_typ) value ->
        let primary_input = Field.Vector.create () in
        let next_input = ref 0 in
        let store_field_elt x =
          let v = !next_input in
          incr next_input ;
          Field.Vector.emplace_back primary_input x ;
          Cvar.Unsafe.of_index v
        in
        let (Typ { var_of_fields; value_to_fields; _ }) = input_typ in
        let fields, aux = value_to_fields value in
        let fields = Array.map ~f:store_field_elt fields in
        let input_var = var_of_fields (fields, aux) in
        let output_var =
          return_typ.var_of_fields
            ( Core_kernel.Array.init return_typ.size_in_field_elements
                ~f:(fun _ -> alloc_var next_input ())
            , return_typ.constraint_system_auxiliary () )
        in
        let first_auxiliary = !next_input in
        { input_var; output_var; first_auxiliary; primary_input }
    end

    module Witness_builder = struct
      type ('input_var, 'return_var, 'return_value, 'field, 'checked) t =
        { run_computation : 'a. ('input_var -> Run_state.t -> 'a) -> 'a
        ; finish_witness_generation :
            Run_state.t * 'return_var -> Proof_inputs.t * 'return_value
        }

      let auxiliary_input ?(handlers = ([] : Handler.t list)) ~input_typ
          ~return_typ value =
        let { Conv.input_var
            ; output_var = output
            ; first_auxiliary = num_inputs
            ; primary_input = input
            } =
          Conv.receive_public_input input_typ return_typ value
        in
        let next_auxiliary = ref num_inputs in
        let aux = Field.Vector.create () in
        let handler =
          List.fold ~init:Request.Handler.fail handlers ~f:(fun handler h ->
              Request.Handler.(push handler (create_single h)) )
        in
        let state =
          Runner.State.make ~num_inputs ~input:(pack_field_vec input)
            ~next_auxiliary ~aux:(pack_field_vec aux) ~handler
            ~with_witness:true ()
        in
        let id = Run_state.id state in
        let run_computation t0 = t0 input_var state in
        let finish_witness_generation (state, res) =
          let final_id = Run_state.id state in
          if id <> final_id then
            failwith "Snarky's internal state has been clobbered." ;
          let (Typ return_typ) = return_typ in
          let res_fields, auxiliary_output_data =
            return_typ.var_to_fields res
          in
          let output_fields, _ = return_typ.var_to_fields output in
          let state =
            Array.fold2_exn ~init:state res_fields output_fields
              ~f:(fun state res_field output_field ->
                Field.Vector.emplace_back input
                  (Runner.get_value state res_field) ;
                fst
                @@ Checked.run
                     (Checked.assert_equal res_field output_field)
                     state )
          in
          let true_output =
            (* NB: We use [output_fields] to avoid resolving [Cvar.t]s beyond a
               vector access.
            *)
            let fields = Array.map ~f:(Runner.get_value state) output_fields in
            return_typ.value_of_fields (fields, auxiliary_output_data)
          in
          ( { Proof_inputs.public_inputs = input; auxiliary_inputs = aux }
          , true_output )
        in
        { run_computation; finish_witness_generation }
    end

    let conv :
        type r_var r_value.
           (int -> _ -> r_var -> Field.Vector.t -> r_value)
        -> ('input_var, 'input_value) Types.Typ.t
        -> _ Types.Typ.t
        -> (unit -> 'input_var -> r_var)
        -> 'input_value
        -> r_value =
     fun cont0 input_typ return_typ k0 value ->
      let { Conv.input_var; output_var; first_auxiliary; primary_input } =
        Conv.receive_public_input input_typ return_typ value
      in
      cont0 first_auxiliary output_var (k0 () input_var) primary_input

    let generate_auxiliary_input :
           run:('a, 'checked) Runner.run
        -> input_typ:_ Types.Typ.t
        -> return_typ:(_, _) Types.Typ.t
        -> ?handlers:Handler.t list
        -> 'k_var
        -> 'k_value =
     fun ~run ~input_typ ~return_typ ?handlers k value ->
      (* NB: No need to finish witness generation, we'll discard the
         witness and public output anyway.
      *)
      let { Witness_builder.run_computation; finish_witness_generation = _ } =
        Witness_builder.auxiliary_input ?handlers ~input_typ ~return_typ value
      in
      let state, res =
        run_computation (fun input_var state -> run (k input_var) state)
      in
      ignore (state, res)

    let generate_witness_conv :
           run:('a, 'checked) Runner.run
        -> f:(Proof_inputs.t -> _ -> 'out)
        -> input_typ:_ Types.Typ.t
        -> return_typ:_ Types.Typ.t
        -> ?handlers:Handler.t list
        -> 'k_var
        -> 'k_value =
     fun ~run ~f ~input_typ ~return_typ ?handlers k value ->
      let builder =
        Witness_builder.auxiliary_input ?handlers ~input_typ ~return_typ value
      in
      let state, res =
        builder.run_computation (fun input_var state ->
            run (k input_var) state )
      in
      let witness, output = builder.finish_witness_generation (state, res) in
      f witness output

    let generate_witness =
      generate_witness_conv ~f:(fun inputs _output -> inputs)
  end

  module Perform = struct
    let generate_witness ~run t ~return_typ k =
      Run.generate_witness ~run t ~return_typ k

    let generate_witness_conv ~run ~f t ~return_typ k =
      Run.generate_witness_conv ~run ~f t ~return_typ k

    let constraint_system = Run.constraint_system

    let run_unchecked = run_unchecked

    let run_and_check_exn = run_and_check_exn

    let run_and_check = run_and_check

    let check_exn = check_exn

    let check = check
  end

  let conv f spec return_typ k =
    Run.conv (fun _ _ x _ -> f x) spec return_typ (fun () -> k)

  let generate_auxiliary_input ~input_typ ~return_typ k =
    Run.generate_auxiliary_input ~run:Checked.run ~input_typ ~return_typ k

  let generate_public_input = Run.generate_public_input

  let generate_witness ~input_typ ~return_typ k =
    Run.generate_witness ~run:Checked.run ~input_typ ~return_typ k

  let generate_witness_conv ~f ~input_typ ~return_typ k =
    Run.generate_witness_conv ~run:Checked.run ~f ~input_typ ~return_typ k

  let constraint_system ~input_typ ~return_typ k =
    Run.constraint_system ~run:Checked.run ~input_typ ~return_typ k

  let run_unchecked t = run_unchecked ~run:Checked.run t

  let run_and_check t = run_and_check ~run:Checked.run t

  let run_and_check_exn t = run_and_check_exn ~run:Checked.run t

  let check t = check ~run:Checked.run t

  let check_exn t = check_exn ~run:Checked.run t
end
