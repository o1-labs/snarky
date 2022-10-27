module Types0 = Types
module Cvar0 = Cvar
module Bignum_bigint = Bigint
module Checked_ast = Checked_ast
open Core_kernel

exception Runtime_error = Ast_runner.Runtime_error

module Runner = Checked_runner

let set_eval_constraints b = Runner.eval_constraints := b

module Make_runners
    (Backend : Backend_extended.S)
    (Checked : Checked_intf.Extended with type field = Backend.Field.t)
    (As_prover : As_prover_intf.Extended
                   with module Types := Checked.Types
                   with type field := Backend.Field.t)
    (Runner : Runner.S
                with module Types := Checked.Types
                with type field := Backend.Field.t
                 and type cvar := Backend.Cvar.t
                 and type constr := Backend.Constraint.t option
                 and type r1cs := Backend.R1CS_constraint_system.t) =
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
  module Var = Var
  module Field0 = Field
  module Cvar = Cvar
  module Constraint = Constraint

  module Handler = struct
    type t = Request.request -> Request.response
  end

  module Runner = Runner

  (* TODO-someday: Add pass to unify variables which have an Equal constraint *)
  let constraint_system ~run ~num_inputs ~return_typ:(Types.Typ.Typ return_typ)
      output t : R1CS_constraint_system.t =
    let input = field_vec () in
    let next_auxiliary = ref (1 + num_inputs) in
    let aux = field_vec () in
    let system = R1CS_constraint_system.create () in
    let state =
      Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux ~system
        ~with_witness:false ()
    in
    let state, res = run t state in
    let res, _ = return_typ.var_to_fields res in
    let output, _ = return_typ.var_to_fields output in
    let _state =
      Array.fold2_exn ~init:state res output ~f:(fun state res output ->
          fst @@ Checked.run (Checked.assert_equal res output) state )
    in
    let auxiliary_input_size = !next_auxiliary - (1 + num_inputs) in
    R1CS_constraint_system.set_auxiliary_input_size system auxiliary_input_size ;
    system

  let auxiliary_input ?system ~run ~num_inputs
      ?(handlers = ([] : Handler.t list)) t0 (input : Field.Vector.t)
      ~return_typ:(Types.Typ.Typ return_typ) ~output : Field.Vector.t =
    let next_auxiliary = ref (1 + num_inputs) in
    let aux = Field.Vector.create () in
    let handler =
      List.fold ~init:Request.Handler.fail handlers ~f:(fun handler h ->
          Request.Handler.(push handler (create_single h)) )
    in
    let state =
      Runner.State.make ?system ~num_inputs ~input:(pack_field_vec input)
        ~next_auxiliary ~aux:(pack_field_vec aux) ~handler ~with_witness:true ()
    in
    let state, res = run t0 state in
    let res, _ = return_typ.var_to_fields res in
    let output, _ = return_typ.var_to_fields output in
    let _state =
      Array.fold2_exn ~init:state res output ~f:(fun state res output ->
          Field.Vector.emplace_back input (Runner.get_value state res) ;
          fst @@ Checked.run (Checked.assert_equal res output) state )
    in
    Option.iter system ~f:(fun system ->
        let auxiliary_input_size = !next_auxiliary - (1 + num_inputs) in
        R1CS_constraint_system.set_auxiliary_input_size system
          auxiliary_input_size ;
        R1CS_constraint_system.finalize system ) ;
    aux

  let run_and_check' ~run t0 =
    let num_inputs = 0 in
    let input = field_vec () in
    let next_auxiliary = ref 1 in
    let aux = Field.Vector.create () in
    let system = R1CS_constraint_system.create () in
    let get_value : Cvar.t -> Field.t =
      let get_one v = Field.Vector.get aux (v - 1) in
      Cvar.eval (`Return_values_will_be_mutated get_one)
    in
    let state =
      Runner.State.make ~num_inputs ~input ~next_auxiliary
        ~aux:(pack_field_vec aux) ~system ~eval_constraints:true
        ~with_witness:true ()
    in
    match run t0 state with
    | exception e ->
        Or_error.of_exn ~backtrace:`Get e
    | _, x ->
        Ok (x, get_value)

  let run_and_check_deferred' ~map ~return ~run t0 =
    let num_inputs = 0 in
    let input = field_vec () in
    let next_auxiliary = ref 1 in
    let aux = Field.Vector.create () in
    let system = R1CS_constraint_system.create () in
    let get_value : Cvar.t -> Field.t =
      let get_one v = Field.Vector.get aux (v - 1) in
      Cvar.eval (`Return_values_will_be_mutated get_one)
    in
    let state =
      Runner.State.make ~num_inputs ~input ~next_auxiliary
        ~aux:(pack_field_vec aux) ~system ~eval_constraints:true
        ~with_witness:true ()
    in
    match run t0 state with
    | exception e ->
        return (Or_error.of_exn ~backtrace:`Get e)
    | res ->
        map res ~f:(function _, x -> Ok (x, get_value))

  let run_unchecked ~run t0 =
    let num_inputs = 0 in
    let input = field_vec () in
    let next_auxiliary = ref 1 in
    let aux = field_vec () in
    let state =
      Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux
        ~with_witness:true ()
    in
    match run t0 state with _, x -> x

  let run_and_check ~run t =
    Or_error.map (run_and_check' ~run t) ~f:(fun (x, get_value) ->
        let x = As_prover.run x get_value in
        x )

  let check ~run t = run_and_check' ~run t |> Result.map ~f:(Fn.const ())

  module Run = struct
    let alloc_var next_input () =
      let v = !next_input in
      incr next_input ; Cvar.Unsafe.of_index v

    let store_field_elt primary_input next_input x =
      let v = alloc_var next_input () in
      Field.Vector.emplace_back primary_input x ;
      v

    let collect_input_constraints :
        type checked input_var input_value.
           int ref
        -> input_typ:
             ( input_var
             , input_value
             , field
             , (unit, field) Checked.Types.Checked.t )
             Types.Typ.typ
        -> return_typ:_ Typ.t
        -> (unit -> input_var -> checked)
        -> _ * (unit -> checked) Checked.t =
     fun next_input ~input_typ:(Typ input_typ) ~return_typ:(Typ return_typ) k ->
      let open Checked in
      let alloc_input
          { Types0.Typ.var_of_fields
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
      let checked =
        let%bind () = input_typ.check var in
        Checked.return (fun () -> k () var)
      in
      (retval, checked)

    let r1cs_h :
        type a checked input_var input_value retval.
           run:(a, checked) Runner.run
        -> int ref
        -> input_typ:
             ( input_var
             , input_value
             , field
             , (unit, field) Checked.Types.Checked.t )
             Types.Typ.typ
        -> return_typ:(a, retval, _) Typ.t
        -> (input_var -> checked)
        -> R1CS_constraint_system.t =
     fun ~run next_input ~input_typ ~return_typ k ->
      let retval, r =
        collect_input_constraints next_input ~input_typ ~return_typ (fun () ->
            k )
      in
      let run_in_run r state =
        let state, x = Checked.run r state in
        run x state
      in
      constraint_system ~run:run_in_run ~num_inputs:(!next_input - 1)
        ~return_typ retval
        (Checked.map ~f:(fun r -> r ()) r)

    let constraint_system (type a checked input_var) :
           run:(a, checked) Runner.run
        -> input_typ:(input_var, _, _, _) Types.Typ.typ
        -> return_typ:_
        -> (input_var -> checked)
        -> R1CS_constraint_system.t =
     fun ~run ~input_typ ~return_typ k ->
      r1cs_h ~run (ref 1) ~input_typ ~return_typ k

    let generate_public_input :
           ('input_var, 'input_value, _, _) Types.Typ.typ
        -> 'input_value
        -> Field.Vector.t =
     fun (Typ { value_to_fields; _ }) value ->
      let primary_input = Field.Vector.create () in
      let next_input = ref 1 in
      let store_field_elt = store_field_elt primary_input next_input in
      let fields, _aux = value_to_fields value in
      let _fields = Array.map ~f:store_field_elt fields in
      primary_input

    let conv :
        type r_var r_value.
           (int -> _ -> r_var -> Field.Vector.t -> r_value)
        -> ('input_var, 'input_value, _) Typ.t
        -> _ Typ.t
        -> (unit -> 'input_var -> r_var)
        -> 'input_value
        -> r_value =
     fun cont0 input_typ (Typ return_typ) k0 ->
      let primary_input = Field.Vector.create () in
      let next_input = ref 1 in
      let store_field_elt x =
        let v = !next_input in
        incr next_input ;
        Field.Vector.emplace_back primary_input x ;
        Cvar.Unsafe.of_index v
      in
      let (Typ { var_of_fields; value_to_fields; _ }) = input_typ in
      fun value ->
        let fields, aux = value_to_fields value in
        let fields = Array.map ~f:store_field_elt fields in
        let var = var_of_fields (fields, aux) in
        let retval =
          return_typ.var_of_fields
            ( Core_kernel.Array.init return_typ.size_in_field_elements
                ~f:(fun _ -> alloc_var next_input ())
            , return_typ.constraint_system_auxiliary () )
        in
        cont0 !next_input retval (k0 () var) primary_input

    let generate_auxiliary_input :
           run:('a, 'checked) Runner.run
        -> input_typ:_ Typ.t
        -> return_typ:(_, _, _) Typ.t
        -> ?handlers:Handler.t list
        -> 'k_var
        -> 'k_value =
     fun ~run ~input_typ ~return_typ ?handlers k ->
      conv
        (fun num_inputs output c primary ->
          let auxiliary =
            auxiliary_input ~run ?handlers ~return_typ ~output ~num_inputs c
              primary
          in
          ignore auxiliary )
        input_typ return_typ
        (fun () -> k)

    let generate_witness_conv :
           run:('a, 'checked) Runner.run
        -> f:(Proof_inputs.t -> _ -> 'out)
        -> input_typ:_ Typ.t
        -> return_typ:_ Typ.t
        -> ?handlers:Handler.t list
        -> 'k_var
        -> 'k_value =
     fun ~run ~f ~input_typ ~return_typ ?handlers k ->
      conv
        (fun num_inputs output c primary ->
          let auxiliary =
            auxiliary_input ~run ?handlers ~return_typ ~output ~num_inputs c
              primary
          in
          let output =
            let (Typ return_typ) = return_typ in
            let fields, aux = return_typ.var_to_fields output in
            let read_cvar =
              let get_one i =
                if i <= num_inputs then Field.Vector.get primary (i - 1)
                else Field.Vector.get auxiliary (i - num_inputs - 1)
              in
              Cvar.eval (`Return_values_will_be_mutated get_one)
            in
            let fields = Array.map ~f:read_cvar fields in
            return_typ.value_of_fields (fields, aux)
          in
          f
            { Proof_inputs.public_inputs = primary
            ; auxiliary_inputs = auxiliary
            }
            output )
        input_typ return_typ
        (fun () -> k)

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

    let run_and_check = run_and_check

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

  let check t = check ~run:Checked.run t
end

module Make_basic
    (Backend : Backend_extended.S)
    (Checked : Checked_intf.Extended with type field = Backend.Field.t)
    (As_prover : As_prover_intf.Extended
                   with module Types := Checked.Types
                   with type field := Backend.Field.t)
    (Runner : Runner.S
                with module Types := Checked.Types
                with type field := Backend.Field.t
                 and type cvar := Backend.Cvar.t
                 and type constr := Backend.Constraint.t option
                 and type r1cs := Backend.R1CS_constraint_system.t) =
struct
  open Backend
  module Checked_S = Checked_intf.Unextend (Checked)
  include Make_runners (Backend) (Checked) (As_prover) (Runner)
  module Bigint = Bigint
  module Var = Var
  module Field0 = Field
  module Cvar = Cvar
  module Constraint = Constraint

  module Handler = struct
    type t = Request.request -> Request.response
  end

  module Typ = struct
    include Types.Typ.T
    module T = Typ.Make (Checked_S) (As_prover)
    include T.T

    type ('var, 'value) t = ('var, 'value, Field.t) T.t

    let unit : (unit, unit) t = unit ()

    let field : (Cvar.t, Field.t) t = field ()

    module type S =
      Typ.Intf.S
        with type field := Field.t
         and type field_var := Cvar.t
         and type _ checked = (unit, Field.t) Checked_S.t

    let mk_typ (type var value)
        (module M : S with type Var.t = var and type Value.t = value) =
      T.mk_typ
        ( module struct
          type field = Field.t

          include M
        end )
  end

  module As_prover = struct
    include As_prover

    type 'a as_prover = 'a t
  end

  module Handle = struct
    include Handle

    let value = As_prover.Handle.value
  end

  module Checked = struct
    include (
      Checked :
        Checked_intf.Extended
          with module Types := Checked.Types
          with type field := field )

    let perform req = request_witness Typ.unit req

    module Runner = Runner

    type run_state = Runner.run_state

    let assert_equal ?label x y =
      match (x, y) with
      | Cvar0.Constant x, Cvar0.Constant y ->
          if Field.equal x y then return ()
          else
            failwithf
              !"assert_equal: %{sexp: Field.t} != %{sexp: Field.t}"
              x y ()
      | _ ->
          assert_equal ?label x y

    (* [equal_constraints z z_inv r] asserts that
       if z = 0 then r = 1, or
       if z <> 0 then r = 0 and z * z_inv = 1
    *)
    let equal_constraints (z : Cvar.t) (z_inv : Cvar.t) (r : Cvar.t) =
      let open Constraint in
      let open Cvar in
      assert_all
        [ r1cs ~label:"equals_1" z_inv z (Cvar.constant Field.one - r)
        ; r1cs ~label:"equals_2" r z (Cvar.constant Field.zero)
        ]

    (* [equal_vars z] computes [(r, z_inv)] that satisfy the constraints in
       [equal_constraints z z_inv r].

       In particular, [r] is [1] if [z = 0] and [0] otherwise.
    *)
    let equal_vars (z : Cvar.t) : (Field.t * Field.t) As_prover.t =
      let open As_prover in
      let%map z = read_var z in
      if Field.equal z Field.zero then (Field.one, Field.zero)
      else (Field.zero, Field.inv z)

    let equal (x : Cvar.t) (y : Cvar.t) : Cvar.t Boolean.t t =
      match (x, y) with
      | Constant x, Constant y ->
          Checked.return
            (Boolean.Unsafe.create
               (Cvar.constant
                  (if Field.equal x y then Field.one else Field.zero) ) )
      | _ ->
          let z = Cvar.(x - y) in
          let%bind r, inv =
            exists Typ.(field * field) ~compute:(equal_vars z)
          in
          let%map () = equal_constraints z inv r in
          Boolean.Unsafe.create r

    let mul ?(label = "Checked.mul") (x : Cvar.t) (y : Cvar.t) =
      match (x, y) with
      | Constant x, Constant y ->
          return (Cvar.constant (Field.mul x y))
      | Constant x, _ ->
          return (Cvar.scale y x)
      | _, Constant y ->
          return (Cvar.scale x y)
      | _, _ ->
          with_label label (fun () ->
              let open Let_syntax in
              let%bind z =
                exists Typ.field
                  ~compute:
                    As_prover.(map2 (read_var x) (read_var y) ~f:Field.mul)
              in
              let%map () = assert_r1cs x y z in
              z )

    let square ?(label = "Checked.square") (x : Cvar.t) =
      match x with
      | Constant x ->
          return (Cvar.constant (Field.square x))
      | _ ->
          with_label label (fun () ->
              let open Let_syntax in
              let%bind z =
                exists Typ.field
                  ~compute:As_prover.(map (read_var x) ~f:Field.square)
              in
              let%map () = assert_square x z in
              z )

    (* We get a better stack trace by failing at the call to is_satisfied, so we
       put a bogus value for the inverse to make the constraint system unsat if
       x is zero. *)
    let inv ?(label = "Checked.inv") (x : Cvar.t) =
      match x with
      | Constant x ->
          return (Cvar.constant (Field.inv x))
      | _ ->
          with_label label (fun () ->
              let open Let_syntax in
              let%bind x_inv =
                exists Typ.field
                  ~compute:
                    As_prover.(
                      map (read_var x) ~f:(fun x ->
                          if Field.(equal zero x) then Field.zero
                          else Backend.Field.inv x ))
              in
              let%map () =
                assert_r1cs ~label:"field_inverse" x x_inv
                  (Cvar.constant Field.one)
              in
              x_inv )

    let div ?(label = "Checked.div") (x : Cvar.t) (y : Cvar.t) =
      match (x, y) with
      | Constant x, Constant y ->
          return (Cvar.constant (Field.( / ) x y))
      | _ ->
          with_label label (fun () ->
              let open Let_syntax in
              let%bind y_inv = inv y in
              mul x y_inv )

    let%snarkydef_ if_ (b : Cvar.t Boolean.t) ~(then_ : Cvar.t) ~(else_ : Cvar.t)
        =
      let open Let_syntax in
      (* r = e + b (t - e)
         r - e = b (t - e)
      *)
      let b = (b :> Cvar.t) in
      match b with
      | Constant b ->
          if Field.(equal b one) then return then_ else return else_
      | _ -> (
          match (then_, else_) with
          | Constant t, Constant e ->
              return Cvar.((t * b) + (e * (constant Field0.one - b)))
          | _, _ ->
              let%bind r =
                exists Typ.field
                  ~compute:
                    (let open As_prover in
                    let open Let_syntax in
                    let%bind b = read_var b in
                    read Typ.field
                      (if Field.equal b Field.one then then_ else else_))
              in
              let%map () =
                assert_r1cs b Cvar.(then_ - else_) Cvar.(r - else_)
              in
              r )

    let%snarkydef_ assert_non_zero (v : Cvar.t) =
      let open Let_syntax in
      let%map _ = inv v in
      ()

    module Boolean = struct
      open Boolean.Unsafe

      type var = Cvar.t Boolean.t

      type value = bool

      let true_ : var = create (Cvar.constant Field.one)

      let false_ : var = create (Cvar.constant Field.zero)

      let not (x : var) : var = create Cvar.((true_ :> Cvar.t) - (x :> Cvar.t))

      let if_ b ~(then_ : var) ~(else_ : var) =
        map ~f:create (if_ b ~then_:(then_ :> Cvar.t) ~else_:(else_ :> Cvar.t))

      (* This is unused for now as we are not using any square constraint system based
         backends. *)
      let _and_for_square_constraint_systems (x : var) (y : var) =
        (* (x + y)^2 = 2 z + x + y

           x^2 + 2 x*y + y^2 = 2 z + x + y
           x + 2 x*y + y = 2 z + x + y
           2 x*y = 2 z
           x * y = z
        *)
        let x = (x :> Cvar.t) in
        let y = (y :> Cvar.t) in
        let open Let_syntax in
        let%bind z =
          exists Typ.field
            ~compute:
              (let open As_prover in
              let open Let_syntax in
              let%map x = read_var x and y = read_var y in
              if Field.(equal one x) && Field.(equal one y) then Field.one
              else Field.zero)
        in
        let%map () =
          let x_plus_y = Cvar.add x y in
          assert_square x_plus_y Cvar.((Field.of_int 2 * z) + x_plus_y)
        in
        create z

      let ( && ) (x : var) (y : var) : var Checked.t =
        Checked.map ~f:create (mul (x :> Cvar.t) (y :> Cvar.t))

      let ( &&& ) = ( && )

      let ( || ) x y =
        let open Let_syntax in
        let%map both_false = (not x) && not y in
        not both_false

      let ( ||| ) = ( || )

      let any = function
        | [] ->
            return false_
        | [ b1 ] ->
            return b1
        | [ b1; b2 ] ->
            b1 || b2
        | bs ->
            let open Let_syntax in
            let%map all_zero =
              equal (Cvar.sum (bs :> Cvar.t list)) (Cvar.constant Field.zero)
            in
            not all_zero

      let all = function
        | [] ->
            return true_
        | [ b1 ] ->
            return b1
        | [ b1; b2 ] ->
            b1 && b2
        | bs ->
            equal
              (Cvar.constant (Field.of_int (List.length bs)))
              (Cvar.sum (bs :> Cvar.t list))

      let to_constant (b : var) =
        Option.map (Cvar.to_constant (b :> Cvar.t)) ~f:Field.(equal one)

      let var_of_value b = if b then true_ else false_

      let typ : (var, value) Typ.t =
        let (Typ typ) =
          Typ.field
          |> Typ.transport
               ~there:(function true -> Field.one | false -> Field.zero)
               ~back:(fun x -> if Field.equal x Field.zero then false else true)
          |> Typ.transport_var
               ~there:(fun (b : var) -> (b :> Cvar.t))
               ~back:create
        in
        Typ
          { typ with
            check =
              (fun v ->
                Checked.assert_
                  (Constraint.boolean ~label:"boolean-alloc" (v :> Cvar.t)) )
          }

      let typ_unchecked : (var, value) Typ.t =
        let (Typ typ) = typ in
        Typ { typ with check = (fun _ -> Checked.return ()) }

      let%test_unit "all" =
        let gen =
          let open Quickcheck.Generator in
          let open Let_syntax in
          let%bind length = small_positive_int in
          list_with_length length bool
        in
        Quickcheck.test gen ~sexp_of:[%sexp_of: bool list] ~f:(fun x ->
            let r =
              run_and_check
                (Checked.map ~f:(As_prover.read typ)
                   (all (List.map ~f:var_of_value x)) )
              |> Or_error.ok_exn
            in
            [%test_eq: bool] r (List.for_all x ~f:Fn.id) )

      let ( lxor ) b1 b2 =
        match (to_constant b1, to_constant b2) with
        | Some b1, Some b2 ->
            return (var_of_value (Caml.not (Bool.equal b1 b2)))
        | Some true, None ->
            return (not b2)
        | None, Some true ->
            return (not b1)
        | Some false, None ->
            return b2
        | None, Some false ->
            return b1
        | None, None ->
            (* (1 - 2 a) (1 - 2 b) = 1 - 2 c
               1 - 2 (a + b) + 4 a b = 1 - 2 c
               - 2 (a + b) + 4 a b = - 2 c
               (a + b) - 2 a b = c
               2 a b = a + b - c
            *)
            let open Let_syntax in
            let%bind res =
              exists typ_unchecked
                ~compute:
                  As_prover.(
                    map2 ~f:Bool.( <> ) (read typ_unchecked b1)
                      (read typ_unchecked b2))
            in
            let%map () =
              let a = (b1 :> Cvar.t) in
              let b = (b2 :> Cvar.t) in
              let c = (res :> Cvar.t) in
              let open Cvar in
              assert_r1cs (a + a) b (a + b - c)
            in
            res

      module Array = struct
        let num_true (bs : var array) =
          Array.fold bs ~init:(Cvar.constant Field.zero) ~f:(fun x y ->
              Cvar.add x (y :> Cvar.t) )

        let any = function
          | [||] ->
              return false_
          | [| b1 |] ->
              return b1
          | [| b1; b2 |] ->
              b1 || b2
          | bs ->
              let open Let_syntax in
              let%map all_zero =
                equal (num_true bs) (Cvar.constant Field.zero)
              in
              not all_zero

        let all = function
          | [||] ->
              return true_
          | [| b1 |] ->
              return b1
          | [| b1; b2 |] ->
              b1 && b2
          | bs ->
              equal
                (Cvar.constant (Field.of_int (Array.length bs)))
                (num_true bs)

        module Assert = struct
          let any bs = assert_non_zero (num_true bs)

          let all bs =
            assert_equal (num_true bs)
              (Cvar.constant (Field.of_int (Array.length bs)))
        end
      end

      let equal (a : var) (b : var) = a lxor b >>| not

      let of_field x =
        let open Let_syntax in
        let%map () = assert_ (Constraint.boolean x) in
        create x

      module Unsafe = struct
        let of_cvar (t : Cvar.t) : var = create t
      end

      module Assert = struct
        let ( = ) (x : var) (y : var) = assert_equal (x :> Cvar.t) (y :> Cvar.t)

        let is_true (v : var) = v = true_

        let%snarkydef_ any (bs : var list) =
          assert_non_zero (Cvar.sum (bs :> Cvar.t list))

        let%snarkydef_ all (bs : var list) =
          assert_equal
            (Cvar.sum (bs :> Cvar.t list))
            (Cvar.constant (Field.of_int (List.length bs)))

        let%snarkydef_ exactly_one (bs : var list) =
          assert_equal (Cvar.sum (bs :> Cvar.t list)) (Cvar.constant Field.one)
      end

      module Expr = struct
        type t = Var of var | And of t list | Or of t list | Not of t

        let rec eval t =
          let open Let_syntax in
          match t with
          | Not t ->
              eval t >>| not
          | Var v ->
              return v
          | And ts ->
              Checked.all (List.map ~f:eval ts) >>= all
          | Or ts ->
              Checked.all (List.map ~f:eval ts) >>= any

        let assert_ t = eval t >>= Assert.is_true

        let ( ! ) v = Var v

        let ( && ) x y = And [ x; y ]

        let ( &&& ) = ( && )

        let ( || ) x y = Or [ x; y ]

        let ( ||| ) = ( || )

        let not t = Not t

        let any xs = Or xs

        let all xs = And xs
      end
    end

    module Control = struct end

    let two_to_the n =
      let rec go acc i =
        if i = 0 then acc else go (Field0.add acc acc) (i - 1)
      in
      go Field0.one n

    type _ Request.t += Choose_preimage : Field.t * int -> bool list Request.t

    let choose_preimage_unchecked v ~length =
      exists
        (Typ.list Boolean.typ ~length)
        ~request:
          As_prover.(map (read_var v) ~f:(fun x -> Choose_preimage (x, length)))
        ~compute:
          (let open As_prover.Let_syntax in
          let%map x = As_prover.read_var v in
          let x = Bigint.of_field x in
          List.init length ~f:(fun i -> Bigint.test_bit x i))

    let packing_sum (bits : Boolean.var list) =
      let ts, _ =
        List.fold_left bits ~init:([], Field.one) ~f:(fun (acc, c) v ->
            ((c, (v :> Cvar.t)) :: acc, Field.add c c) )
      in
      Cvar.linear_combination ts

    let choose_preimage (v : Cvar.t) ~length : Boolean.var list t =
      let open Let_syntax in
      let%bind bits = choose_preimage_unchecked v ~length in
      let lc = packing_sum bits in
      let%map () =
        assert_r1cs ~label:"Choose_preimage" lc (Cvar.constant Field.one) v
      in
      bits

    let choose_preimage_flagged (v : Cvar.t) ~length =
      let open Let_syntax in
      let%bind bits = choose_preimage_unchecked v ~length in
      let lc = packing_sum bits in
      let%map success = equal lc v in
      (bits, `Success success)

    module List =
      Monad_sequence.List
        (Checked)
        (struct
          type t = Boolean.var

          include Boolean
        end)

    module Array =
      Monad_sequence.Array
        (Checked)
        (struct
          type t = Boolean.var

          let any = Boolean.Array.any

          let all = Boolean.Array.all
        end)
  end

  module Cvar1 = struct
    include Cvar

    let project =
      let two = Field.of_int 2 in
      fun (vars : Checked.Boolean.var list) ->
        let rec go res = function
          | [] ->
              res
          | v :: vs ->
              go Cvar0.(Add (v, Scale (two, res))) vs
        in
        match List.rev (vars :> Cvar.t list) with
        | [] ->
            Cvar0.Constant Field.zero
        | v :: vs ->
            go v vs

    let pack vars =
      assert (List.length vars < Field.size_in_bits) ;
      project vars

    let unpack v ~length =
      assert (length < Field.size_in_bits) ;
      Checked.choose_preimage v ~length

    let unpack_flagged v ~length =
      assert (length < Field.size_in_bits) ;
      Checked.choose_preimage_flagged v ~length
  end

  module Field = struct
    include Field0

    let gen =
      Quickcheck.Generator.map
        Bignum_bigint.(gen_incl zero (size - one))
        ~f:(fun x -> Bigint.(to_field (of_bignum_bigint x)))

    let gen_incl lo hi =
      let lo_bigint = Bigint.(to_bignum_bigint @@ of_field lo) in
      let hi_bigint = Bigint.(to_bignum_bigint @@ of_field hi) in
      Quickcheck.Generator.map
        Bignum_bigint.(gen_incl lo_bigint hi_bigint)
        ~f:(fun x -> Bigint.(to_field (of_bignum_bigint x)))

    let gen_uniform =
      Quickcheck.Generator.map
        Bignum_bigint.(gen_uniform_incl zero (size - one))
        ~f:(fun x -> Bigint.(to_field (of_bignum_bigint x)))

    let gen_uniform_incl lo hi =
      let lo_bigint = Bigint.(to_bignum_bigint @@ of_field lo) in
      let hi_bigint = Bigint.(to_bignum_bigint @@ of_field hi) in
      Quickcheck.Generator.map
        Bignum_bigint.(gen_uniform_incl lo_bigint hi_bigint)
        ~f:(fun x -> Bigint.(to_field (of_bignum_bigint x)))

    let typ = Typ.field

    type var' = Var.t

    module Var = Cvar1

    let parity x = Bigint.(test_bit (of_field x) 0)

    module Checked = struct
      include Cvar1

      let equal = Checked.equal

      let mul x y = Checked.mul ~label:"Field.Checked.mul" x y

      let square x = Checked.square ~label:"Field.Checked.square" x

      let div x y = Checked.div ~label:"Field.Checked.div" x y

      let inv x = Checked.inv ~label:"Field.Checked.inv" x

      let sqrt (x : Cvar.t) : Cvar.t Checked.t =
        match x with
        | Constant x ->
            Checked.return (Cvar.constant (Field.sqrt x))
        | _ ->
            let open Checked in
            let open Let_syntax in
            let%bind y =
              exists ~compute:As_prover.(map (read_var x) ~f:Field.sqrt) typ
            in
            let%map () = assert_square y x in
            y

      let quadratic_nonresidue =
        lazy
          (let rec go i =
             let x = Field.of_int i in
             if not (Field.is_square x) then x else go Int.(i + 1)
           in
           go 2 )

      (* The trick here is the following.

         Let beta be a known non-square.

         x is not a square iff beta*x is a square

         So we guess the result [is_square] and y a sqrt of one of {x, beta*x} and assert

         y * y = is_square * x + (1 - is_square) * (beta * x)

         which, letting B = beta*x holds iff

         y * y
         = is_square * x + B - is_square * B
         = is_square * (x - B) + B
      *)
      let sqrt_check x =
        let open Checked in
        let open Let_syntax in
        let%bind is_square =
          exists
            ~compute:As_prover.(map (read_var x) ~f:Field.is_square)
            Boolean.typ
        in
        let%bind y =
          exists typ
            ~compute:
              As_prover.(
                Let_syntax.(
                  let%map is_square = read Boolean.typ is_square
                  and x = read_var x in
                  if is_square then Field.sqrt x
                  else Field.(sqrt (Lazy.force quadratic_nonresidue * x))))
        in
        let b = scale x (Lazy.force quadratic_nonresidue) in
        let%bind t = mul (is_square :> Var.t) (x - b) in
        let%map () = assert_square y (t + b) in
        (y, is_square)

      let is_square x =
        let open Checked.Let_syntax in
        let%map _, b = sqrt_check x in
        b

      let%test_unit "is_square" =
        let x = Field.random () in
        let typf = Typ.field in
        let x2 = Field.square x in
        assert (Field.(equal (x * x) x2)) ;
        let run elt =
          let answer =
            run_and_check
              (Checked.map
                 ~f:(As_prover.read Checked.Boolean.typ)
                 Checked.(
                   Let_syntax.(
                     let%bind x = exists typf ~compute:(As_prover.return elt) in
                     is_square x)) )
            |> Or_error.ok_exn
          in
          answer
        in
        assert (run x2) ;
        assert (not (run (Field.mul (Lazy.force quadratic_nonresidue) x2)))

      let choose_preimage_var = Checked.choose_preimage

      type comparison_result =
        { less : Checked.Boolean.var; less_or_equal : Checked.Boolean.var }

      let if_ = Checked.if_

      let compare ~bit_length a b =
        (* Overview of the logic:
           let n = bit_length
           We have 0 <= a < 2^n, 0 <= b < 2^n, and so
             -2^n < b - a < 2^n
           If (b - a) >= 0, then
             2^n <= 2^n + b - a < 2^{n+1},
           and so the n-th bit must be set.
           If (b - a) < 0 then
             0 < 2^n + b - a < 2^n
           and so the n-th bit must not be set.
           Thus, we can use the n-th bit of 2^n + b - a to determine whether
             (b - a) >= 0 <-> a <= b.

           We also need that the maximum value
             2^n + (2^n - 1) - 0 = 2^{n+1} - 1
           fits inside the field, so for the max field element f,
             2^{n+1} - 1 <= f -> n+1 <= log2(f) = size_in_bits - 1
        *)
        assert (Int.(bit_length <= size_in_bits - 2)) ;
        let open Checked in
        let open Let_syntax in
        [%with_label_ "compare"] (fun () ->
            let alpha_packed =
              Cvar.(constant (two_to_the bit_length) + b - a)
            in
            let%bind alpha = unpack alpha_packed ~length:Int.(bit_length + 1) in
            let prefix, less_or_equal =
              match Core_kernel.List.split_n alpha bit_length with
              | p, [ l ] ->
                  (p, l)
              | _ ->
                  failwith "compare: Invalid alpha"
            in
            let%bind not_all_zeros = Boolean.any prefix in
            let%map less = Boolean.(less_or_equal && not_all_zeros) in
            { less; less_or_equal } )

      module Assert = struct
        let lt ~bit_length x y =
          let open Checked in
          let open Let_syntax in
          let%bind { less; _ } = compare ~bit_length x y in
          Boolean.Assert.is_true less

        let lte ~bit_length x y =
          let open Checked in
          let open Let_syntax in
          let%bind { less_or_equal; _ } = compare ~bit_length x y in
          Boolean.Assert.is_true less_or_equal

        let gt ~bit_length x y = lt ~bit_length y x

        let gte ~bit_length x y = lte ~bit_length y x

        let non_zero = Checked.assert_non_zero

        let equal x y = Checked.assert_equal ~label:"Checked.Assert.equal" x y

        let not_equal (x : t) (y : t) =
          Checked.with_label "Checked.Assert.not_equal" (fun () ->
              non_zero (sub x y) )
      end

      let lt_bitstring_value =
        let module Boolean = Checked.Boolean in
        let module Expr = struct
          module Binary = struct
            type 'a t = Lit of 'a | And of 'a * 'a t | Or of 'a * 'a t
          end

          module Nary = struct
            type 'a t = Lit of 'a | And of 'a t list | Or of 'a t list

            let rec of_binary : 'a Binary.t -> 'a t = function
              | Lit x ->
                  Lit x
              | And (x, And (y, t)) ->
                  And [ Lit x; Lit y; of_binary t ]
              | Or (x, Or (y, t)) ->
                  Or [ Lit x; Lit y; of_binary t ]
              | And (x, t) ->
                  And [ Lit x; of_binary t ]
              | Or (x, t) ->
                  Or [ Lit x; of_binary t ]

            let rec eval =
              let open Checked.Let_syntax in
              function
              | Lit x ->
                  return x
              | And xs ->
                  Checked.List.map xs ~f:eval >>= Boolean.all
              | Or xs ->
                  Checked.List.map xs ~f:eval >>= Boolean.any
          end
        end in
        let rec lt_binary xs ys : Boolean.var Expr.Binary.t =
          match (xs, ys) with
          | [], [] ->
              Lit Boolean.false_
          | [ _x ], [ false ] ->
              Lit Boolean.false_
          | [ x ], [ true ] ->
              Lit (Boolean.not x)
          | [ x1; _x2 ], [ true; false ] ->
              Lit (Boolean.not x1)
          | [ _x1; _x2 ], [ false; false ] ->
              Lit Boolean.false_
          | x :: xs, false :: ys ->
              And (Boolean.not x, lt_binary xs ys)
          | x :: xs, true :: ys ->
              Or (Boolean.not x, lt_binary xs ys)
          | _ :: _, [] | [], _ :: _ ->
              failwith "lt_bitstring_value: Got unequal length strings"
        in
        fun (xs : Boolean.var Bitstring_lib.Bitstring.Msb_first.t)
            (ys : bool Bitstring_lib.Bitstring.Msb_first.t) ->
          let open Expr.Nary in
          eval
            (of_binary (lt_binary (xs :> Boolean.var list) (ys :> bool list)))

      let field_size_bits =
        List.init Field.size_in_bits ~f:(fun i ->
            Z.testbit
              (Bignum_bigint.to_zarith_bigint Field.size)
              Stdlib.(Field.size_in_bits - 1 - i) )
        |> Bitstring_lib.Bitstring.Msb_first.of_list

      let unpack_full x =
        let module Bitstring = Bitstring_lib.Bitstring in
        let open Checked.Let_syntax in
        let%bind res =
          choose_preimage_var x ~length:Field.size_in_bits
          >>| Bitstring.Lsb_first.of_list
        in
        let%map () =
          lt_bitstring_value
            (Bitstring.Msb_first.of_lsb_first res)
            field_size_bits
          >>= Checked.Boolean.Assert.is_true
        in
        res

      let parity ?length x =
        let open Checked in
        let unpack =
          let unpack_full x =
            unpack_full x >>| Bitstring_lib.Bitstring.Lsb_first.to_list
          in
          match length with
          | None ->
              unpack_full
          | Some length ->
              let length = Int.min length Field.size_in_bits in
              if Int.equal length Field.size_in_bits then unpack_full
              else choose_preimage_var ~length
        in
        unpack x >>| Base.List.hd_exn
    end
  end

  module Bitstring_checked = struct
    type t = Checked.Boolean.var list

    let lt_value = Field.Checked.lt_bitstring_value

    let chunk_for_equality (t1 : t) (t2 : t) =
      let chunk_size = Field.size_in_bits - 1 in
      let rec go acc t1 t2 =
        match (t1, t2) with
        | [], [] ->
            acc
        | _, _ ->
            let t1_a, t1_b = List.split_n t1 chunk_size in
            let t2_a, t2_b = List.split_n t2 chunk_size in
            go ((t1_a, t2_a) :: acc) t1_b t2_b
      in
      go [] t1 t2

    let equal t1 t2 =
      let open Checked in
      all
        (Base.List.map (chunk_for_equality t1 t2) ~f:(fun (x1, x2) ->
             equal (Cvar1.pack x1) (Cvar1.pack x2) ) )
      >>= Boolean.all

    let equal_expect_true t1 t2 =
      let open Checked in
      all
        (Core_kernel.List.map (chunk_for_equality t1 t2) ~f:(fun (x1, x2) ->
             (* Inlined [Field.equal], but skip creating the field element for
                this chunk if possible.
             *)
             let z = Cvar1.(pack x1 - pack x2) in
             let%bind r, inv =
               exists
                 Typ.(field * field)
                 ~compute:
                   As_prover.(
                     match
                       Core_kernel.List.map2 x1 x2 ~f:(fun x1 x2 ->
                           let%map x1 = read_var (x1 :> Cvar.t)
                           and x2 = read_var (x2 :> Cvar.t) in
                           Field.equal x1 x2 )
                     with
                     | Ok res ->
                         let%bind res = all res in
                         if Core_kernel.List.for_all ~f:Fn.id res then
                           return (Field.one, Field.zero)
                         else equal_vars z
                     | _ ->
                         equal_vars z)
             in
             let%map () = equal_constraints z inv r in
             Boolean.Unsafe.of_cvar r ) )
      >>= Boolean.all

    module Assert = struct
      let equal t1 t2 =
        let open Checked in
        Base.List.map (chunk_for_equality t1 t2) ~f:(fun (x1, x2) ->
            Constraint.equal (Cvar1.pack x1) (Cvar1.pack x2) )
        |> assert_all ~label:"Bitstring.Assert.equal"
    end
  end

  let%test_unit "lt_bitstring_value" =
    let gen =
      let open Quickcheck.Generator in
      let open Let_syntax in
      let%bind length = small_positive_int in
      let%map x = list_with_length length bool
      and y = list_with_length length bool in
      (x, y)
    in
    Quickcheck.test gen ~f:(fun (x, y) ->
        let correct_answer = [%compare: bool list] x y < 0 in
        let lt =
          run_and_check
            (Checked.map
               ~f:(As_prover.read Checked.Boolean.typ)
               (Field.Checked.lt_bitstring_value
                  (Bitstring_lib.Bitstring.Msb_first.of_list
                     (List.map ~f:Checked.Boolean.var_of_value x) )
                  (Bitstring_lib.Bitstring.Msb_first.of_list y) ) )
          |> Or_error.ok_exn
        in
        assert (Bool.equal lt correct_answer) )

  include Checked

  let%snarkydef_ if_ (b : Boolean.var) ~typ:(Typ typ : ('var, _) Typ.t)
      ~(then_ : 'var) ~(else_ : 'var) =
    let then_, then_aux = typ.var_to_fields then_ in
    let else_, else_aux = typ.var_to_fields else_ in
    let%bind res =
      Array.all
        (Core_kernel.Array.map2_exn then_ else_ ~f:(fun then_ else_ ->
             if_ b ~then_ ~else_ ) )
    in
    let%map res_aux =
      (* Abstraction leak.. *)
      let res_aux = ref None in
      let%map () =
        as_prover
          As_prover.(
            if%map read Boolean.typ b then res_aux := Some then_aux
            else res_aux := Some else_aux)
      in
      match !res_aux with
      | Some res_aux ->
          res_aux
      | None ->
          typ.constraint_system_auxiliary ()
    in
    typ.var_of_fields (res, res_aux)

  let make_checked_ast x = x

  let run_checked_ast x = x

  module Test = struct
    let checked_to_unchecked typ1 typ2 checked input =
      let checked_result =
        run_and_check
          (let open Let_syntax in
          let%bind input = exists typ1 ~compute:(As_prover.return input) in
          let%map result = checked input in
          As_prover.read typ2 result)
        |> Or_error.ok_exn
      in
      checked_result

    let test_equal (type a) ?(sexp_of_t = sexp_of_opaque) ?(equal = Caml.( = ))
        typ1 typ2 checked unchecked input =
      let checked_result = checked_to_unchecked typ1 typ2 checked input in
      let sexp_of_a = sexp_of_t in
      let compare_a x y = if equal x y then 0 else 1 in
      [%test_eq: a] checked_result (unchecked input)
  end

  module R1CS_constraint_system = struct
    include R1CS_constraint_system
  end
end

module Make (Backend : Backend_intf.S) = struct
  module Backend_extended = Backend_extended.Make (Backend)
  module Runner0 = Runner.Make (Backend_extended)

  module As_prover0 =
    As_prover.Make_extended
      (struct
        type field = Backend_extended.Field.t
      end)
      (Checked_ast)
      (As_prover.Make (Checked_ast) (As_prover0))

  module Checked_for_basic = struct
    include (
      Checked_ast :
        Checked_intf.S
          with module Types = Checked_ast.Types
          with type ('a, 'f) t := ('a, 'f) Checked_ast.t
           and type 'f field := 'f )

    type field = Backend_extended.Field.t

    type 'a t = ('a, field) Types.Checked.t

    let run = Runner0.run
  end

  module Basic =
    Make_basic (Backend_extended) (Checked_for_basic) (As_prover0) (Runner0)
  include Basic
  module Number = Number.Make (Basic)
  module Enumerable = Enumerable.Make (Basic)
end

module Typ0 = Typ

module Run = struct
  let functor_counter = ref 0

  let active_counters = ref []

  let is_active_functor_id num =
    match !active_counters with
    | [] ->
        (* Show the usual error, the functor isn't wrong as far as we can tell.
        *)
        true
    | active :: _ ->
        Int.equal active num

  let active_functor_id () = List.hd_exn !active_counters

  module Make_basic (Backend : Backend_intf.S) = struct
    module Snark = Make (Backend)
    open Run_state
    open Snark

    let set_constraint_logger = set_constraint_logger

    let clear_constraint_logger = clear_constraint_logger

    let this_functor_id = incr functor_counter ; !functor_counter

    let state =
      ref
        (Run_state.make ~input:(field_vec ()) ~aux:(field_vec ())
           ~eval_constraints:false ~num_inputs:0 ~next_auxiliary:(ref 1)
           ~with_witness:false ~stack:[] ~is_running:false () )

    let in_prover () : bool = Run_state.has_witness !state

    let in_checked_computation () : bool =
      is_active_functor_id this_functor_id && Run_state.is_running !state

    let run (checked : _ Checked.t) =
      match checked with
      | Pure x ->
          x
      | _ ->
          if not (is_active_functor_id this_functor_id) then
            failwithf
              "Could not run this function.\n\n\
               Hint: The module used to create this function had internal ID \
               %i, but the module used to run it had internal ID %i. The same \
               instance of Snarky.Snark.Run.Make must be used for both."
              this_functor_id (active_functor_id ()) ()
          else if not (Run_state.is_running !state) then
            failwith
              "This function can't be run outside of a checked computation." ;
          let state', x = Runner.run checked !state in
          state := state' ;
          x

    let as_stateful x state' =
      state := state' ;
      let a = x () in
      (!state, a)

    let make_checked x = Checked_ast.Direct (as_stateful x, fun x -> Pure x)

    let make_checked_ast = make_checked

    module R1CS_constraint_system = Snark.R1CS_constraint_system
    module Var = Snark.Var

    type field = Snark.field

    module Bigint = Snark.Bigint
    module Constraint = Snark.Constraint

    module Typ = struct
      open Snark.Typ

      type nonrec ('var, 'value) t = ('var, 'value) t

      let unit = unit

      let field = field

      let tuple2 = tuple2

      let ( * ) = ( * )

      let tuple3 = tuple3

      let list = list

      let array = array

      let hlist = hlist

      let transport = transport

      let transport_var = transport_var

      let of_hlistable = of_hlistable

      module Internal = Internal

      module type S =
        Typ0.Intf.S
          with type field := Field.t
           and type field_var := Cvar.t
           and type _ checked = unit

      let mk_typ (type var value)
          (module M : S with type Var.t = var and type Value.t = value) =
        mk_typ
          ( module struct
            type _ checked = unit Checked.t

            module Var = struct
              include M.Var

              let check x =
                Checked_ast.Direct
                  ( (fun state' ->
                      (* We may already be inside a different checked
                         computation, e.g. a proof inside a proof!
                         Stash the state of the outer proof while we run our
                         computation, then restore it once we're done.
                      *)
                      let old_state = !state in
                      state := state' ;
                      let res = check x in
                      let state' = !state in
                      state := old_state ;
                      (state', res) )
                  , fun x -> Pure x )
            end

            module Value = M.Value
          end )
    end

    module Boolean = struct
      open Snark.Boolean

      type nonrec var = var

      type value = bool

      let true_ = true_

      let false_ = false_

      let if_ b ~then_ ~else_ = run (if_ b ~then_ ~else_)

      let not = not

      let ( && ) x y = run (x && y)

      let ( &&& ) = ( && )

      let ( || ) x y = run (x || y)

      let ( ||| ) = ( || )

      let ( lxor ) x y = run (x lxor y)

      let any l = run (any l)

      let all l = run (all l)

      let of_field x = run (of_field x)

      let var_of_value = var_of_value

      let typ = typ

      let typ_unchecked = typ_unchecked

      let equal x y = run (equal x y)

      module Expr = struct
        open Snark.Boolean.Expr

        type nonrec t = t

        let ( ! ) = ( ! )

        let ( && ) = ( && )

        let ( &&& ) = ( && )

        let ( || ) = ( || )

        let ( ||| ) = ( ||| )

        let any = any

        let all = all

        let not = not

        let eval x = run (eval x)

        let assert_ x = run (assert_ x)
      end

      module Unsafe = Unsafe

      module Assert = struct
        open Snark.Boolean.Assert

        let ( = ) x y = run (x = y)

        let is_true x = run (is_true x)

        let any l = run (any l)

        let all l = run (all l)

        let exactly_one l = run (exactly_one l)
      end

      module Array = struct
        open Snark.Boolean.Array

        let any x = run (any x)

        let all x = run (all x)

        module Assert = struct
          let any x = run (Assert.any x)

          let all x = run (Assert.all x)
        end
      end
    end

    module Field = struct
      open Snark.Field

      let size_in_bits = size_in_bits

      let size = size

      module Constant = struct
        type t = Snark.Field.t [@@deriving bin_io, sexp, hash, compare, eq]

        let gen = gen

        let gen_uniform = gen_uniform

        module T = struct
          let bin_shape_t = bin_shape_t

          let bin_writer_t = bin_writer_t

          let bin_write_t = bin_write_t

          let bin_size_t = bin_size_t

          let bin_reader_t = bin_reader_t

          let __bin_read_t__ = __bin_read_t__

          let bin_read_t = bin_read_t

          let bin_t = bin_t

          let sexp_of_t = sexp_of_t

          let t_of_sexp = t_of_sexp

          let of_int = of_int

          let one = one

          let zero = zero

          let add = add

          let sub = sub

          let mul = mul

          let inv = inv

          let square = square

          let sqrt = sqrt

          let is_square = is_square

          let equal = equal

          let size_in_bits = size_in_bits

          let print = print

          let to_string = to_string

          let random = random

          module Mutable = Mutable

          let ( += ) = ( += )

          let ( -= ) = ( -= )

          let ( *= ) = ( *= )

          module Vector = Vector

          let negate = negate

          let ( + ) = ( + )

          let ( - ) = ( - )

          let ( * ) = ( * )

          let ( / ) = ( / )

          let of_string = of_string

          let to_string = to_string

          let unpack = unpack

          let project = project

          let parity = parity
        end

        include T
      end

      open Snark.Field.Var

      type nonrec t = t

      let length = length

      let var_indices = var_indices

      let to_constant_and_terms = to_constant_and_terms

      let constant = constant

      let to_constant = to_constant

      let linear_combination = linear_combination

      let sum = sum

      let add = add

      let negate = negate

      let sub = sub

      let scale = scale

      let project = project

      let pack = pack

      (* New definitions *)

      let of_int i = constant (Constant.of_int i)

      let one = constant Constant.one

      let zero = constant Constant.zero

      open Snark.Field.Checked

      let mul x y = run (mul x y)

      let square x = run (square x)

      let div x y = run (div x y)

      let inv x = run (inv x)

      let is_square x = run (is_square x)

      let sqrt x = run (sqrt x)

      let sqrt_check x = run (sqrt_check x)

      let equal x y = run (equal x y)

      let unpack x ~length = run (unpack x ~length)

      let unpack_flagged x ~length = run (unpack_flagged x ~length)

      let unpack_full x = run (unpack_full x)

      let parity ?length x = run (parity ?length x)

      let choose_preimage_var x ~length = run (choose_preimage_var x ~length)

      type nonrec comparison_result = comparison_result =
        { less : Boolean.var; less_or_equal : Boolean.var }

      let compare ~bit_length x y = run (compare ~bit_length x y)

      let if_ b ~then_ ~else_ = run (if_ b ~then_ ~else_)

      let ( + ) = add

      let ( - ) = sub

      let ( * ) = mul

      let ( / ) = div

      module Unsafe = Unsafe

      module Assert = struct
        open Snark.Field.Checked.Assert

        let lte ~bit_length x y = run (lte ~bit_length x y)

        let gte ~bit_length x y = run (gte ~bit_length x y)

        let lt ~bit_length x y = run (lt ~bit_length x y)

        let gt ~bit_length x y = run (gt ~bit_length x y)

        let not_equal x y = run (not_equal x y)

        let equal x y = run (equal x y)

        let non_zero x = run (non_zero x)
      end

      let typ = typ
    end

    module Proof_inputs = Proof_inputs

    module Bitstring_checked = struct
      open Snark.Bitstring_checked

      type nonrec t = t

      let equal x y = run (equal x y)

      let equal_expect_true x y = run (equal_expect_true x y)

      let lt_value x y = run (lt_value x y)

      module Assert = struct
        open Snark.Bitstring_checked.Assert

        let equal x y = run (equal x y)
      end
    end

    module As_prover = struct
      type 'a t = 'a

      type 'a as_prover = 'a t

      let eval_as_prover f =
        if Run_state.as_prover !state && Run_state.has_witness !state then
          let a = f (Runner.get_value !state) in
          a
        else failwith "Can't evaluate prover code outside an as_prover block"

      let in_prover_block () = Run_state.as_prover !state

      let read_var var = eval_as_prover (As_prover.read_var var)

      let read typ var = eval_as_prover (As_prover.read typ var)

      include Field.Constant.T

      module Ref = struct
        type 'a t = 'a As_prover.Ref.t

        let create f = run As_prover.(Ref.create (map (return ()) ~f))

        let get r = eval_as_prover (As_prover.Ref.get r)

        let set r x = eval_as_prover (As_prover.Ref.set r x)
      end

      let run_prover f _tbl =
        (* Allow for nesting of prover blocks, by caching the current value and
           restoring it once we're done.
        *)
        let old = Run_state.as_prover !state in
        Run_state.set_as_prover !state true ;
        let a = f () in
        Run_state.set_as_prover !state old ;
        a
    end

    module Handle = struct
      type ('var, 'value) t = ('var, 'value) Handle.t

      let value handle () = As_prover.eval_as_prover (Handle.value handle)

      let var = Handle.var
    end

    let mark_active ~f =
      let counters = !active_counters in
      active_counters := this_functor_id :: counters ;
      try
        let ret = f () in
        active_counters := counters ;
        ret
      with exn ->
        active_counters := counters ;
        raise exn

    let mark_active_deferred (type a ma) ~(map : ma -> f:(a -> a) -> ma) ~f =
      let counters = !active_counters in
      active_counters := this_functor_id :: counters ;
      try
        map (f ()) ~f:(fun (ret : a) ->
            active_counters := counters ;
            ret )
      with exn ->
        active_counters := counters ;
        raise exn

    let assert_ ?label c = run (assert_ ?label c)

    let assert_all ?label c = run (assert_all ?label c)

    let assert_r1cs ?label a b c = run (assert_r1cs ?label a b c)

    let assert_square ?label x y = run (assert_square ?label x y)

    let as_prover p = run (as_prover (As_prover.run_prover p))

    let next_auxiliary () = run (next_auxiliary ())

    let request_witness typ p =
      run (request_witness typ (As_prover.run_prover p))

    let perform p = run (perform (As_prover.run_prover p))

    let request ?such_that typ r =
      match such_that with
      | None ->
          request_witness typ (fun () -> r)
      | Some such_that ->
          let x = request_witness typ (fun () -> r) in
          such_that x ; x

    let exists ?request ?compute typ =
      let request = Option.map request ~f:As_prover.run_prover in
      let compute = Option.map compute ~f:As_prover.run_prover in
      run (exists ?request ?compute typ)

    let exists_handle ?request ?compute typ =
      let request = Option.map request ~f:As_prover.run_prover in
      let compute = Option.map compute ~f:As_prover.run_prover in
      run (exists_handle ?request ?compute typ)

    type nonrec response = response

    let unhandled = unhandled

    type request = Request.request =
      | With :
          { request : 'a Request.t
          ; respond : 'a Request.Response.t -> response
          }
          -> request

    module Handler = Handler

    let handle x h =
      let h = Request.Handler.create_single h in
      let handler = Run_state.handler !state in
      state := Run_state.set_handler !state (Request.Handler.push handler h) ;
      let a = x () in
      state := Run_state.set_handler !state handler ;
      a

    let handle_as_prover x h =
      let h = h () in
      handle x h

    let if_ b ~typ ~then_ ~else_ = run (if_ b ~typ ~then_ ~else_)

    let with_label lbl x =
      let stack = Run_state.stack !state in
      let log_constraint = Run_state.log_constraint !state in
      state := Run_state.set_stack !state (lbl :: stack) ;
      Option.iter log_constraint ~f:(fun f ->
          f ~at_label_boundary:(`Start, lbl) None ) ;
      let a = x () in
      Option.iter log_constraint ~f:(fun f ->
          f ~at_label_boundary:(`End, lbl) None ) ;
      state := Run_state.set_stack !state stack ;
      a

    let inject_wrapper :
        type r_var input_var.
        f:(r_var -> r_var) -> (input_var -> r_var) -> input_var -> r_var =
     fun ~f x a ->
      let inject_wrapper ~f x = f x in
      inject_wrapper ~f (x a)

    let constraint_system ~input_typ ~return_typ x =
      let x = inject_wrapper x ~f:(fun x () -> mark_active ~f:x) in
      Perform.constraint_system ~run:as_stateful ~input_typ ~return_typ x

    let generate_public_input = generate_public_input

    let generate_witness ~input_typ ~return_typ x =
      let x = inject_wrapper x ~f:(fun x () -> mark_active ~f:x) in
      Perform.generate_witness ~run:as_stateful ~input_typ ~return_typ x

    let generate_witness_conv ~f ~input_typ ~return_typ x =
      let x = inject_wrapper x ~f:(fun x () -> mark_active ~f:x) in
      Perform.generate_witness_conv ~run:as_stateful ~f ~input_typ ~return_typ x

    let run_unchecked x =
      Perform.run_unchecked ~run:as_stateful (fun () -> mark_active ~f:x)

    let run_and_check (type a) (x : unit -> (unit -> a) As_prover.t) =
      let res =
        Perform.run_and_check ~run:as_stateful (fun () ->
            mark_active ~f:(fun () ->
                let prover_block = x () in
                Run_state.set_as_prover !state true ;
                As_prover.run_prover prover_block ) )
      in
      Run_state.set_as_prover !state true ;
      res

    module Run_and_check_deferred (M : sig
      type _ t

      val return : 'a -> 'a t

      val map : 'a t -> f:('a -> 'b) -> 'b t
    end) =
    struct
      open M

      let run_and_check ~run t =
        map
          (run_and_check_deferred' ~run t ~map ~return)
          ~f:
            (Or_error.map ~f:(fun (x, get_value) ->
                 let x = Basic.As_prover.run x get_value in
                 x ) )

      let as_stateful x state' =
        state := state' ;
        map (x ()) ~f:(fun a -> (!state, a))

      let run_and_check (type a) (x : unit -> (unit -> a) As_prover.t M.t) =
        let mark_active = mark_active_deferred ~map in
        let res =
          run_and_check ~run:as_stateful (fun () ->
              mark_active ~f:(fun () ->
                  map (x ()) ~f:(fun prover_block ->
                      Run_state.set_as_prover !state true ;
                      As_prover.run_prover prover_block ) ) )
        in
        Run_state.set_as_prover !state true ;
        res
    end

    let check x = Perform.check ~run:as_stateful x

    let constraint_count ?(weight = Fn.const 1) ?log x =
      let count = ref 0 in
      let log_constraint ?at_label_boundary c =
        ( match at_label_boundary with
        | None ->
            ()
        | Some (pos, lab) ->
            Option.iter log ~f:(fun f ->
                let start =
                  Some (match pos with `Start -> true | _ -> false)
                in
                f ?start lab !count ) ) ;
        count := !count + Option.value_map ~default:0 ~f:weight c
      in
      (* TODO(mrmr1993): Enable label-level logging for the imperative API. *)
      let old = !state in
      state :=
        Runner.State.make ~num_inputs:0 ~input:Vector.null ~aux:Vector.null
          ~next_auxiliary:(ref 1) ~eval_constraints:false ~with_witness:false
          ~log_constraint () ;
      ignore (mark_active ~f:x) ;
      state := old ;
      !count

    module Internal_Basic = Snark

    let run_checked = run

    let run_checked_ast x = run_checked x
  end

  module Make (Backend : Backend_intf.S) = struct
    module Basic = Make_basic (Backend)
    include Basic
    module Number = Number.Run.Make (Basic)
    module Enumerable = Enumerable.Run.Make (Basic)
  end
end

type 'field m = (module Snark_intf.Run with type field = 'field)

let make (type field) (module Backend : Backend_intf.S with type Field.t = field)
    : field m =
  (module Run.Make (Backend))
