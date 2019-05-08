module Bignum_bigint = Bigint
module Checked_ast = Checked
open Core_kernel

let () = Camlsnark_c.linkme

module Runner = Checked_runner

let set_eval_constraints b = Runner.eval_constraints := b

let reduce_to_prover = ref false

let set_reduce_to_prover b = reduce_to_prover := b

module Make_basic
    (Backend : Backend_extended.S)
    (Checked : Checked_intf.Extended with type field = Backend.Field.t)
    (As_prover : As_prover_intf.S
                 with module Types := Checked.Types
                 with type field := Backend.Field.t)
    (Runner : Runner.S
              with type field := Backend.Field.t
               and type cvar := Backend.Cvar.t
               and type constr := Backend.Constraint.t
               and type r1cs := Backend.R1CS_constraint_system.t) =
struct
  open Backend
  module Checked_S = Checked_intf.Unextend (Checked)

  let set_constraint_logger = Runner.set_constraint_logger

  let clear_constraint_logger = Runner.clear_constraint_logger

  type field = Field.t

  module Bigint = Bigint
  module Proof = Proof
  module Verification_key = Verification_key
  module Proving_key = Proving_key
  module Keypair = Keypair
  module Var = Var
  module Field0 = Field
  module Cvar = Cvar
  module Linear_combination = Linear_combination
  module Constraint = Constraint

  module Typ_monads = struct
    open Typ_monads

    module Store = struct
      include Restrict_monad.Make2 (Store) (Field)

      let store = Store.store

      let run = Store.run
    end

    module Read = struct
      include Restrict_monad.Make2 (Read) (Field)

      let read = Read.read

      let run = Read.run
    end

    module Alloc = struct
      open Alloc
      include Restrict_monad.Make2 (Alloc) (Field)

      let alloc = alloc

      let run = run

      let size t = size t
    end
  end

  module Handler = struct
    type t = Request.request -> Request.response
  end

  module Typ = struct
    include Types.Typ.T
    module T = Typ.Make (Checked_S)
    include Typ_monads
    include T.T

    type ('var, 'value) t = ('var, 'value, Field.t) T.t

    module Data_spec = struct
      include Typ.Data_spec0

      type ('r_var, 'r_value, 'k_var, 'k_value) t =
        ('r_var, 'r_value, 'k_var, 'k_value, field) T.Data_spec.t

      let size t = T.Data_spec.size t
    end

    let unit : (unit, unit) t = unit ()

    let field : (Cvar.t, Field.t) t = field ()

    (* TODO: Assert that a stored value has the same shape as the template. *)
    module Of_traversable (T : Traversable.S) = struct
      let typ ~template
          ({read; store; alloc; check} : ('elt_var, 'elt_value) t) :
          ('elt_var T.t, 'elt_value T.t) t =
        let traverse_store =
          let module M = T.Traverse (Store) in
          M.f
        in
        let traverse_read =
          let module M = T.Traverse (Read) in
          M.f
        in
        let traverse_alloc =
          let module M = T.Traverse (Alloc) in
          M.f
        in
        let traverse_checked =
          let module M =
            T.Traverse
              (Restrict_monad.Make2
                 (Checked)
                 (struct
                   type t = unit
                 end)) in
          M.f
        in
        let read var = traverse_read var ~f:read in
        let store value = traverse_store value ~f:store in
        let alloc = traverse_alloc template ~f:(fun () -> alloc) in
        let check t = Checked.map (traverse_checked t ~f:check) ~f:ignore in
        {read; store; alloc; check}
    end
  end

  module As_prover = struct
    include As_prover

    type ('a, 'prover_state) as_prover = ('a, 'prover_state) t
  end

  module Handle = struct
    include Handle

    let value = As_prover.Handle.value
  end

  module Checked = struct
    open Types.Checked
    open Run_state

    include (
      Checked :
        Checked_intf.Extended
        with module Types := Checked.Types
        with type field := field )

    let perform req = request_witness Typ.unit req

    module Runner = Runner

    type 'prover_state run_state = 'prover_state Runner.run_state

    let rec constraint_count_aux : type a s.
           log:(?start:_ -> _)
        -> auxc:_
        -> int
        -> (a, s, _) Types.Checked.t
        -> int * a =
     fun ~log ~auxc count t0 ->
      match t0 with
      | Pure x ->
          (count, x)
      | Direct (d, k) ->
          let input = Field.Vector.create () in
          let aux = Field.Vector.create () in
          let state =
            Runner.State.make ~num_inputs:0 ~input ~next_auxiliary:auxc ~aux
              None
          in
          let count = ref count in
          let log_constraint c = count := !count + List.length c in
          let state = {state with log_constraint= Some log_constraint} in
          let _, x = d state in
          constraint_count_aux ~log ~auxc !count (k x)
      | Reduced (t, _, _, k) ->
          let count, y = constraint_count_aux ~log ~auxc count t in
          constraint_count_aux ~log ~auxc count (k y)
      | As_prover (_x, k) ->
          constraint_count_aux ~log ~auxc count k
      | Add_constraint (c, t) ->
          constraint_count_aux ~log ~auxc (count + List.length c) t
      | Next_auxiliary k ->
          constraint_count_aux ~log ~auxc count (k !auxc)
      | With_label (s, t, k) ->
          log ~start:true s count ;
          let count', y = constraint_count_aux ~log ~auxc count t in
          log s count' ;
          constraint_count_aux ~log ~auxc count' (k y)
      | With_state (_p, _and_then, t_sub, k) ->
          let count', y = constraint_count_aux ~log ~auxc count t_sub in
          constraint_count_aux ~log ~auxc count' (k y)
      | With_handler (_h, t, k) ->
          let count, x = constraint_count_aux ~log ~auxc count t in
          constraint_count_aux ~log ~auxc count (k x)
      | Clear_handler (t, k) ->
          let count, x = constraint_count_aux ~log ~auxc count t in
          constraint_count_aux ~log ~auxc count (k x)
      | Exists ({alloc; check; _}, _c, k) ->
          let alloc_var () =
            let v = !auxc in
            incr auxc ; Cvar.Unsafe.of_index v
          in
          let var = Typ.Alloc.run alloc alloc_var in
          (* TODO: Push a label onto the stack here *)
          let count, () = constraint_count_aux ~log ~auxc count (check var) in
          constraint_count_aux ~log ~auxc count (k {Handle.var; value= None})

    let constraint_count ?(log = fun ?start _ _ -> ())
        (t : (_, _, _) Types.Checked.t) : int =
      let next_auxiliary = ref 1 in
      fst (constraint_count_aux ~log ~auxc:next_auxiliary 0 t)

    (* TODO-someday: Add pass to unify variables which have an Equal constraint *)
    let constraint_system ~run ~num_inputs t : R1CS_constraint_system.t =
      let input = Field.Vector.create () in
      let next_auxiliary = ref (1 + num_inputs) in
      let aux = Field.Vector.create () in
      let system = R1CS_constraint_system.create () in
      let state =
        Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux ~system None
      in
      ignore (run t state) ;
      let auxiliary_input_size = !next_auxiliary - (1 + num_inputs) in
      R1CS_constraint_system.set_auxiliary_input_size system
        auxiliary_input_size ;
      system

    let auxiliary_input ?system ~run ~num_inputs
        ?(handlers = ([] : Handler.t list)) t0 s0 (input : Field.Vector.t) :
        Field.Vector.t =
      let next_auxiliary = ref (1 + num_inputs) in
      let aux = Field.Vector.create () in
      let handler =
        List.fold ~init:Request.Handler.fail handlers ~f:(fun handler h ->
            Request.Handler.(push handler (create_single h)) )
      in
      let state =
        Runner.State.make ?system ~num_inputs ~input ~next_auxiliary ~aux
          ~handler (Some s0)
      in
      ignore (run t0 state) ;
      Option.iter system ~f:(fun system ->
          let auxiliary_input_size = !next_auxiliary - (1 + num_inputs) in
          R1CS_constraint_system.set_auxiliary_input_size system
            auxiliary_input_size ;
          R1CS_constraint_system.finalize system ) ;
      aux

    let run_and_check' ~run t0 s0 =
      let num_inputs = 0 in
      let input = Field.Vector.create () in
      let next_auxiliary = ref 1 in
      let aux = Field.Vector.create () in
      let system = R1CS_constraint_system.create () in
      let get_value : Cvar.t -> Field.t =
        let get_one v = Field.Vector.get aux (v - 1) in
        Cvar.eval get_one
      in
      let state =
        Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux ~system
          ~eval_constraints:true (Some s0)
      in
      match run t0 state with
      | exception e ->
          Or_error.of_exn e
      | {prover_state= Some s; _}, x ->
          Ok (s, x, get_value)
      | _ ->
          failwith "run_and_check': Expected a value from run, got None."

    let run_unchecked ~run t0 s0 =
      let num_inputs = 0 in
      let input = Field.Vector.create () in
      let next_auxiliary = ref 1 in
      let aux = Field.Vector.create () in
      let state =
        Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux (Some s0)
      in
      match run t0 state with
      | {prover_state= Some s; _}, x ->
          (s, x)
      | _ ->
          failwith "run_unchecked: Expected a value from run, got None."

    let run_and_check ~run t s =
      Or_error.map (run_and_check' ~run t s) ~f:(fun (s, x, get_value) ->
          let s', x = As_prover.run x get_value s in
          (s', x) )

    let check ~run t s = run_and_check' ~run t s |> Result.map ~f:(Fn.const ())

    let equal (x : Cvar.t) (y : Cvar.t) : (Cvar.t Boolean.t, _) t =
      let open Let_syntax in
      let%bind inv =
        exists Typ.field
          ~compute:
            (let open As_prover.Let_syntax in
            let%map x = As_prover.read_var x and y = As_prover.read_var y in
            if Field.equal x y then Field.zero else Field.inv (Field.sub x y))
      and r =
        exists Typ.field
          ~compute:
            (let open As_prover.Let_syntax in
            let%map x = As_prover.read_var x and y = As_prover.read_var y in
            if Field.equal x y then Field.one else Field.zero)
      in
      let%map () =
        let open Constraint in
        let open Cvar in
        assert_all
          [ r1cs ~label:"equals_1" inv (x - y) (Cvar.constant Field.one - r)
          ; r1cs ~label:"equals_2" r (x - y) (Cvar.constant Field.zero) ]
      in
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
          with_label label
            (let open Let_syntax in
            let%bind z =
              exists Typ.field
                ~compute:
                  As_prover.(map2 (read_var x) (read_var y) ~f:Field.mul)
            in
            let%map () = assert_r1cs x y z in
            z)

    let square ?(label = "Checked.square") (x : Cvar.t) =
      match x with
      | Constant x ->
          return (Cvar.constant (Field.square x))
      | _ ->
          with_label label
            (let open Let_syntax in
            let%bind z =
              exists Typ.field
                ~compute:As_prover.(map (read_var x) ~f:Field.square)
            in
            let%map () = assert_square x z in
            z)

    (* We get a better stack trace by failing at the call to is_satisfied, so we
     put a bogus value for the inverse to make the constraint system unsat if
     x is zero. *)
    let inv ?(label = "Checked.inv") (x : Cvar.t) =
      match x with
      | Constant x ->
          return (Cvar.constant (Field.inv x))
      | _ ->
          with_label label
            (let open Let_syntax in
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
            x_inv)

    let div ?(label = "Checked.div") x y =
      with_label label
        (let open Let_syntax in
        let%bind y_inv = inv y in
        mul x y_inv)

    let%snarkydef_ if_ (b : Cvar.t Boolean.t) ~(then_ : Cvar.t)
        ~(else_ : Cvar.t) =
      let open Let_syntax in
      (* r = e + b (t - e)
      r - e = b (t - e)
    *)
      let b = (b :> Cvar.t) in
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
          let%map () = assert_r1cs b Cvar.(then_ - else_) Cvar.(r - else_) in
          r

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

      let ( && ) (x : var) (y : var) =
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

      let ( || ) x y =
        let open Let_syntax in
        let%map both_false = (not x) && not y in
        not both_false

      let any = function
        | [] ->
            return false_
        | [b1] ->
            return b1
        | [b1; b2] ->
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
        | [b1] ->
            return b1
        | [b1; b2] ->
            b1 && b2
        | bs ->
            equal
              (Cvar.constant (Field.of_int (List.length bs)))
              (Cvar.sum (bs :> Cvar.t list))

      let to_constant (b : var) =
        Option.map (Cvar.to_constant (b :> Cvar.t)) ~f:Field.(equal one)

      let var_of_value b = if b then true_ else false_

      let typ : (var, value) Typ.t =
        let open Typ in
        let store b =
          Store.(map (store (if b then Field.one else Field.zero)) ~f:create)
        in
        let read (v : var) =
          let open Read.Let_syntax in
          let%map x = Read.read (v :> Cvar.t) in
          if Field.equal x Field.one then true
          else if Field.equal x Field.zero then false
          else failwith "Boolean.typ: Got non boolean value for variable"
        in
        let alloc = Alloc.(map alloc ~f:create) in
        let check (v : var) =
          Checked.assert_
            (Constraint.boolean ~label:"boolean-alloc" (v :> Cvar.t))
        in
        {read; store; alloc; check}

      let typ_unchecked : (var, value) Typ.t =
        {typ with check= (fun _ -> Checked.return ())}

      let ( lxor ) b1 b2 =
        match (to_constant b1, to_constant b2) with
        | Some b1, Some b2 ->
            return (var_of_value (b1 <> b2))
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
                    map2 ~f:( <> ) (read typ_unchecked b1)
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

        let ( && ) x y = And [x; y]

        let ( || ) x y = Or [x; y]

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

    type _ Request.t +=
      | Choose_preimage : Field.t * int -> bool list Request.t

    let choose_preimage_unchecked v ~length =
      exists
        (Typ.list Boolean.typ ~length)
        ~request:
          As_prover.(
            map (read_var v) ~f:(fun x -> Choose_preimage (x, length)))
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

    let choose_preimage (v : Cvar.t) ~length : (Boolean.var list, 's) t =
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
      Monad_sequence.List (struct
          type nonrec ('a, 's) t = ('a, 's) t

          include (
            Checked_S :
              Checked_intf.S
              with module Types := Checked_S.Types
              with type ('a, 's, 'f) t :=
                          ('a, 's, 'f) Checked_S.Types.Checked.t )
        end)
        (struct
          type t = Boolean.var

          include Boolean
        end)
  end

  module Data_spec = Typ.Data_spec

  module Run = struct
    open Run_state
    open Data_spec

    let alloc_var next_input () =
      let v = !next_input in
      incr next_input ; Cvar.Unsafe.of_index v

    let store_field_elt primary_input next_input x =
      let v = alloc_var next_input () in
      Field.Vector.emplace_back primary_input x ;
      v

    module Proof_system = struct
      type ('checked, 'inputs, 's) proof_system =
        { compute: 'checked
        ; reduced_compute: 'checked Lazy.t
        ; check_inputs: (unit, 's) Checked.t
        ; provide_inputs:
            Field.Vector.t -> (unit, 'inputs) H_list.t -> Field.Vector.t
        ; num_inputs: int
        ; handler: Request.Handler.t
        ; mutable proving_key: Proving_key.t option
        ; mutable verification_key: Verification_key.t option
        ; proving_key_path: string option
        ; verification_key_path: string option }

      let rec allocate_inputs : type checked k1 k2.
             reduce_to_prover:(int ref -> checked -> checked)
          -> (unit, 's) Checked.t
          -> int ref
          -> (checked, unit, k1, k2) t
          -> k1
          -> (checked, k2, 's) proof_system =
       fun ~reduce_to_prover check_inputs next_input t compute ->
        let open Checked in
        match t with
        | [] ->
            { compute
            ; reduced_compute=
                lazy (reduce_to_prover (ref !next_input) compute)
            ; check_inputs= Checked.return ()
            ; provide_inputs= (fun input ([] : (unit, unit) H_list.t) -> input)
            ; num_inputs= !next_input - 1
            ; handler= Request.Handler.fail
            ; proving_key= None
            ; verification_key= None
            ; proving_key_path= None
            ; verification_key_path= None }
        | {alloc; check; store; _} :: t' ->
            let before_input = !next_input in
            let var = Typ.Alloc.run alloc (alloc_var next_input) in
            let after_input = !next_input in
            let compute = compute var in
            let check_inputs =
              let%bind () = check_inputs in
              with_state (As_prover.return ()) (check var)
            in
            let { compute
                ; reduced_compute
                ; check_inputs
                ; provide_inputs
                ; num_inputs
                ; handler
                ; proving_key
                ; verification_key
                ; proving_key_path
                ; verification_key_path } =
              allocate_inputs ~reduce_to_prover check_inputs next_input t'
                compute
            in
            let provide_inputs input H_list.(value :: values) =
              (* NOTE: We assume here that [store] and [alloc] allocate their
                 variables in the same way, and order them the same way in
                 their output.
                 This is "safe", in that you could never generate a proof when
                 they deviated previously, since the constraints system we
                 generate the keys for and the one satisfied by the prover
                 would be different.
                 Thus, here we only store the values, with the understanding
                 that the values passed from [alloc] above should already be
                 identical. *)
              let next_input = ref before_input in
              let store_field_elt = store_field_elt input next_input in
              let _var = Typ.Store.run (store value) store_field_elt in
              if not (Int.equal !next_input after_input) then
                failwithf
                  "allocate_inputs: Cannot work with this Typ.t. The alloc \
                   method allocates %i field elements, but the store method \
                   allocates %i."
                  (after_input - before_input)
                  (!next_input - before_input)
                  ()
              else provide_inputs input values
            in
            { compute
            ; reduced_compute
            ; check_inputs
            ; provide_inputs
            ; num_inputs
            ; handler
            ; proving_key
            ; verification_key
            ; proving_key_path
            ; verification_key_path }

      let create ~reduce_to_prover ?proving_key ?verification_key
          ?proving_key_path ?verification_key_path
          ?(handlers = ([] : Handler.t list)) ?(reduce = false) ~public_input
          compute =
        let next_input = ref 1 in
        let proof_system =
          allocate_inputs ~reduce_to_prover (Checked.return ()) next_input
            public_input compute
        in
        let force x = ignore (Lazy.force x) in
        if reduce then ignore (force proof_system.reduced_compute) else () ;
        let handler =
          List.fold ~init:proof_system.handler handlers ~f:(fun handler h ->
              Request.Handler.(push handler (create_single h)) )
        in
        { proof_system with
          proving_key
        ; verification_key
        ; proving_key_path
        ; verification_key_path
        ; handler }

      let run_proof_system ~run ?(reduce = !reduce_to_prover) ~input ?system
          ?eval_constraints ?(handlers = ([] : Handler.t list)) proof_system s
          =
        let {num_inputs; _} = proof_system in
        let handler =
          List.fold ~init:proof_system.handler handlers ~f:(fun handler h ->
              Request.Handler.(push handler (create_single h)) )
        in
        let prover_state =
          Checked.Runner.State.make ~num_inputs ~input
            ~next_auxiliary:(ref (num_inputs + 1))
            ~aux:(Field.Vector.create ()) ?system ?eval_constraints ~handler s
        in
        let prover_state, () =
          Checked.run proof_system.check_inputs prover_state
        in
        let compute =
          if reduce && not (Option.is_some prover_state.system) then
            Lazy.force proof_system.reduced_compute
          else proof_system.compute
        in
        let prover_state, a = run compute prover_state in
        Option.iter prover_state.system ~f:(fun system ->
            let aux_input_size =
              !(prover_state.next_auxiliary) - (1 + num_inputs)
            in
            R1CS_constraint_system.set_auxiliary_input_size system
              aux_input_size ;
            R1CS_constraint_system.finalize system ) ;
        (prover_state, a)

      let constraint_system ~run proof_system =
        let input = Field.Vector.create () in
        let system = R1CS_constraint_system.create () in
        ignore (run_proof_system ~run ~input ~system proof_system None) ;
        system

      let digest ~run proof_system =
        let system = constraint_system ~run proof_system in
        R1CS_constraint_system.digest system

      let generate_keypair ~run proof_system =
        let keypair = Keypair.generate (constraint_system ~run proof_system) in
        proof_system.proving_key <- Some keypair.pk ;
        proof_system.verification_key <- Some keypair.vk ;
        (* Write keys to the corresponding files. *)
        Option.iter proof_system.proving_key_path ~f:(fun path ->
            Out_channel.write_all path ~data:(Proving_key.to_string keypair.pk)
        ) ;
        Option.iter proof_system.verification_key_path ~f:(fun path ->
            Out_channel.write_all path
              ~data:(Verification_key.to_string keypair.vk) ) ;
        keypair

      let run_with_input ~run ?reduce ~public_input ?system ?eval_constraints
          ?handlers proof_system s =
        let input =
          proof_system.provide_inputs (Field.Vector.create ()) public_input
        in
        let ({prover_state= s; _} as state), a =
          run_proof_system ~run ?reduce ~input ?system ?eval_constraints
            ?handlers proof_system (Some s)
        in
        match s with
        | Some s ->
            (s, a, state)
        | None ->
            failwith
              "run_with_input: Expected a value from run_proof_system, got \
               None."

      let run_unchecked ~run ~public_input ?handlers ?reduce proof_system eval
          s =
        let s, a, state =
          run_with_input ~run ?reduce ~public_input ?handlers proof_system s
        in
        As_prover.run (eval a) (Checked.Runner.get_value state) s

      let run_checked' ~run ~public_input ?handlers ?reduce proof_system s =
        match
          run_with_input ~run ?reduce ~public_input ~eval_constraints:true
            ?handlers proof_system s
        with
        | exception e ->
            Or_error.of_exn e
        | s, x, state ->
            Ok (s, x, state)

      let run_checked ~run ~public_input ?handlers ?reduce proof_system eval s
          =
        Or_error.map
          (run_checked' ~run ?reduce ~public_input ?handlers proof_system s)
          ~f:(fun (s, x, state) ->
            let s', x =
              As_prover.run (eval x) (Checked.Runner.get_value state) s
            in
            (s', x) )

      let check ~run ~public_input ?handlers ?reduce proof_system s =
        Or_error.map ~f:(Fn.const ())
          (run_checked' ~run ?reduce ~public_input ?handlers proof_system s)

      let read_proving_key proof_system =
        match proof_system.proving_key_path with
        | Some path ->
            Some (Proving_key.of_string (In_channel.read_all path))
        | None ->
            None

      let read_verification_key proof_system =
        match proof_system.verification_key_path with
        | Some path ->
            Some (Verification_key.of_string (In_channel.read_all path))
        | None ->
            None

      let prove ~run ~public_input ?proving_key ?handlers ?reduce ?message
          proof_system s =
        let proving_key =
          List.find_map_exn
            ~f:(fun f -> f ())
            [ (fun () -> proving_key)
            ; (fun () -> proof_system.proving_key)
            ; (fun () -> read_proving_key proof_system)
            ; (fun () -> Some (generate_keypair ~run proof_system).pk) ]
        in
        let system =
          let s = Proving_key.r1cs_constraint_system proving_key in
          if R1CS_constraint_system.get_primary_input_size s = 0 then Some s
          else None
        in
        let _, _, state =
          run_with_input ~run ?reduce ~public_input ?system ?handlers
            proof_system s
        in
        let {input; aux; _} = state in
        Proof.create ?message proving_key ~primary:input ~auxiliary:aux

      let verify ~public_input ?verification_key ?message proof_system proof =
        let input =
          proof_system.provide_inputs (Field.Vector.create ()) public_input
        in
        let verification_key =
          List.find_map_exn
            ~f:(fun f -> f ())
            [ (fun () -> verification_key)
            ; (fun () -> proof_system.verification_key)
            ; (fun () -> read_verification_key proof_system)
            ; (fun () ->
                failwith
                  "Could not verify the proof; no verification key has been \
                   provided." ) ]
        in
        Proof.verify ?message proof verification_key input
    end

    let rec collect_input_constraints : type checked s r2 k1 k2.
        int ref -> (checked, r2, k1, k2) t -> k1 -> (checked, s) Checked.t =
     fun next_input t k ->
      let open Checked in
      match t with
      | [] ->
          Checked.return k
      | {alloc; check; _} :: t' ->
          let var = Typ.Alloc.run alloc (alloc_var next_input) in
          let r = collect_input_constraints next_input t' (k var) in
          let%map () = with_state (As_prover.return ()) (check var)
          and r = r in
          r

    let r1cs_h : type a s checked r2 k1 k2.
           run:(a, s, checked) Checked.Runner.run
        -> int ref
        -> (checked, r2, k1, k2) t
        -> k1
        -> R1CS_constraint_system.t =
     fun ~run next_input t k ->
      let r = collect_input_constraints next_input t k in
      let run_in_run r state =
        let state, x = Checked.run r state in
        run x state
      in
      Checked.constraint_system ~run:run_in_run ~num_inputs:(!next_input - 1) r

    let constraint_system (type a s checked k_var) :
           run:(a, s, checked) Checked.Runner.run
        -> exposing:(checked, _, k_var, _) t
        -> k_var
        -> R1CS_constraint_system.t =
     fun ~run ~exposing k -> r1cs_h ~run (ref 1) exposing k

    let generate_keypair :
           run:(_, _, 'checked) Checked.Runner.run
        -> exposing:('checked, _, 'k_var, _) t
        -> 'k_var
        -> Keypair.t =
     fun ~run ~exposing k ->
      Keypair.generate (constraint_system ~run ~exposing k)

    let verify :
           ?message:Proof.message
        -> Proof.t
        -> Verification_key.t
        -> ('r_var, bool, 'k_var, 'k_value) t
        -> 'k_value =
     fun ?message proof vk t0 ->
      let primary_input = Field.Vector.create () in
      let next_input = ref 1 in
      let store_field_elt = store_field_elt primary_input next_input in
      let rec go : type r_var k_var k_value.
          (r_var, bool, k_var, k_value) t -> k_value =
       fun t ->
        match t with
        | [] ->
            Proof.verify ?message proof vk primary_input
        | {store; _} :: t' ->
            fun value ->
              let _var = Typ.Store.run (store value) store_field_elt in
              go t'
      in
      go t0

    let conv : type r_var r_value.
           (r_var -> Field.Vector.t -> r_value)
        -> (r_var, r_value, 'k_var, 'k_value) t
        -> 'k_var
        -> 'k_value =
     fun cont0 t0 k0 ->
      let primary_input = Field.Vector.create () in
      let store_field_elt =
        let next_input = ref 1 in
        fun x ->
          let v = !next_input in
          incr next_input ;
          Field.Vector.emplace_back primary_input x ;
          Cvar.Unsafe.of_index v
      in
      let rec go : type k_var k_value.
          (r_var, r_value, k_var, k_value) t -> k_var -> k_value =
       fun t k ->
        match t with
        | [] ->
            cont0 k primary_input
        | {store; _} :: t' ->
            fun value ->
              let var = Typ.Store.run (store value) store_field_elt in
              go t' (k var)
      in
      go t0 k0

    let prove :
           run:('a, 's, 'checked) Checked.Runner.run
        -> ?message:Proof.message
        -> Proving_key.t
        -> ('checked, Proof.t, 'k_var, 'k_value) t
        -> ?handlers:Handler.t list
        -> 's
        -> 'k_var
        -> 'k_value =
     fun ~run ?message key t ?handlers s k ->
      conv
        (fun c primary ->
          let system =
            let s = Proving_key.r1cs_constraint_system key in
            if R1CS_constraint_system.get_primary_input_size s = 0 then Some s
            else None
          in
          let auxiliary =
            Checked.auxiliary_input ?system ~run ?handlers
              ~num_inputs:(Field.Vector.length primary)
              c s primary
          in
          Proof.create ?message key ~primary ~auxiliary )
        t k

    let generate_auxiliary_input :
           run:('a, 's, 'checked) Checked.Runner.run
        -> ('checked, unit, 'k_var, 'k_value) t
        -> ?handlers:Handler.t list
        -> 's
        -> 'k_var
        -> 'k_value =
     fun ~run t ?handlers s k ->
      conv
        (fun c primary ->
          let auxiliary =
            Checked.auxiliary_input ~run ?handlers
              ~num_inputs:(Field.Vector.length primary)
              c s primary
          in
          ignore auxiliary )
        t k

    let reduce_to_prover : type a s.
           ((a, s, Field.t) Checked_ast.t, Proof.t, 'k_var, 'k_value) t
        -> 'k_var
        -> (Proving_key.t -> ?handlers:Handler.t list -> s -> 'k_value)
           Staged.t =
     fun t0 k0 ->
      let next_input = ref 1 in
      let alloc_var () =
        let v = !next_input in
        incr next_input ; Cvar.Unsafe.of_index v
      in
      let rec go : type k_var k_value.
             ((a, s, Field.t) Checked_ast.t, Proof.t, k_var, k_value) t
          -> k_var
          -> k_var =
       fun t k ->
        match t with
        | [] ->
            Checked.Runner.reduce_to_prover next_input k
        | {alloc; _} :: t' ->
            let var = Typ.Alloc.run alloc alloc_var in
            let ret = go t' (k var) in
            fun _ -> ret
      in
      let reduced = go t0 k0 in
      stage (fun key ?handlers s ->
          prove ~run:Checked.Runner.run key t0 ?handlers s reduced )
  end

  module Cvar1 = struct
    include Cvar

    let project (vars : Checked.Boolean.var list) =
      let rec go c acc = function
        | [] ->
            List.rev acc
        | v :: vs ->
            go (Field.add c c) ((c, v) :: acc) vs
      in
      Cvar.linear_combination (go Field.one [] (vars :> Cvar.t list))

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

    let typ = Typ.field

    type var' = Var.t

    module Var = Cvar1

    module Checked = struct
      include Cvar1

      let equal = Checked.equal

      let mul x y = Checked.mul ~label:"Field.Checked.mul" x y

      let square x = Checked.square ~label:"Field.Checked.square" x

      let div x y = Checked.div ~label:"Field.Checked.div" x y

      let inv x = Checked.inv ~label:"Field.Checked.inv" x

      let choose_preimage_var = Checked.choose_preimage

      type comparison_result =
        {less: Checked.Boolean.var; less_or_equal: Checked.Boolean.var}

      let if_ = Checked.if_

      let compare ~bit_length a b =
        let open Checked in
        let open Let_syntax in
        [%with_label_ "compare"]
          (let alpha_packed =
             Cvar.(constant (two_to_the bit_length) + b - a)
           in
           let%bind alpha = unpack alpha_packed ~length:Int.(bit_length + 1) in
           let prefix, less_or_equal =
             match Core_kernel.List.split_n alpha bit_length with
             | p, [l] ->
                 (p, l)
             | _ ->
                 failwith "compare: Invalid alpha"
           in
           let%bind not_all_zeros = Boolean.any prefix in
           let%map less = Boolean.(less_or_equal && not_all_zeros) in
           {less; less_or_equal})

      module Assert = struct
        let lt ~bit_length x y =
          let open Checked in
          let open Let_syntax in
          let%bind {less; _} = compare ~bit_length x y in
          Boolean.Assert.is_true less

        let lte ~bit_length x y =
          let open Checked in
          let open Let_syntax in
          let%bind {less_or_equal; _} = compare ~bit_length x y in
          Boolean.Assert.is_true less_or_equal

        let gt ~bit_length x y = lt ~bit_length y x

        let gte ~bit_length x y = lte ~bit_length y x

        let non_zero = Checked.assert_non_zero

        let equal x y = Checked.assert_equal ~label:"Checked.Assert.equal" x y

        let not_equal (x : t) (y : t) =
          Checked.with_label "Checked.Assert.not_equal" (non_zero (sub x y))
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
                  And [Lit x; Lit y; of_binary t]
              | Or (x, Or (y, t)) ->
                  Or [Lit x; Lit y; of_binary t]
              | And (x, t) ->
                  And [Lit x; of_binary t]
              | Or (x, t) ->
                  Or [Lit x; of_binary t]

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
          | [_x], [false] ->
              Lit Boolean.false_
          | [x], [true] ->
              Lit (Boolean.not x)
          | [x1; _x2], [true; false] ->
              Lit (Boolean.not x1)
          | [_x1; _x2], [false; false] ->
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
            go ((Cvar1.pack t1_a, Cvar1.pack t2_a) :: acc) t1_b t2_b
      in
      go [] t1 t2

    let equal t1 t2 =
      let open Checked in
      all
        (Core.List.map (chunk_for_equality t1 t2) ~f:(fun (x1, x2) ->
             equal x1 x2 ))
      >>= Boolean.all

    module Assert = struct
      let equal t1 t2 =
        let open Checked in
        Core.List.map (chunk_for_equality t1 t2) ~f:(fun (x1, x2) ->
            Constraint.equal x1 x2 )
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
        let correct_answer = x < y in
        let (), lt =
          Checked.run_and_check ~run:Checked.run
            (Checked.map
               ~f:(As_prover.read Checked.Boolean.typ)
               (Field.Checked.lt_bitstring_value
                  (Bitstring_lib.Bitstring.Msb_first.of_list
                     (List.map ~f:Checked.Boolean.var_of_value x))
                  (Bitstring_lib.Bitstring.Msb_first.of_list y)))
            ()
          |> Or_error.ok_exn
        in
        assert (lt = correct_answer) )

  include Checked

  module Proof_system = struct
    open Run.Proof_system

    type ('a, 's, 'inputs) t = (('a, 's) Checked.t, 'inputs, 's) proof_system

    let create ?proving_key ?verification_key ?proving_key_path
        ?verification_key_path ?handlers ?reduce ~public_input checked =
      create ~reduce_to_prover:Runner.reduce_to_prover ?proving_key
        ?verification_key ?proving_key_path ?verification_key_path ?handlers
        ?reduce ~public_input checked

    let constraint_system (proof_system : _ t) =
      constraint_system ~run:Checked.run proof_system

    let digest (proof_system : _ t) = digest ~run:Checked.run proof_system

    let generate_keypair (proof_system : _ t) =
      generate_keypair ~run:Checked.run proof_system

    let run_unchecked ~public_input ?handlers ?reduce (proof_system : _ t) =
      run_unchecked ~run:Checked.run ~public_input ?handlers ?reduce
        proof_system

    let run_checked ~public_input ?handlers ?reduce (proof_system : _ t) =
      run_checked ~run:Checked.run ~public_input ?handlers ?reduce proof_system

    let check ~public_input ?handlers ?reduce (proof_system : _ t) =
      check ~run:Checked.run ~public_input ?handlers ?reduce proof_system

    let prove ~public_input ?proving_key ?handlers ?reduce ?message
        (proof_system : _ t) =
      prove ~run:Checked.run ~public_input ?proving_key ?handlers ?reduce
        ?message proof_system

    let verify ~public_input ?verification_key ?message (proof_system : _ t) =
      verify ~public_input ?verification_key ?message proof_system
  end

  module Perform = struct
    type ('a, 't) t =
      't -> unit Checked.run_state -> unit Checked.run_state * 'a

    let generate_keypair ~run ~exposing k =
      Run.generate_keypair ~run ~exposing k

    let prove ~run ?message key t k = Run.prove ~run ?message key t () k

    let verify = Run.verify

    let constraint_system = Run.constraint_system

    let run_unchecked ~run t = snd (run_unchecked ~run t ())

    let run_and_check ~run t = Or_error.map (run_and_check ~run t ()) ~f:snd

    let check ~run t = check ~run t ()
  end

  let generate_keypair ~exposing k =
    Run.generate_keypair ~run:Checked.run ~exposing k

  let conv f = Run.conv (fun x _ -> f x)

  let prove ?message key t s k = Run.prove ~run:Checked.run ?message key t s k

  let generate_auxiliary_input t s k =
    Run.generate_auxiliary_input ~run:Checked.run t s k

  let verify = Run.verify

  let constraint_system ~exposing k =
    Run.constraint_system ~run:Checked.run ~exposing k

  let run_unchecked t s = run_unchecked ~run:Checked.run t s

  let run_and_check t s = run_and_check ~run:Checked.run t s

  let check t s = check ~run:Checked.run t s

  module Test = struct
    let checked_to_unchecked typ1 typ2 checked input =
      let (), checked_result =
        run_and_check
          (let open Let_syntax in
          let%bind input = exists typ1 ~compute:(As_prover.return input) in
          let%map result = checked input in
          As_prover.read typ2 result)
          ()
        |> Or_error.ok_exn
      in
      checked_result

    let test_equal (type a) ?(sexp_of_t = sexp_of_opaque) ?(equal = ( = )) typ1
        typ2 checked unchecked input =
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

  module Basic =
    Make_basic
      (Backend_extended)
      (struct
        include (
          Checked :
            Checked_intf.S
            with module Types = Checked.Types
            with type ('a, 's, 'f) t := ('a, 's, 'f) Checked.t
             and type 'f field := 'f )

        type field = Backend_extended.Field.t

        type ('a, 's) t = ('a, 's, field) Types.Checked.t

        let run = Runner0.run
      end)
      (As_prover.Make (struct
           type field = Backend_extended.Field.t
         end)
         (Checked)
         (As_prover.Make_basic (Checked)))
      (Runner0)

  include Basic
  module Number = Number.Make (Basic)
  module Enumerable = Enumerable.Make (Basic)
end

module Run = struct
  module Make_basic (Backend : Backend_intf.S) = struct
    module Snark = Make (Backend)
    open Run_state
    open Snark

    let set_constraint_logger = set_constraint_logger

    let clear_constraint_logger = clear_constraint_logger

    let state =
      ref
        { system= None
        ; input= Field.Vector.create ()
        ; aux= Field.Vector.create ()
        ; eval_constraints= false
        ; num_inputs= 0
        ; next_auxiliary= ref 1
        ; prover_state= None
        ; stack= []
        ; handler= Request.Handler.fail
        ; is_running= false
        ; as_prover= ref false
        ; log_constraint= None }

    let run checked =
      if !(!state.as_prover) then
        failwith
          "Can't run checked code as the prover: the verifier's constraint \
           system will not match." ;
      if not !state.is_running then
        failwith "This function can't be run outside of a checked computation." ;
      let state', x = Runner.run checked !state in
      state := state' ;
      x

    let as_stateful x state' =
      state := state' ;
      let a = x () in
      (!state, a)

    module Proving_key = Snark.Proving_key
    module Verification_key = Snark.Verification_key
    module R1CS_constraint_system = Snark.R1CS_constraint_system
    module Keypair = Snark.Keypair
    module Var = Snark.Var

    type field = Snark.field

    module Bigint = Snark.Bigint
    module Constraint = Snark.Constraint
    module Data_spec = Snark.Data_spec

    module Typ = struct
      open Snark.Typ
      module Store = Store
      module Alloc = Alloc
      module Read = Read

      type 'prover_state run_state = 'prover_state Snark.Checked.run_state

      type nonrec ('var, 'value) t = ('var, 'value) t

      let store = store

      let read = read

      let alloc = alloc

      let check typ var = run (check typ var)

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

      module Of_traversable = Of_traversable
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

      let ( || ) x y = run (x || y)

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

        let ( || ) = ( || )

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
    end

    module Field = struct
      open Snark.Field

      let size_in_bits = size_in_bits

      let size = size

      module Constant = struct
        type t = Snark.Field.t [@@deriving bin_io, sexp, hash, compare, eq]

        let gen = gen

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

          let size = size

          let unpack = unpack

          let project = project
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

      let equal x y = run (equal x y)

      let unpack x ~length = run (unpack x ~length)

      let unpack_flagged x ~length = run (unpack_flagged x ~length)

      let unpack_full x = run (unpack_full x)

      let choose_preimage_var x ~length = run (choose_preimage_var x ~length)

      type nonrec comparison_result = comparison_result =
        {less: Boolean.var; less_or_equal: Boolean.var}

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

    module Proof = Proof

    module Bitstring_checked = struct
      open Snark.Bitstring_checked

      type nonrec t = t

      let equal x y = run (equal x y)

      let lt_value x y = run (lt_value x y)

      module Assert = struct
        open Snark.Bitstring_checked.Assert

        let equal x y = run (equal x y)
      end
    end

    module As_prover = struct
      type 'a t = 'a

      let eval_as_prover f =
        if !(!state.as_prover) && Option.is_some !state.prover_state then (
          let s = Option.value_exn !state.prover_state in
          let s, a = f (Runner.get_value !state) s in
          state := Run_state.set_prover_state (Some s) !state ;
          a )
        else failwith "Can't evaluate prover code outside an as_prover block"

      let in_prover_block () = !(!state.as_prover)

      let read_var var = eval_as_prover (As_prover.read_var var)

      let get_state () = eval_as_prover As_prover.get_state

      let set_state s = eval_as_prover (As_prover.set_state s)

      let modify_state f = eval_as_prover (As_prover.modify_state f)

      let read typ var = eval_as_prover (As_prover.read typ var)

      include Field.Constant.T

      let run_prover f tbl s =
        let old = !(!state.as_prover) in
        !state.as_prover := true ;
        state := Run_state.set_prover_state (Some s) !state ;
        let a = f () in
        let s' = Option.value_exn !state.prover_state in
        !state.as_prover := old ;
        (s', a)
    end

    module Handle = struct
      type ('var, 'value) t = ('var, 'value) Handle.t

      let value handle () = As_prover.eval_as_prover (Handle.value handle)

      let var = Handle.var
    end

    module Proof_system = struct
      open Run.Proof_system

      type ('a, 'public_input) t =
        (unit -> 'a, 'public_input, unit) proof_system

      let create ?proving_key ?verification_key ?proving_key_path
          ?verification_key_path ?handlers ~public_input checked =
        create
          ~reduce_to_prover:(fun i f -> f)
          ?proving_key ?verification_key ?proving_key_path
          ?verification_key_path ?handlers ~public_input checked

      let run = as_stateful

      let constraint_system (proof_system : _ t) =
        constraint_system ~run proof_system

      let digest (proof_system : _ t) = digest ~run proof_system

      let generate_keypair (proof_system : _ t) =
        generate_keypair ~run proof_system

      let run_unchecked ~public_input ?handlers (proof_system : _ t) =
        snd
          (run_unchecked ~run ~public_input ?handlers proof_system
             (fun a _ s -> (s, a))
             ())

      let run_checked ~public_input ?handlers (proof_system : _ t) =
        Or_error.map
          (run_checked' ~run ~public_input ?handlers proof_system ())
          ~f:(fun (s, x, state) -> x)

      let check ~public_input ?handlers (proof_system : _ t) =
        Or_error.map ~f:(Fn.const ())
          (run_checked' ~run ~public_input ?handlers proof_system ())

      let prove ~public_input ?proving_key ?handlers ?message
          (proof_system : _ t) =
        prove ~run ~public_input ?proving_key ?handlers ?message proof_system
          ()

      let verify ~public_input ?verification_key ?message (proof_system : _ t)
          =
        verify ~public_input ?verification_key ?message proof_system
    end

    let assert_ ?label c = run (assert_ ?label c)

    let assert_all ?label c = run (assert_all ?label c)

    let assert_r1cs ?label a b c = run (assert_r1cs ?label a b c)

    let assert_square ?label x y = run (assert_square ?label x y)

    let as_prover p = run (as_prover (As_prover.run_prover p))

    let next_auxiliary () = run next_auxiliary

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
          { request: 'a Request.t
          ; respond: 'a Request.Response.t -> response }
          -> request

    module Handler = Handler

    let handle x h =
      let h = Request.Handler.create_single h in
      let {handler; _} = !state in
      state := {!state with handler= Request.Handler.push handler h} ;
      let a = x () in
      state := {!state with handler} ;
      a

    let with_label lbl x =
      let {stack; _} = !state in
      state := {!state with stack= lbl :: stack} ;
      let a = x () in
      state := {!state with stack} ;
      a

    let make_checked x =
      let f state =
        let {prover_state; _} = state in
        let state =
          Run_state.set_prover_state
            (Option.map prover_state ~f:(fun _ -> ()))
            state
        in
        let state, a = as_stateful x state in
        let state = Run_state.set_prover_state prover_state state in
        (state, a)
      in
      Types.Checked.Direct (f, fun x -> Pure x)

    let constraint_system ~exposing x =
      Perform.constraint_system ~run:as_stateful ~exposing x

    let generate_keypair ~exposing x =
      Perform.generate_keypair ~run:as_stateful ~exposing x

    let prove ?message pk x = Perform.prove ~run:as_stateful ?message pk x

    let verify ?message pf vk spec = verify ?message pf vk spec

    let run_unchecked x = Perform.run_unchecked ~run:as_stateful x

    let run_and_check x =
      let res =
        Perform.run_and_check ~run:as_stateful (fun () ->
            let prover_block = x () in
            !state.as_prover := true ;
            As_prover.run_prover prover_block )
      in
      !state.as_prover := true ;
      res

    let check x = Perform.check ~run:as_stateful x |> Result.is_ok

    let constraint_count ?(log = fun ?start _ _ -> ()) x =
      let count = ref 0 in
      let log_constraint c = count := !count + Core_kernel.List.length c in
      let old = !state in
      state :=
        Runner.State.make ~num_inputs:0 ~input:Vector.null ~aux:Vector.null
          ~next_auxiliary:(ref 1) ~eval_constraints:false None ;
      state := {!state with log_constraint= Some log_constraint} ;
      ignore (x ()) ;
      state := old ;
      !count

    module Internal_Basic = Snark

    let run_checked = run
  end

  module Make (Backend : Backend_intf.S) = struct
    module Basic = Make_basic (Backend)
    include Basic
    module Number = Number.Run.Make (Basic)
    module Enumerable = Enumerable.Run.Make (Basic)
  end
end

type 'field m = (module Snark_intf.Run with type field = 'field)

let make (type field)
    (module Backend : Backend_intf.S with type Field.t = field) : field m =
  (module Run.Make (Backend))

let%test_module "snark0-test" =
  ( module struct
    include Make (Backends.Mnt4.Default)

    let bin_io_id m = Fn.compose (Binable.of_string m) (Binable.to_string m)

    let swap b (x, y) = if b then (y, x) else (x, y)

    let%test_unit "key serialization" =
      let main x =
        let%bind y = exists Field.typ ~compute:(As_prover.return Field.zero) in
        let rec go b acc i =
          if i = 0 then return acc
          else
            let%bind z =
              Tuple2.uncurry Field.Checked.mul
                (swap b (Field.Checked.add y acc, x))
            in
            go b z (i - 1)
        in
        let%bind _ = go false x 19 in
        let%bind _ = go true y 20 in
        return ()
      in
      let kp = generate_keypair ~exposing:[Field.typ] main in
      let vk = Keypair.vk kp |> bin_io_id (module Verification_key) in
      let pk = Keypair.pk kp |> bin_io_id (module Proving_key) in
      let input = Field.one in
      let proof = prove pk [Field.typ] () main input in
      assert (verify proof vk [Field.typ] input)
  end )
