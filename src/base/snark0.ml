module Cvar0 = Cvar
module Bignum_bigint = Bigint
module Checked_ast = Checked
module Typ_monads0 = Typ_monads
open Core_kernel

exception Runtime_error = Checked_runner.Runtime_error

module Runner = Checked_runner

let set_eval_constraints b = Runner.eval_constraints := b

let reduce_to_prover = ref false

let set_reduce_to_prover b = reduce_to_prover := b

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
               and type constr := Backend.Constraint.t
               and type r1cs := Backend.R1CS_constraint_system.t) =
struct
  open Backend
  module Checked_S = Checked_intf.Unextend (Checked)

  let set_constraint_logger = Runner.set_constraint_logger

  let clear_constraint_logger = Runner.clear_constraint_logger

  type field = Field.t

  let field_vec_id : Field.Vector.t Type_equal.Id.t =
    Type_equal.Id.create ~name:"field-vector" sexp_of_opaque

  let pack_field_vec v =
    Run_state.Vector.T ((module Field.Vector), field_vec_id, v)

  let field_vec () = pack_field_vec (Field.Vector.create ())

  let cast_field_vec_exn (T (_, id, v) : _ Run_state.Vector.t) : Field.Vector.t
      =
    let T = Type_equal.Id.same_witness_exn id field_vec_id in
    v

  module Proof_inputs = struct
    type t = {public_inputs: Field.Vector.t; auxiliary_inputs: Field.Vector.t}
  end

  module Bigint = Bigint

  module Proof = struct
    include Proof

    let of_inputs ?message key
        {Proof_inputs.public_inputs= primary; auxiliary_inputs= auxiliary} =
      create ?message key ~primary ~auxiliary
  end

  module Verification_key = struct
    include Verification_key

    type verification_key = t [@@deriving bin_io]

    module With_r1cs_hash = struct
      type t = Md5.t * verification_key [@@deriving bin_io]
    end
  end

  module Proving_key = struct
    include Proving_key

    type proving_key = t [@@deriving bin_io]

    module With_r1cs_hash = struct
      type t = Md5.t * proving_key [@@deriving bin_io]
    end
  end

  module Keypair = Keypair
  module Var = Var
  module Field0 = Field
  module Cvar = Cvar
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
    end

    module Alloc = struct
      open Alloc
      include Restrict_monad.Make2 (Alloc) (Field)

      let alloc = alloc

      let run = run
    end
  end

  module Handler = struct
    type t = Request.request -> Request.response
  end

  module Typ = struct
    include Types.Typ.T
    module T = Typ.Make (Checked_S) (As_prover)
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
    open Run_state

    include (
      Checked :
        Checked_intf.Extended
        with module Types := Checked.Types
        with type field := field )

    let perform req = request_witness Typ.unit req

    module Runner = Runner

    type 'prover_state run_state = 'prover_state Runner.run_state

    (* TODO-someday: Add pass to unify variables which have an Equal constraint *)
    let constraint_system ~run ~num_inputs t : R1CS_constraint_system.t =
      let input = field_vec () in
      let next_auxiliary = ref (1 + num_inputs) in
      let aux = field_vec () in
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
        Runner.State.make ?system ~num_inputs ~input:(pack_field_vec input)
          ~next_auxiliary ~aux:(pack_field_vec aux) ~handler (Some s0)
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
          ~aux:(pack_field_vec aux) ~system ~eval_constraints:true (Some s0)
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
      let input = field_vec () in
      let next_auxiliary = ref 1 in
      let aux = field_vec () in
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

    (* [equal_constraints z z_inv r] asserts that
       if z = 0 then r = 1, or
       if z <> 0 then r = 0 and z * z_inv = 1
    *)
    let equal_constraints (z : Cvar.t) (z_inv : Cvar.t) (r : Cvar.t) =
      let open Constraint in
      let open Cvar in
      assert_all
        [ r1cs ~label:"equals_1" z_inv z (Cvar.constant Field.one - r)
        ; r1cs ~label:"equals_2" r z (Cvar.constant Field.zero) ]

    (* [equal_vars z] computes [(r, z_inv)] that satisfy the constraints in
       [equal_constraints z z_inv r].

       In particular, [r] is [1] if [z = 0] and [0] otherwise.
    *)
    let equal_vars (z : Cvar.t) : (Field.t * Field.t, _) As_prover.t =
      let open As_prover in
      let%map z = read_var z in
      if Field.equal z Field.zero then (Field.one, Field.zero)
      else (Field.zero, Field.inv z)

    let equal (x : Cvar.t) (y : Cvar.t) : (Cvar.t Boolean.t, _) t =
      let z = Cvar.(x - y) in
      let%bind r, inv = exists Typ.(field * field) ~compute:(equal_vars z) in
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
            let%map () = assert_r1cs b Cvar.(then_ - else_) Cvar.(r - else_) in
            r )

    let%snarkydef_ assert_non_zero (v : Cvar.t) =
      let open Let_syntax in
      let%map _ = inv v in
      ()

    (** Read the [Cvar.t]s that represent the value [x].

        WARNING: This assumes that reading zero will not cause an error within
        the [read] function.
    *)
    let unsafe_read_cvars {Typ.read; _} x =
      Typ_monads0.Read.run
        (Typ_monads0.Read.make_cvars (read x))
        (fun _ -> Field.zero)

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

      let ( && ) (x : var) (y : var) : (var, _) Checked.t =
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

      let%test_unit "all" =
        let gen =
          let open Quickcheck.Generator in
          let open Let_syntax in
          let%bind length = small_positive_int in
          list_with_length length bool
        in
        Quickcheck.test gen ~sexp_of:[%sexp_of: bool list] ~f:(fun x ->
            let (), r =
              run_and_check ~run
                (Checked.map ~f:(As_prover.read typ)
                   (all (List.map ~f:var_of_value x)))
                ()
              |> Or_error.ok_exn
            in
            [%test_eq: bool] r (List.for_all x ~f:Fn.id) )

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

      module Array = struct
        let num_true (bs : var array) =
          Array.fold bs ~init:(Cvar.constant Field.zero) ~f:(fun x y ->
              Cvar.add x (y :> Cvar.t) )

        let any = function
          | [||] ->
              return false_
          | [|b1|] ->
              return b1
          | [|b1; b2|] ->
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
          | [|b1|] ->
              return b1
          | [|b1; b2|] ->
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

        let ( && ) x y = And [x; y]

        let ( &&& ) = ( && )

        let ( || ) x y = Or [x; y]

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
        ; mutable r1cs_digest: Md5.t option
        ; proving_key_path: string option
        ; verification_key_path: string option
        ; keys_have_hashes: bool }

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
            ; r1cs_digest= None
            ; proving_key_path= None
            ; verification_key_path= None
            ; keys_have_hashes= false }
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
                ; r1cs_digest
                ; proving_key_path
                ; verification_key_path
                ; keys_have_hashes } =
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
            ; r1cs_digest
            ; proving_key_path
            ; verification_key_path
            ; keys_have_hashes }

      let create ~reduce_to_prover ?proving_key ?verification_key
          ?proving_key_path ?verification_key_path ?(keys_with_hashes = true)
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
        ; handler
        ; keys_have_hashes= keys_with_hashes }

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
            ~aux:(field_vec ()) ?system ?eval_constraints ~handler s
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
        Option.iter prover_state.system
          ~f:(fun (T ((module C), system) : Field.t Constraint_system.t) ->
            proof_system.r1cs_digest <- Some (C.digest system) ;
            let aux_input_size =
              !(prover_state.next_auxiliary) - (1 + num_inputs)
            in
            C.set_auxiliary_input_size system aux_input_size ;
            C.finalize system ) ;
        (prover_state, a)

      let constraint_system ~run proof_system =
        let input = field_vec () in
        let system = R1CS_constraint_system.create () in
        ignore (run_proof_system ~run ~input ~system proof_system None) ;
        system

      let digest ~run proof_system =
        match proof_system.r1cs_digest with
        | Some digest ->
            (* Use cached digest. *)
            digest
        | None ->
            let system = constraint_system ~run proof_system in
            R1CS_constraint_system.digest system

      let generate_keypair ~run proof_system =
        let system = constraint_system ~run proof_system in
        let keypair = Keypair.generate system in
        proof_system.proving_key <- Some keypair.pk ;
        proof_system.verification_key <- Some keypair.vk ;
        (* Write keys to the corresponding files. *)
        if proof_system.keys_have_hashes then (
          let digest = digest ~run proof_system in
          Option.iter proof_system.proving_key_path ~f:(fun path ->
              Bin_prot_io.write
                (module Proving_key.With_r1cs_hash)
                path ~data:(digest, keypair.pk) ) ;
          Option.iter proof_system.verification_key_path ~f:(fun path ->
              Bin_prot_io.write
                (module Verification_key.With_r1cs_hash)
                path ~data:(digest, keypair.vk) ) )
        else (
          Option.iter proof_system.proving_key_path ~f:(fun path ->
              Bin_prot_io.write (module Proving_key) path ~data:keypair.pk ) ;
          Option.iter proof_system.verification_key_path ~f:(fun path ->
              Bin_prot_io.write (module Verification_key) path ~data:keypair.vk
          ) ) ;
        keypair

      let run_with_input ~run ?reduce ~public_input ?system ?eval_constraints
          ?handlers proof_system s =
        let input =
          proof_system.provide_inputs (Field.Vector.create ()) public_input
        in
        let ({prover_state= s; _} as state), a =
          run_proof_system ~run ?reduce ~input:(pack_field_vec input) ?system
            ?eval_constraints ?handlers proof_system (Some s)
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

      let read_proving_key ?r1cs proof_system =
        match proof_system.proving_key_path with
        | Some path -> (
          try
            let ret =
              match r1cs with
              | Some r1cs ->
                  let digest = R1CS_constraint_system.digest r1cs in
                  proof_system.r1cs_digest <- Some digest ;
                  let digest', pk =
                    Bin_prot_io.read (module Proving_key.With_r1cs_hash) path
                  in
                  if Md5.equal digest digest' then (
                    (* We don't store the R1CS for keys, so set it here if we
                       know it.
                    *)
                    Backend.Proving_key.set_constraint_system pk r1cs ;
                    Some pk )
                  else None
              | None ->
                  Some (Bin_prot_io.read (module Proving_key) path)
            in
            Option.iter ret ~f:(fun key -> proof_system.proving_key <- Some key) ;
            ret
          with _ -> None )
        | None ->
            None

      let read_verification_key ?digest proof_system =
        match proof_system.verification_key_path with
        | Some path -> (
          try
            let ret =
              match digest with
              | Some digest ->
                  let digest', vk =
                    Bin_prot_io.read
                      (module Verification_key.With_r1cs_hash)
                      path
                  in
                  if Md5.equal digest digest' then Some vk else None
              | None ->
                  Some (Bin_prot_io.read (module Verification_key) path)
            in
            Option.iter ret ~f:(fun key ->
                proof_system.verification_key <- Some key ) ;
            ret
          with _ -> None )
        | None ->
            None

      let prove ~run ~public_input ?proving_key ?handlers ?reduce ?message
          proof_system s =
        let proving_key =
          List.find_map_exn
            ~f:(fun f -> f ())
            [ (fun () -> proving_key)
            ; (fun () -> proof_system.proving_key)
            ; (fun () ->
                let r1cs =
                  if proof_system.keys_have_hashes then
                    Some (constraint_system ~run proof_system)
                  else None
                in
                read_proving_key ?r1cs proof_system )
            ; (fun () -> Some (generate_keypair ~run proof_system).pk) ]
        in
        let system =
          match Proving_key.is_initialized proving_key with
          | `Yes ->
              None
          | `No s ->
              Some s
        in
        let _, _, state =
          run_with_input ~run ?reduce ~public_input ?system ?handlers
            proof_system s
        in
        let {input; aux; _} = state in
        Proof.create ?message proving_key ~primary:(cast_field_vec_exn input)
          ~auxiliary:(cast_field_vec_exn aux)

      let verify ~run ~public_input ?verification_key ?message proof_system
          proof =
        let input =
          proof_system.provide_inputs (Field.Vector.create ()) public_input
        in
        let verification_key =
          List.find_map_exn
            ~f:(fun f -> f ())
            [ (fun () -> verification_key)
            ; (fun () -> proof_system.verification_key)
            ; (fun () ->
                let digest =
                  if proof_system.keys_have_hashes then
                    Some (digest ~run proof_system)
                  else None
                in
                read_verification_key ?digest proof_system )
            ; (fun () ->
                failwith
                  "Could not verify the proof; no verification key has been \
                   provided." ) ]
        in
        Proof.verify ?message proof verification_key input

      let generate_witness ~run ~public_input ?handlers ?reduce proof_system s
          =
        let _, _, state =
          run_with_input ~run ?reduce ~public_input ?handlers proof_system s
        in
        let {input; aux; _} = state in
        { Proof_inputs.public_inputs= cast_field_vec_exn input
        ; auxiliary_inputs= cast_field_vec_exn aux }
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

    let generate_public_input :
        ('r_var, Field.Vector.t, 'k_var, 'k_value) t -> 'k_value =
     fun t0 ->
      let primary_input = Field.Vector.create () in
      let next_input = ref 1 in
      let store_field_elt = store_field_elt primary_input next_input in
      let rec go : type r_var k_var k_value.
          (r_var, Field.Vector.t, k_var, k_value) t -> k_value =
       fun t ->
        match t with
        | [] ->
            primary_input
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

    let conv_never_use : type r_var r_value.
           (unit -> 'hack)
        -> (unit -> r_var, r_value, 'k_var, 'k_value) t
        -> ('hack -> 'k_var)
        -> 'k_var =
     fun f t k ->
      let rec go : type k_var k_value.
             (unit -> r_var, r_value, k_var, k_value) t
          -> ('hack -> k_var)
          -> k_var =
       fun t ->
        match t with
        | [] ->
            fun k () ->
              let hack = f () in
              k hack ()
        | _ :: t' ->
            fun k arg -> go t' (fun hack -> k hack arg)
      in
      go t k

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
            match Proving_key.is_initialized key with
            | `Yes ->
                None
            | `No s ->
                Some s
          in
          let run =
            if Option.is_none system then run
            else
              (* If we're regenerating the R1CS, we need to collect and
                 evaluate the constraints for the inputs.
              *)
              let next_input = ref 1 in
              let r = collect_input_constraints next_input t k in
              let run_in_run x state =
                let state, _x = Checked.run r state in
                assert (Int.equal !next_input (Field.Vector.length primary + 1)) ;
                run x state
              in
              run_in_run
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

    let generate_witness_conv :
           run:('a, 's, 'checked) Checked.Runner.run
        -> f:(Proof_inputs.t -> 'out)
        -> ('checked, 'out, 'k_var, 'k_value) t
        -> ?handlers:Handler.t list
        -> 's
        -> 'k_var
        -> 'k_value =
     fun ~run ~f t ?handlers s k ->
      conv
        (fun c primary ->
          let auxiliary =
            Checked.auxiliary_input ~run ?handlers
              ~num_inputs:(Field.Vector.length primary)
              c s primary
          in
          f {Proof_inputs.public_inputs= primary; auxiliary_inputs= auxiliary}
          )
        t k

    let generate_witness = generate_witness_conv ~f:Fn.id
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

      let sqrt x =
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
           go 2)

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
          let (), answer =
            Checked.run_and_check ~run:Checked.run
              (Checked.map
                 ~f:(As_prover.read Checked.Boolean.typ)
                 Checked.(
                   Let_syntax.(
                     let%bind x =
                       exists typf ~compute:(As_prover.return elt)
                     in
                     is_square x)))
              ()
            |> Or_error.ok_exn
          in
          answer
        in
        assert (run x2) ;
        assert (not (run (Field.mul (Lazy.force quadratic_nonresidue) x2)))

      let choose_preimage_var = Checked.choose_preimage

      type comparison_result =
        {less: Checked.Boolean.var; less_or_equal: Checked.Boolean.var}

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
             equal (Cvar1.pack x1) (Cvar1.pack x2) ))
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
             Boolean.Unsafe.of_cvar r ))
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

  let%snarkydef_ if_ (b : Boolean.var) ~(typ : ('var, _) Typ.t) ~(then_ : 'var)
      ~(else_ : 'var) =
    let then_ = unsafe_read_cvars typ then_ in
    let else_ = unsafe_read_cvars typ else_ in
    let%map res =
      all
        (Core_kernel.List.map2_exn then_ else_ ~f:(fun then_ else_ ->
             if_ b ~then_ ~else_ ))
    in
    let res = ref res in
    let ret =
      (* Run the typ's allocator, providing the values from the Cvar.t list
         [res].
      *)
      Typ_monads0.Alloc.run typ.alloc (fun () ->
          match !res with
          | hd :: tl ->
              res := tl ;
              hd
          | _ ->
              assert false )
    in
    assert (!res = []) ;
    ret

  module Proof_system = struct
    open Run.Proof_system

    type ('a, 's, 'inputs) t = (('a, 's) Checked.t, 'inputs, 's) proof_system

    let create ?proving_key ?verification_key ?proving_key_path
        ?verification_key_path ?keys_with_hashes ?handlers ?reduce
        ~public_input checked =
      create ~reduce_to_prover:Runner.reduce_to_prover ?proving_key
        ?verification_key ?proving_key_path ?verification_key_path
        ?keys_with_hashes ?handlers ?reduce ~public_input checked

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
      verify ~run:Checked.run ~public_input ?verification_key ?message
        proof_system

    let generate_witness ~public_input ?handlers ?reduce (proof_system : _ t) =
      generate_witness ~run:Checked.run ~public_input ?handlers ?reduce
        proof_system
  end

  module Perform = struct
    type ('a, 's, 't) t =
      't -> 's Checked.run_state -> 's Checked.run_state * 'a

    let generate_keypair ~run ~exposing k =
      Run.generate_keypair ~run ~exposing k

    let prove ~run ?message key t k s = Run.prove ~run ?message key t s k

    let verify = Run.verify

    let generate_witness ~run t k s = Run.generate_witness ~run t s k

    let generate_witness_conv ~run ~f t k s =
      Run.generate_witness_conv ~run ~f t s k

    let constraint_system = Run.constraint_system

    let run_unchecked = run_unchecked

    let run_and_check = run_and_check

    let check = check
  end

  let generate_keypair ~exposing k =
    Run.generate_keypair ~run:Checked.run ~exposing k

  let conv f = Run.conv (fun x _ -> f x)

  let conv_never_use = Run.conv_never_use

  let prove ?message key t s k = Run.prove ~run:Checked.run ?message key t s k

  let generate_auxiliary_input t s k =
    Run.generate_auxiliary_input ~run:Checked.run t s k

  let verify = Run.verify

  let generate_public_input = Run.generate_public_input

  let generate_witness t s k = Run.generate_witness ~run:Checked.run t s k

  let generate_witness_conv ~f t s k =
    Run.generate_witness_conv ~run:Checked.run ~f t s k

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

  module As_prover0 =
    As_prover.Make_extended (struct
        type field = Backend_extended.Field.t
      end)
      (Checked)
      (As_prover.Make (Checked) (As_prover0))

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
      (As_prover0)
      (Runner0)

  include Basic
  module Number = Number.Make (Basic)
  module Enumerable = Enumerable.Make (Basic)
end

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

  let error_on_counter = ref None

  let throw_on_id id = error_on_counter := Some id

  module Make_basic
      (Backend : Backend_intf.S) (Prover_state : sig
          type t
      end) =
  struct
    module Snark = Make (Backend)
    open Run_state
    open Snark

    type prover_state = Prover_state.t

    let set_constraint_logger = set_constraint_logger

    let clear_constraint_logger = clear_constraint_logger

    let this_functor_id =
      incr functor_counter ;
      let id = !functor_counter in
      match !error_on_counter with
      | Some id' when Int.equal id id' ->
          failwithf
            "Attempted to create Snarky.Snark.Run.Make functor with ID %i" id
            ()
      | _ ->
          id

    let state =
      ref
        { system= None
        ; input= field_vec ()
        ; aux= field_vec ()
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
      if not (is_active_functor_id this_functor_id) then
        failwithf
          "Could not run this function.\n\n\
           Hint: The module used to create this function had internal ID %i, \
           but the module used to run it had internal ID %i. The same \
           instance of Snarky.Snark.Run.Make must be used for both."
          this_functor_id (active_functor_id ()) ()
      else if not !state.is_running then
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

      module Internal = struct
        include Internal

        (* Warning: Don't try this at home! *)
        let fn (typ1 : ('var1, 'value1) t) (typ2 : ('var2, 'value2) t) :
            ('var1 -> 'var2, 'value1 -> 'value2) t =
          { store=
              (fun f ->
                (* NOTE: We don't do any storing here; the [exists] call below
                         sets up new variables and constraints on each function
                         call, ensuring that the return values are distinct in
                         the constraint system.
                *)
                Store.return (fun a ->
                    run
                      (Checked_S.exists typ2
                         ~compute:As_prover.(map ~f (read typ1 a))) ) )
          ; read=
              (fun f ->
                Read.return (fun a ->
                    (* Sanity check: The read monad should only be evaluated as
                       the prover.
                    *)
                    assert !(!state.as_prover) ;
                    let ret = Stdlib.ref None in
                    run
                      Checked_S.(
                        (* NOTE: This [exists] is safe: no variables are
                                 allocated or constraints added in prover mode.
                        *)
                        let%bind a =
                          exists typ1 ~compute:(As_prover.return a)
                        in
                        let%map () =
                          as_prover
                            As_prover.(
                              let%map x = read typ2 (f a) in
                              ret := Some x)
                        in
                        match !ret with
                        | Some ret ->
                            ret
                        | None ->
                            (* In prover mode, this can't happen. *)
                            assert false) ) )
          ; alloc=
              (* NOTE: We don't do any allocations here; the [exists] call
                       below sets up new variables and constraints on each
                       function call, ensuring that the return values are
                       distinct in the constraint system.
              *)
              Alloc.return (fun _ -> run (exists typ2))
          ; check=
              (fun _ ->
                (* NOTE: There are no variables allocated, so there is nothing to
                       check here. The relevant checks are done in the [exists]
                       call in [store]/[alloc] above, once for each function
                       call.
                *)
                Checked.return () ) }
      end

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

    module Proof_inputs = Proof_inputs
    module Proof = Proof

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

      module Ref = struct
        type 'a t = 'a As_prover.Ref.t

        let create f = run As_prover.(Ref.create (map (return ()) ~f))

        let get r = eval_as_prover (As_prover.Ref.get r)

        let set r x = eval_as_prover (As_prover.Ref.set r x)
      end

      let run_prover f _tbl s =
        let old = !(!state.as_prover) in
        !state.as_prover := true ;
        state := Run_state.set_prover_state (Some s) !state ;
        let a = f () in
        let s' = Option.value_exn !state.prover_state in
        !state.as_prover := old ;
        (s', a)

      let with_lens lens as_prover =
        eval_as_prover (As_prover.with_lens lens as_prover)
    end

    module Handle = struct
      type ('var, 'value) t = ('var, 'value) Handle.t

      let value handle () = As_prover.eval_as_prover (Handle.value handle)

      let var = Handle.var
    end

    module Proof_system = struct
      open Run.Proof_system

      type ('a, 'public_input) t =
        (unit -> 'a, 'public_input, prover_state) proof_system

      let create ?proving_key ?verification_key ?proving_key_path
          ?verification_key_path ?keys_with_hashes ?handlers ~public_input
          checked =
        create
          ~reduce_to_prover:(fun _i f -> f)
          ?proving_key ?verification_key ?proving_key_path
          ?verification_key_path ?keys_with_hashes ?handlers ~public_input
          checked

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

      let run = as_stateful

      let constraint_system (proof_system : _ t) =
        mark_active ~f:(fun () -> constraint_system ~run proof_system)

      let digest (proof_system : _ t) =
        mark_active ~f:(fun () -> digest ~run proof_system)

      let generate_keypair (proof_system : _ t) =
        mark_active ~f:(fun () -> generate_keypair ~run proof_system)

      let run_unchecked ~public_input ?handlers (proof_system : _ t) s =
        mark_active ~f:(fun () ->
            run_unchecked ~run ~public_input ?handlers proof_system
              (fun a _ s -> (s, a))
              s )

      let run_checked ~public_input ?handlers (proof_system : _ t) s =
        mark_active ~f:(fun () ->
            Or_error.map
              (run_checked' ~run ~public_input ?handlers proof_system s)
              ~f:(fun (s, x, _state) -> (s, x)) )

      let check ~public_input ?handlers (proof_system : _ t) s =
        mark_active ~f:(fun () ->
            Or_error.map ~f:(Fn.const ())
              (run_checked' ~run ~public_input ?handlers proof_system s) )

      let prove ~public_input ?proving_key ?handlers ?message
          (proof_system : _ t) s =
        mark_active ~f:(fun () ->
            prove ~run ~public_input ?proving_key ?handlers ?message
              proof_system s )

      let verify ~public_input ?verification_key ?message (proof_system : _ t)
          =
        verify ~run ~public_input ?verification_key ?message proof_system

      let generate_witness ~public_input ?handlers (proof_system : _ t) s =
        mark_active ~f:(fun () ->
            generate_witness ~run ~public_input ?handlers proof_system s )
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

    let handle_as_prover x h =
      let h = h () in
      handle x h

    let if_ b ~typ ~then_ ~else_ = run (if_ b ~typ ~then_ ~else_)

    let with_label lbl x =
      let {stack; log_constraint; _} = !state in
      state := {!state with stack= lbl :: stack} ;
      Option.iter log_constraint ~f:(fun f ->
          f ~at_label_boundary:(`Start, lbl) [] ) ;
      let a = x () in
      Option.iter log_constraint ~f:(fun f ->
          f ~at_label_boundary:(`End, lbl) [] ) ;
      state := {!state with stack} ;
      a

    let make_checked x = Types.Checked.Direct (as_stateful x, fun x -> Pure x)

    let rec inject_wrapper : type r_var r_value k_var k_value.
           (r_var, r_value, k_var, k_value) Data_spec.t
        -> f:(r_var -> r_var)
        -> k_var
        -> k_var =
     fun spec ~f ->
      match spec with
      | [] ->
          fun x -> f x
      | _ :: spec ->
          fun x a -> inject_wrapper spec ~f (x a)

    let constraint_system ~exposing x =
      let x =
        inject_wrapper exposing x ~f:(fun x () -> Proof_system.mark_active ~f:x)
      in
      Perform.constraint_system ~run:as_stateful ~exposing x

    let generate_keypair ~exposing x =
      let x =
        inject_wrapper exposing x ~f:(fun x () -> Proof_system.mark_active ~f:x)
      in
      Perform.generate_keypair ~run:as_stateful ~exposing x

    let prove ?message pk spec x =
      let x =
        inject_wrapper spec x ~f:(fun x () -> Proof_system.mark_active ~f:x)
      in
      Perform.prove ~run:as_stateful ?message pk spec x

    let verify ?message pf vk spec = verify ?message pf vk spec

    let generate_public_input = generate_public_input

    let generate_witness spec x =
      let x =
        inject_wrapper spec x ~f:(fun x () -> Proof_system.mark_active ~f:x)
      in
      Perform.generate_witness ~run:as_stateful spec x

    let generate_witness_conv ~f spec x =
      let x =
        inject_wrapper spec x ~f:(fun x () -> Proof_system.mark_active ~f:x)
      in
      Perform.generate_witness_conv ~run:as_stateful ~f spec x

    let run_unchecked x =
      Perform.run_unchecked ~run:as_stateful (fun () ->
          Proof_system.mark_active ~f:x )

    let run_and_check x =
      let res =
        Perform.run_and_check ~run:as_stateful (fun () ->
            Proof_system.mark_active ~f:(fun () ->
                let prover_block = x () in
                !state.as_prover := true ;
                As_prover.run_prover prover_block ) )
      in
      !state.as_prover := true ;
      res

    let check x s = Perform.check ~run:as_stateful x s

    let constraint_count ?(weight = Core_kernel.List.length) ?log x =
      let count = ref 0 in
      let log_constraint ?at_label_boundary c =
        ( match at_label_boundary with
        | None ->
            ()
        | Some (pos, lab) ->
            Option.iter log ~f:(fun f ->
                f ?start:(Some (pos = `Start)) lab !count ) ) ;
        count := !count + weight c
      in
      (* TODO(mrmr1993): Enable label-level logging for the imperative API. *)
      ignore log ;
      let old = !state in
      state :=
        Runner.State.make ~num_inputs:0 ~input:Vector.null ~aux:Vector.null
          ~next_auxiliary:(ref 1) ~eval_constraints:false None ;
      state := {!state with log_constraint= Some log_constraint} ;
      ignore (Proof_system.mark_active ~f:x) ;
      state := old ;
      !count

    module Internal_Basic = Snark

    let run_checked = run
  end

  module Make
      (Backend : Backend_intf.S) (Prover_state : sig
          type t
      end) =
  struct
    module Basic = Make_basic (Backend) (Prover_state)
    include Basic
    module Number = Number.Run.Make (Basic)
    module Enumerable = Enumerable.Run.Make (Basic)
  end
end

type 'field m = (module Snark_intf.Run with type field = 'field)

type ('prover_state, 'field) m' =
  (module Snark_intf.Run
     with type field = 'field
      and type prover_state = 'prover_state)

let make (type field)
    (module Backend : Backend_intf.S with type Field.t = field) :
    (unit, field) m' =
  (module Run.Make (Backend) (Unit))

let make' (type field prover_state)
    (module Backend : Backend_intf.S with type Field.t = field) :
    (prover_state, field) m' =
  ( module Run.Make
             (Backend)
             (struct
               type t = prover_state
             end) )

let ignore_state (type prover_state field)
    ((module M) : (prover_state, field) m') : field m =
  (module M)
