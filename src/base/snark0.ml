open Core_kernel
module Bignum_bigint = Bigint

exception Runtime_error of string list * exn * string

module Runner = Checked_runner

let set_eval_constraints b = Runner.eval_constraints := b

module Make_basic
    (Backend : Backend_extended.S)
    (Checked : Checked_intf.Extended
                 with type field = Backend.Field.t
                  and type field_var = Backend.Cvar.t
                  and type run_state = Backend.Run_state.t)
    (As_prover : As_prover0.Extended
                   with type field := Backend.Field.t
                    and type field_var = Backend.Cvar.t)
    (Ref : As_prover_ref.S
             with module Types := Checked.Types
              and type 'f field := Backend.Field.t
              and type 'f field_var := Backend.Cvar.t
              and type ('a, 'run_state) checked := 'a Checked.t)
    (Runner : Runner.S
                with module Types := Checked.Types
                with type field := Backend.Field.t
                 and type field_vector := Backend.Field.Vector.t
                 and type cvar := Backend.Cvar.t
                 and type constr := Backend.Constraint.t option
                 and type r1cs := Backend.Constraint_system.t
                 and type run_state := Backend.Run_state.t) =
struct
  open Backend
  module Checked_S = Checked_intf.Unextend (Checked)
  include Runners.Make (Backend) (Checked) (As_prover) (Runner)
  module Bigint = Bigint
  module Field0 = Field
  module Constraint = Constraint
  module Run_state = Run_state

  type field_var = Cvar.t

  type run_state = Run_state.t

  module Handler = struct
    type t = Request.request -> Request.response
  end

  module Typ = struct
    include Types.Typ.T
    module T = Typ.Make (Checked_S)
    include T.T

    type ('var, 'value) t = ('var, 'value, Field.t, Cvar.t, Run_state.t) T.t

    let unit : (unit, unit) t = unit ()

    let field : (Cvar.t, Field.t) t = field ()
  end

  module As_prover = struct
    include As_prover

    type 'a as_prover = 'a t

    module Ref = Ref
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
           and type field := field
           and type field_var := Cvar.t
           and type run_state := Run_state.t )

    let perform req = request_witness Typ.unit req

    module Runner = Runner
    include Utils.Make (Backend) (Checked) (Typ) (As_prover) (Runner)

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
      let linear_combination terms =
        List.fold terms ~init:(Cvar.constant Field.zero) ~f:(fun acc (c, t) ->
            Cvar.add acc (Cvar.scale t c) )
      in
      linear_combination ts

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

    let project (vars : Checked.Boolean.var list) : t =
      let two = Field.of_int 2 in
      let rec go res = function
        | [] ->
            res
        | v :: vs ->
            go Cvar.(add v (scale res two)) vs
      in
      match List.rev (vars :> Cvar.t list) with
      | [] ->
          Cvar.constant Field.zero
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
        match Cvar.to_constant x with
        | Some x ->
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
        let lt ~bit_length (x : Cvar.t) (y : Cvar.t) =
          match (Cvar.to_constant x, Cvar.to_constant y) with
          | Some x, Some y ->
              assert (Field.compare x y < 0) ;
              Checked.return ()
          | _ ->
              let open Checked in
              let open Let_syntax in
              let%bind { less; _ } = compare ~bit_length x y in
              Boolean.Assert.is_true less

        let lte ~bit_length (x : Cvar.t) (y : Cvar.t) =
          match (Cvar.to_constant x, Cvar.to_constant y) with
          | Some x, Some y ->
              assert (Field.compare x y <= 0) ;
              Checked.return ()
          | _ ->
              let open Checked in
              let open Let_syntax in
              let%bind { less_or_equal; _ } = compare ~bit_length x y in
              Boolean.Assert.is_true less_or_equal

        let gt ~bit_length x y = lt ~bit_length y x

        let gte ~bit_length x y = lte ~bit_length y x

        let non_zero (v : Cvar.t) =
          match Cvar.to_constant v with
          | Some v ->
              if Field.(equal zero v) then
                failwithf "assert_non_zero: failed on constant %s"
                  (Field.to_string v) () ;
              Checked.return ()
          | _ ->
              Checked.assert_non_zero v

        let equal x y = Checked.assert_equal ~label:"Checked.Assert.equal" x y

        let not_equal (x : t) (y : t) =
          match (Cvar.to_constant x, Cvar.to_constant y) with
          | Some x, Some y ->
              if Field.(equal x y) then
                failwithf "not_equal: failed on constants %s and %s"
                  (Field.to_string x) (Field.to_string y) () ;
              Checked.return ()
          | _, _ ->
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
                     (List.map ~f:Checked.(constant Boolean.typ) x) )
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

  module Constraint_system = Constraint_system
end

(** The main functor for the monadic interface. 
    See [Run.Make] for the same thing but for the imperative interface. *)
module Make (Backend : Backend_intf.S) = struct
  module Backend_extended = Backend_extended.Make (Backend)
  module Runner0 = Runner.Make (Backend_extended)
  module Checked_runner = Runner0.Checked_runner
  module Checked1 =
    Checked.Make (Backend_extended) (Checked_runner) (As_prover0)

  module Field_T = struct
    type field = Backend_extended.Field.t

    type field_var = Backend_extended.Cvar.t
  end

  module As_prover_ext = As_prover0.Make_extended (Field_T) (As_prover0)

  module Ref :
    As_prover_ref.S
      with module Types = Checked1.Types
       and type ('a, 'run_state) checked := ('a, 'run_state) Checked1.t
       and type 'f field := Backend_extended.Field.t
       and type 'f field_var := Backend_extended.Cvar.t =
    As_prover_ref.Make (Backend_extended.Run_state) (Checked1) (As_prover0)

  module Checked_for_basic = struct
    include (
      Checked1 :
        Checked_intf.S
          with module Types = Checked1.Types
          with type ('a, 'run_state) t := ('a, 'run_state) Checked1.t
           and type 'f field := Backend_extended.Field.t
           and type 'f field_var := Backend_extended.Cvar.t
           and type run_state := Backend_extended.Run_state.t )

    type field = Backend_extended.Field.t

    type field_var = Backend_extended.Cvar.t

    type run_state = Backend_extended.Run_state.t

    type 'a t = ('a, Backend_extended.Run_state.t) Types.Checked.t

    let run = Runner0.run
  end

  module Basic =
    Make_basic (Backend_extended) (Checked_for_basic) (As_prover_ext) (Ref)
      (Runner0)
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
    open Snark

    let set_constraint_logger = set_constraint_logger

    let clear_constraint_logger = clear_constraint_logger

    let this_functor_id = incr functor_counter ; !functor_counter

    type run_state = Run_state.t

    let state =
      ref
        (Run_state.make ~system:false ~eval_constraints:false ~num_inputs:0
           ~stack:[] ~is_running:false () )

    let in_prover () : bool = Run_state.has_witness !state

    let in_checked_computation () : bool =
      is_active_functor_id this_functor_id && Run_state.is_running !state

    let run (checked : _ Checked.t) =
      match checked with
      | Pure a ->
          a
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

    let make_checked (type a) (f : unit -> a) : _ Checked.t =
      let g : Run_state.t -> Run_state.t * a = as_stateful f in
      Function g

    module Constraint_system = Snark.Constraint_system

    type field = Snark.field

    type field_var = Snark.field_var

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
    end

    let constant (Typ typ : _ Typ.t) x =
      let fields, aux = typ.value_to_fields x in
      let field_vars = Core_kernel.Array.map ~f:Cvar.constant fields in
      typ.var_of_fields (field_vars, aux)

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

      let constant = constant

      let to_constant = to_constant

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
        type 'a t = 'a As_prover_ref.t

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

    (** Caches the global [state] before running [f]. 
        It is expected that [f] will reset the global state for its own use only, 
        hence why we need to reset it after running [f].*)
    let finalize_is_running f =
      let cached_state = !state in
      let x =
        match f () with
        | exception e ->
            (* Warning: it is important to clean the global state before reraising the exception.
               Imagine if a user of snarky catches exceptions instead of letting the program panic,
                then the next usage of snarky might be messed up. *)
            state := cached_state ;
            raise e
        | x ->
            x
      in
      state := cached_state ;
      x

    let constraint_system ~input_typ ~return_typ x : Constraint_system.t =
      finalize_is_running (fun () ->
          let x = inject_wrapper x ~f:(fun x () -> mark_active ~f:x) in
          Perform.constraint_system ~run:as_stateful ~input_typ ~return_typ x )

    let generate_public_input t x : As_prover.Vector.t =
      finalize_is_running (fun () -> generate_public_input t x)

    let generate_witness ~input_typ ~return_typ x a : Proof_inputs.t =
      finalize_is_running (fun () ->
          let x = inject_wrapper x ~f:(fun x () -> mark_active ~f:x) in
          Perform.generate_witness ~run:as_stateful ~input_typ ~return_typ x a )

    let generate_witness_conv (type out)
        ~(f : Proof_inputs.t -> 'r_value -> out) ~input_typ ~return_typ x input
        : out =
      finalize_is_running (fun () ->
          let x = inject_wrapper x ~f:(fun x () -> mark_active ~f:x) in
          Perform.generate_witness_conv ~run:as_stateful ~f ~input_typ
            ~return_typ x input )

    let run_unchecked x =
      finalize_is_running (fun () ->
          Perform.run_unchecked ~run:as_stateful (fun () -> mark_active ~f:x) )

    let run_and_check (type a) (x : unit -> (unit -> a) As_prover.t) :
        a Or_error.t =
      finalize_is_running (fun () ->
          let res =
            Perform.run_and_check ~run:as_stateful (fun () ->
                mark_active ~f:(fun () ->
                    let prover_block = x () in
                    Run_state.set_as_prover !state true ;
                    As_prover.run_prover prover_block ) )
          in
          Run_state.set_as_prover !state true ;
          res )

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

      let run_and_check (type a) (x : unit -> (unit -> a) As_prover.t M.t) :
          a Or_error.t M.t =
        finalize_is_running (fun () ->
            let mark_active = mark_active_deferred ~map in
            let res =
              run_and_check ~run:as_stateful (fun () ->
                  mark_active ~f:(fun () ->
                      map (x ()) ~f:(fun prover_block ->
                          Run_state.set_as_prover !state true ;
                          As_prover.run_prover prover_block ) ) )
            in
            Run_state.set_as_prover !state true ;
            res )
    end

    let check x : unit Or_error.t =
      finalize_is_running (fun () -> Perform.check ~run:as_stateful x)

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
        Runner.State.make ~num_inputs:0 ~system:false ~eval_constraints:false
          ~log_constraint () ;
      ignore (mark_active ~f:x) ;
      state := old ;
      !count

    module Internal_Basic = struct
      include Snark

      type state = Snark.Run_state.t
    end

    let run_checked = run
  end

  module Make (Backend : Backend_intf.S) = struct
    module Basic = Make_basic (Backend)
    include Basic
    module Number = Number.Run.Make (Basic)
    module Enumerable = Enumerable.Run.Make (Basic)
  end
end

type ('field, 'field_var) m =
  (module Snark_intf.Run
     with type field = 'field
      and type field_var = 'field_var )

let make (type field field_var)
    (module Backend : Backend_intf.S
      with type Field.t = field
       and type Cvar.t = field_var ) : (field, field_var) m =
  (module Run.Make (Backend))
