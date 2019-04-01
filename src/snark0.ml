module Bignum_bigint = Bigint
open Core_kernel

let () = Camlsnark_c.linkme

let eval_constraints = ref false

let set_eval_constraints b = eval_constraints := b

module Make_basic (Backend : Backend_intf.S) = struct
  open Backend

  type field = Field.t

  module Bigint = struct
    include Bigint.R

    let of_bignum_bigint n = of_decimal_string (Bignum_bigint.to_string n)

    let to_bignum_bigint n =
      let rec go i two_to_the_i acc =
        if i = Field.size_in_bits then acc
        else
          let acc' =
            if test_bit n i then Bignum_bigint.(acc + two_to_the_i) else acc
          in
          go (i + 1) Bignum_bigint.(two_to_the_i + two_to_the_i) acc'
      in
      go 0 Bignum_bigint.one Bignum_bigint.zero
  end

  module Proof = Proof

  module Verification_key = struct
    include Verification_key
    include Binable.Of_stringable (Verification_key)
  end

  module Proving_key = struct
    include Proving_key
  end

  module Keypair = struct
    type t = {pk: Proving_key.t; vk: Verification_key.t}
    [@@deriving fields, bin_io]

    let create = Fields.create

    let of_backend_keypair kp = {pk= Keypair.pk kp; vk= Keypair.vk kp}

    let generate = Fn.compose of_backend_keypair Backend.Keypair.create
  end

  module Var = struct
    module T = struct
      include Backend.Var

      let compare x y = Int.compare (index x) (index y)

      let t_of_sexp _ = failwith "Var.t_of_sexp"

      let sexp_of_t v =
        Sexp.(List [Atom "var"; Atom (Int.to_string (index v))])
    end

    include T
    include Comparable.Make (T)
  end

  module Field0 = struct
    include Field

    let size = Bigint.to_bignum_bigint Backend.field_size

    let inv x = if equal x zero then failwith "Field.inv: zero" else inv x

    (* TODO: Optimize *)
    let div x y = mul x (inv y)

    let negate x = sub zero x

    let unpack x =
      let n = Bigint.of_field x in
      List.init size_in_bits ~f:(fun i -> Bigint.test_bit n i)

    let project_reference =
      let rec go x acc = function
        | [] -> acc
        | b :: bs -> go (Field.add x x) (if b then Field.add acc x else acc) bs
      in
      fun bs -> go Field.one Field.zero bs

    let project bs =
      (* todo: 32-bit and ARM support. basically this code needs to always match the loop in the C++ of_data implementation. *)
      assert (Sys.word_size = 64 && not Sys.big_endian) ;
      let module R = Backend.Bigint.R in
      let chunks_of n xs =
        List.groupi ~break:(fun i _ _ -> Int.equal (i mod n) 0) xs
      in
      let chunks64 = chunks_of 64 bs in
      let z = Char.of_int_exn 0 in
      let arr = Bigstring.init (8 * R.length_in_bytes) ~f:(fun _ -> z) in
      List.(
        iteri ~f:(fun i elt ->
            Bigstring.set_int64_t_le arr ~pos:(i * 8)
              Int64.(
                foldi ~init:zero
                  ~f:(fun i acc el ->
                    acc + if el then shift_left one i else zero )
                  elt) ))
        chunks64 ;
      Backend.Bigint.R.(of_data arr ~bitcount:(List.length bs) |> to_field)

    let compare t1 t2 = Bigint.(compare (of_field t1) (of_field t2))

    let hash_fold_t s x =
      Bignum_bigint.hash_fold_t s Bigint.(to_bignum_bigint (of_field x))

    let hash = Hash.of_fold hash_fold_t

    let to_bignum_bigint = Fn.compose Bigint.to_bignum_bigint Bigint.of_field

    let of_bignum_bigint = Fn.compose Bigint.to_field Bigint.of_bignum_bigint

    let sexp_of_t = Fn.compose Bignum_bigint.sexp_of_t to_bignum_bigint

    let t_of_sexp = Fn.compose of_bignum_bigint Bignum_bigint.t_of_sexp

    let to_string = Fn.compose Bignum_bigint.to_string to_bignum_bigint

    let of_string = Fn.compose of_bignum_bigint Bignum_bigint.of_string

    let%test_unit "project correctness" =
      Quickcheck.test
        Quickcheck.Generator.(
          small_positive_int >>= fun x -> list_with_length x bool)
        ~f:(fun bs ->
          [%test_eq: string]
            (project bs |> to_string)
            (project_reference bs |> to_string) )

    module Infix = struct
      let ( + ) = add

      let ( * ) = mul

      let ( - ) = sub

      let ( / ) = div
    end
  end

  module Cvar = struct
    include Cvar.Make (Field0) (Var)

    let var_indices t =
      let _, terms = to_constant_and_terms t in
      List.map ~f:(fun (_, v) -> Var.index v) terms

    let to_constant : t -> Field0.t option = function
      | Constant x -> Some x
      | _ -> None
  end

  module Linear_combination = struct
    let of_constant = function
      | None -> Linear_combination.create ()
      | Some c -> Linear_combination.of_field c

    let of_var (cv : Cvar.t) =
      let constant, terms = Cvar.to_constant_and_terms cv in
      let t = of_constant constant in
      List.iter terms ~f:(fun (c, v) -> Linear_combination.add_term t c v) ;
      t

    let of_field = Backend.Linear_combination.of_field

    let zero = of_field Field.zero
  end

  module Constraint = struct
    open Constraint
    include Constraint.T

    type basic = Cvar.t Constraint.basic

    type 'k with_constraint_args = ?label:string -> 'k

    type t = Cvar.t Constraint.t [@@deriving sexp]

    let basic_to_r1cs_constraint : basic -> R1CS_constraint.t =
      let of_var = Linear_combination.of_var in
      function
      | Boolean v ->
          let lc = of_var v in
          let constr = R1CS_constraint.create lc lc lc in
          R1CS_constraint.set_is_square constr true ;
          constr
      | Equal (v1, v2) ->
          (* 0 * 0 = (v1 - v2) *)
          let constr =
            R1CS_constraint.create Linear_combination.zero
              Linear_combination.zero
              (of_var (Cvar.sub v1 v2))
          in
          R1CS_constraint.set_is_square constr true ;
          constr
      | Square (a, c) ->
          let a = of_var a in
          let constr = R1CS_constraint.create a a (of_var c) in
          R1CS_constraint.set_is_square constr true ;
          constr
      | R1CS (a, b, c) ->
          let constr =
            R1CS_constraint.create (of_var a) (of_var b) (of_var c)
          in
          R1CS_constraint.set_is_square constr false ;
          constr

    let stack_to_string = String.concat ~sep:"\n"

    let add ~stack (t : t) system =
      List.iter t ~f:(fun {basic; annotation} ->
          let label = Option.value annotation ~default:"<unknown>" in
          let c = basic_to_r1cs_constraint basic in
          R1CS_constraint_system.add_constraint_with_annotation system c
            (stack_to_string (label :: stack)) )

    let eval_basic t get_value =
      match t with
      | Boolean v ->
          let x = get_value v in
          Field.(equal x zero || equal x one)
      | Equal (v1, v2) -> Field.equal (get_value v1) (get_value v2)
      | R1CS (v1, v2, v3) ->
          Field.(equal (mul (get_value v1) (get_value v2)) (get_value v3))
      | Square (a, c) -> Field.equal (Field.square (get_value a)) (get_value c)

    let eval t get_value =
      List.for_all t ~f:(fun {basic; _} -> eval_basic basic get_value)
  end

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

  module Checked0 = struct
    type 'prover_state run_state = ('prover_state, Field.t) Types.Run_state.t

    type ('a, 's) t = ('a, 's, Field.t) Checked.t

    include Checked.T
  end

  module Typ = struct
    open Types.Typ
    include Typ_monads
    include Typ.T

    type ('var, 'value) t = ('var, 'value, Field.t) Types.Typ.t

    type ('var, 'value) typ = ('var, 'value) t

    module Data_spec = struct
      (** TODO: This exists only to bring the constructors into scope in this
                module. Upstream a patch to permit different arities in types
                with a different arity type manifest.
      *)
      type ('r_var, 'r_value, 'k_var, 'k_value, 'field) data_spec =
                                                                   ( 'r_var
                                                                   , 'r_value
                                                                   , 'k_var
                                                                   , 'k_value
                                                                   , 'field )
                                                                   Typ
                                                                   .Data_spec
                                                                   .t =
        | ( :: ) :
            ('var, 'value, 'f) Types.Typ.t
            * ('r_var, 'r_value, 'k_var, 'k_value, 'f) data_spec
            -> ( 'r_var
               , 'r_value
               , 'var -> 'k_var
               , 'value -> 'k_value
               , 'f )
               data_spec
        | [] : ('r_var, 'r_value, 'r_var, 'r_value, 'f) data_spec

      type ('r_var, 'r_value, 'k_var, 'k_value) t =
        ('r_var, 'r_value, 'k_var, 'k_value, field) Typ.Data_spec.t

      let size t =
        let rec go : type r_var r_value k_var k_value.
            int -> (r_var, r_value, k_var, k_value) t -> int =
         fun acc t ->
          match t with
          | [] -> acc
          | {alloc; _} :: t' -> go (acc + Alloc.size alloc) t'
        in
        go 0 t
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
                 (Checked0)
                 (struct
                   type t = unit
                 end)) in
          M.f
        in
        let read var = traverse_read var ~f:read in
        let store value = traverse_store value ~f:store in
        let alloc = traverse_alloc template ~f:(fun () -> alloc) in
        let check t = Checked0.map (traverse_checked t ~f:check) ~f:ignore in
        {read; store; alloc; check}
    end
  end

  module As_prover = struct
    include As_prover.Make (struct
      type field = Field.t
    end)

    type ('a, 'prover_state) as_prover = ('a, 'prover_state) t
  end

  module Handle = Handle

  module Checked = struct
    open Types.Checked
    open Types.Run_state
    include Checked0

    let perform req = request_witness Typ.unit req

    module Runner = struct
      type state = unit run_state

      type ('a, 's, 't) run = 't -> 's run_state -> 's run_state * 'a

      let set_prover_state prover_state
          { system
          ; input
          ; aux
          ; eval_constraints
          ; num_inputs
          ; next_auxiliary
          ; prover_state= _
          ; stack
          ; handler
          ; is_running
          ; as_prover
          ; run_special } =
        { system
        ; input
        ; aux
        ; eval_constraints
        ; num_inputs
        ; next_auxiliary
        ; prover_state
        ; stack
        ; handler
        ; is_running
        ; as_prover
        ; run_special }

      let set_handler handler state = {state with handler}

      let get_handler {handler; _} = handler

      let set_stack stack state = {state with stack}

      let get_stack {stack; _} = stack

      let get_value {num_inputs; input; aux; _} : Cvar.t -> Field.t =
        let get_one i =
          if i <= num_inputs then Field.Vector.get input (i - 1)
          else Field.Vector.get aux (i - num_inputs - 1)
        in
        Cvar.eval get_one

      let store_field_elt {next_auxiliary; aux; _} x =
        let v = !next_auxiliary in
        incr next_auxiliary ;
        Field.Vector.emplace_back aux x ;
        Cvar.Unsafe.of_index v

      let alloc_var {next_auxiliary; _} () =
        let v = !next_auxiliary in
        incr next_auxiliary ; Cvar.Unsafe.of_index v

      let run_as_prover x state =
        match (x, state.prover_state) with
        | Some x, Some s ->
            let old = !(state.as_prover) in
            state.as_prover := true ;
            let s', y = As_prover.run x (get_value state) s in
            state.as_prover := old ;
            ({state with prover_state= Some s'}, Some y)
        | _, _ -> (state, None)

      (* INVARIANT: run _ s = (s', _) gives
         (s'.prover_state = Some _) iff (s.prover_state = Some _) *)
      let rec run : type a s. (a, s) t -> s run_state -> s run_state * a =
       fun t s ->
        match t with
        | As_prover (x, k) ->
            let s', (_ : unit option) = run_as_prover (Some x) s in
            run k s'
        | _ when !(s.as_prover) ->
            failwith
              "Can't run checked code as the prover: the verifier's \
               constraint system will not match."
        | Pure x -> (s, x)
        | Direct (d, k) ->
            let s, y = d s in
            run (k y) s
        | Reduced (t, d, res, k) ->
            let s, y =
              if Option.is_some s.prover_state && Option.is_none s.system then
                (d s, res)
              else run t s
            in
            run (k y) s
        | With_label (lab, t, k) ->
            let {stack; _} = s in
            let s', y = run t {s with stack= lab :: stack} in
            run (k y) {s' with stack}
        | Add_constraint (c, t) ->
            if s.eval_constraints && not (Constraint.eval c (get_value s)) then (
              Constraint.print c (get_value s) ;
              failwithf "Constraint unsatisfied:\n%s\n%s\n"
                (Constraint.annotation c)
                (Constraint.stack_to_string s.stack)
                () ) ;
            Option.iter s.system ~f:(fun system ->
                Constraint.add ~stack:s.stack c system ) ;
            run t s
        | With_state (p, and_then, t_sub, k) ->
            let s, s_sub = run_as_prover (Some p) s in
            let s_sub, y = run t_sub (set_prover_state s_sub s) in
            let s, (_ : unit option) =
              run_as_prover (Option.map ~f:and_then s_sub.prover_state) s
            in
            run (k y) s
        | With_handler (h, t, k) ->
            let {handler; _} = s in
            let s', y =
              run t {s with handler= Request.Handler.push handler h}
            in
            run (k y) {s' with handler}
        | Clear_handler (t, k) ->
            let {handler; _} = s in
            let s', y = run t {s with handler= Request.Handler.fail} in
            run (k y) {s' with handler}
        | Exists ({store; alloc; check; _}, p, k) -> (
          match s.prover_state with
          | Some ps ->
              let old = !(s.as_prover) in
              s.as_prover := true ;
              let ps, value =
                Provider.run p s.stack (get_value s) ps s.handler
              in
              s.as_prover := old ;
              let var = Typ.Store.run (store value) (store_field_elt s) in
              (* TODO: Push a label onto the stack here *)
              let s, () = run (check var) (set_prover_state (Some ()) s) in
              run
                (k {Handle.var; value= Some value})
                (set_prover_state (Some ps) s)
          | None ->
              let var = Typ.Alloc.run alloc (alloc_var s) in
              (* TODO: Push a label onto the stack here *)
              let s, () = run (check var) (set_prover_state None s) in
              run (k {Handle.var; value= None}) (set_prover_state None s) )
        | Next_auxiliary k -> run (k !(s.next_auxiliary)) s

      let dummy_vector = Field.Vector.create ()

      let fake_state next_auxiliary =
        { system= None
        ; input= dummy_vector
        ; aux= dummy_vector
        ; eval_constraints= false
        ; num_inputs= 0
        ; next_auxiliary
        ; prover_state= None
        ; stack= []
        ; handler= Request.Handler.fail
        ; is_running= true
        ; as_prover= ref false
        ; run_special= None }

      let rec flatten_as_prover : type a s.
          int ref -> (a, s) t -> (s run_state -> s run_state) * a =
       fun next_auxiliary t ->
        match t with
        | As_prover (x, k) ->
            let f, a = flatten_as_prover next_auxiliary k in
            ( (fun s ->
                let s', (_ : unit option) = run_as_prover (Some x) s in
                f s' )
            , a )
        | Pure x -> (Fn.id, x)
        | Direct (d, k) ->
            let _, y = d (fake_state next_auxiliary) in
            let f, a = flatten_as_prover next_auxiliary (k y) in
            ( (fun s ->
                let {prover_state; _} = s in
                let s, _y = d s in
                f (set_prover_state prover_state s) )
            , a )
        | Reduced (t, d, _res, k) ->
            let f, y = flatten_as_prover next_auxiliary t in
            let g, a = flatten_as_prover next_auxiliary (k y) in
            ((fun s -> g (f s)), a)
        | With_label (lab, t, k) ->
            let f, y = flatten_as_prover next_auxiliary t in
            let g, a = flatten_as_prover next_auxiliary (k y) in
            ((fun s -> g (f s)), a)
        | Add_constraint (c, t) -> flatten_as_prover next_auxiliary t
        | With_state (p, and_then, t_sub, k) ->
            let f_sub, y = flatten_as_prover next_auxiliary t_sub in
            let f, a = flatten_as_prover next_auxiliary (k y) in
            ( (fun s ->
                let s, s_sub = run_as_prover (Some p) s in
                let s_sub = f_sub (set_prover_state s_sub s) in
                let s, (_ : unit option) =
                  run_as_prover (Option.map ~f:and_then s_sub.prover_state) s
                in
                f s )
            , a )
        | With_handler (h, t, k) ->
            let f, y = flatten_as_prover next_auxiliary t in
            let g, a = flatten_as_prover next_auxiliary (k y) in
            ( (fun s ->
                let {handler; _} = s in
                let s' = f {s with handler= Request.Handler.push handler h} in
                g {s' with handler} )
            , a )
        | Clear_handler (t, k) ->
            let f, y = flatten_as_prover next_auxiliary t in
            let g, a = flatten_as_prover next_auxiliary (k y) in
            ( (fun s ->
                let {handler; _} = s in
                let s' = f {s with handler= Request.Handler.fail} in
                g {s' with handler} )
            , a )
        | Exists ({store; alloc; check; _}, p, k) ->
            let var =
              Typ.Alloc.run alloc (alloc_var (fake_state next_auxiliary))
            in
            let f, () = flatten_as_prover next_auxiliary (check var) in
            let handle = {Handle.var; value= None} in
            let g, a = flatten_as_prover next_auxiliary (k handle) in
            ( (fun s ->
                let old = !(s.as_prover) in
                s.as_prover := true ;
                let ps, value =
                  Provider.run p s.stack (get_value s)
                    (Option.value_exn s.prover_state)
                    s.handler
                in
                s.as_prover := old ;
                let _var = Typ.Store.run (store value) (store_field_elt s) in
                let s = f (set_prover_state (Some ()) s) in
                handle.value <- Some value ;
                g (set_prover_state (Some ps) s) )
            , a )
        | Next_auxiliary k ->
            flatten_as_prover next_auxiliary (k !next_auxiliary)

      let reduce_to_prover (type a s) next_auxiliary (t : (a, s) t) : (a, s) t
          =
        let f, a = flatten_as_prover next_auxiliary t in
        Reduced (t, f, a, return)

      module State = struct
        let make ~num_inputs ~input ~next_auxiliary ~aux ?system
            ?(eval_constraints = !eval_constraints) ?handler (s0 : 's option) =
          next_auxiliary := 1 + num_inputs ;
          (* We can't evaluate the constraints if we are not computing over a value. *)
          let eval_constraints = eval_constraints && Option.is_some s0 in
          Option.iter system ~f:(fun system ->
              R1CS_constraint_system.set_primary_input_size system num_inputs
          ) ;
          { system
          ; input
          ; aux
          ; eval_constraints
          ; num_inputs
          ; next_auxiliary
          ; prover_state= s0
          ; stack= []
          ; handler= Option.value handler ~default:Request.Handler.fail
          ; is_running= true
          ; as_prover= ref false
          ; run_special= None }
      end
    end

    let rec constraint_count_aux : type a s s1.
           log:(?start:_ -> _)
        -> auxc:_
        -> int
        -> (a, s, _) Types.Checked.t
        -> int * a =
     fun ~log ~auxc count t0 ->
      match t0 with
      | Pure x -> (count, x)
      | Direct (d, k) ->
          let input = Field.Vector.create () in
          let aux = Field.Vector.create () in
          let state =
            Runner.State.make ~num_inputs:0 ~input ~next_auxiliary:auxc ~aux
              None
          in
          let count = ref count in
          let run_special (type a s s1) (x : (a, s, _) Types.Checked.t) =
            let count', a = constraint_count_aux ~log ~auxc !count x in
            count := count' ;
            a
          in
          let state = {state with run_special= Some run_special} in
          let _, x = d state in
          constraint_count_aux ~log ~auxc !count (k x)
      | Reduced (t, _, _, k) ->
          let count, y = constraint_count_aux ~log ~auxc count t in
          constraint_count_aux ~log ~auxc count (k y)
      | As_prover (_x, k) -> constraint_count_aux ~log ~auxc count k
      | Add_constraint (_c, t) -> constraint_count_aux ~log ~auxc (count + 1) t
      | Next_auxiliary k -> constraint_count_aux ~log ~auxc count (k !auxc)
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

    let constraint_count ?(log = fun ?start _ _ -> ()) (t : (_, _) t) : int =
      let next_auxiliary = ref 1 in
      fst (constraint_count_aux ~log ~auxc:next_auxiliary 0 t)

    (* TODO-someday: Add pass to unify variables which have an Equal constraint *)
    let constraint_system ~run ~num_inputs (t : ('a, 's) t) :
        R1CS_constraint_system.t =
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

    let auxiliary_input ~run ~num_inputs ?(handlers = ([] : Handler.t list)) t0
        s0 (input : Field.Vector.t) : Field.Vector.t =
      let next_auxiliary = ref (1 + num_inputs) in
      let aux = Field.Vector.create () in
      let handler =
        List.fold ~init:Request.Handler.fail handlers ~f:(fun handler h ->
            Request.Handler.(push handler (create_single h)) )
      in
      let state =
        Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux ~handler
          (Some s0)
      in
      ignore (run t0 state) ;
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
      | exception e -> Or_error.of_exn e
      | {prover_state= Some s; _}, x -> Ok (s, x, get_value)
      | _ -> failwith "run_and_check': Expected a value from run, got None."

    let run_unchecked ~run t0 s0 =
      let num_inputs = 0 in
      let input = Field.Vector.create () in
      let next_auxiliary = ref 1 in
      let aux = Field.Vector.create () in
      let state =
        Runner.State.make ~num_inputs ~input ~next_auxiliary ~aux (Some s0)
      in
      match run t0 state with
      | {prover_state= Some s; _}, x -> (s, x)
      | _ -> failwith "run_unchecked: Expected a value from run, got None."

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
        let open Cvar.Infix in
        assert_all
          [ r1cs ~label:"equals_1" inv (x - y) (Cvar.constant Field.one - r)
          ; r1cs ~label:"equals_2" r (x - y) (Cvar.constant Field.zero) ]
      in
      Boolean.Unsafe.create r

    let mul ?(label = "Checked.mul") (x : Cvar.t) (y : Cvar.t) =
      match (x, y) with
      | Constant x, Constant y -> return (Cvar.constant (Field.mul x y))
      | Constant x, _ -> return (Cvar.scale y x)
      | _, Constant y -> return (Cvar.scale x y)
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
      | Constant x -> return (Cvar.constant (Field.square x))
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
      | Constant x -> return (Cvar.constant (Field.inv x))
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
          return Cvar.(Infix.((t * b) + (e * (constant Field0.one - b))))
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
            assert_r1cs b Cvar.Infix.(then_ - else_) Cvar.Infix.(r - else_)
          in
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

      let not (x : var) : var =
        create Cvar.Infix.((true_ :> Cvar.t) - (x :> Cvar.t))

      let if_ b ~(then_ : var) ~(else_ : var) =
        Checked0.map ~f:create
          (if_ b ~then_:(then_ :> Cvar.t) ~else_:(else_ :> Cvar.t))

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
          assert_square x_plus_y Cvar.Infix.((Field.of_int 2 * z) + x_plus_y)
        in
        create z

      let ( || ) x y =
        let open Let_syntax in
        let%map both_false = (not x) && not y in
        not both_false

      let any = function
        | [] -> return false_
        | [b1] -> return b1
        | [b1; b2] -> b1 || b2
        | bs ->
            let open Let_syntax in
            let%map all_zero =
              equal (Cvar.sum (bs :> Cvar.t list)) (Cvar.constant Field.zero)
            in
            not all_zero

      let all = function
        | [] -> return true_
        | [b1] -> return b1
        | [b1; b2] -> b1 && b2
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
          assert_ (Constraint.boolean ~label:"boolean-alloc" (v :> Cvar.t))
        in
        {read; store; alloc; check}

      let typ_unchecked : (var, value) Typ.t =
        {typ with check= (fun _ -> return ())}

      let ( lxor ) b1 b2 =
        match (to_constant b1, to_constant b2) with
        | Some b1, Some b2 -> return (var_of_value (b1 <> b2))
        | Some true, None -> return (not b2)
        | None, Some true -> return (not b1)
        | Some false, None -> return b2
        | None, Some false -> return b1
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
              let open Cvar.Infix in
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
          | Not t -> eval t >>| not
          | Var v -> return v
          | And ts -> Checked0.all (List.map ~f:eval ts) >>= all
          | Or ts -> Checked0.all (List.map ~f:eval ts) >>= any

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
      Monad_sequence.List
        (Checked0)
        (struct
          type t = Boolean.var

          include Boolean
        end)
  end

  module Data_spec = Typ.Data_spec

  module Run = struct
    open Types.Run_state
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
        { compute: unit -> 'checked
        ; check_inputs: (unit, 's) Checked.t
        ; provide_inputs:
            Field.Vector.t -> (unit, 'inputs) H_list.t -> Field.Vector.t
        ; num_inputs: int
        ; handler: Request.Handler.t
        ; mutable proving_key: Proving_key.t option
        ; mutable verification_key: Verification_key.t option
        ; proving_key_path: string option
        ; verification_key_path: string option }

      let rec allocate_inputs : type checked r2 k1 k2.
             (unit, 's) Checked.t
          -> int ref
          -> (checked, unit, k1, k2) t
          -> (unit -> k1)
          -> (checked, k2, 's) proof_system =
       fun check_inputs next_input t compute ->
        match t with
        | [] ->
            { compute
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
            let compute () = compute () var in
            let check_inputs =
              let open Checked.Let_syntax in
              let%bind () = check_inputs in
              Checked.with_state (As_prover.return ()) (check var)
            in
            let { compute
                ; check_inputs
                ; provide_inputs
                ; num_inputs
                ; handler
                ; proving_key
                ; verification_key
                ; proving_key_path
                ; verification_key_path } =
              allocate_inputs check_inputs next_input t' compute
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
            ; check_inputs
            ; provide_inputs
            ; num_inputs
            ; handler
            ; proving_key
            ; verification_key
            ; proving_key_path
            ; verification_key_path }

      let create ?proving_key ?verification_key ?proving_key_path
          ?verification_key_path ?(handlers = ([] : Handler.t list))
          ~public_input compute =
        let next_input = ref 1 in
        let proof_system =
          allocate_inputs (Checked.return ()) next_input public_input
            (fun () -> compute )
        in
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

      let run_proof_system ~run ~input ?system ?eval_constraints
          ?(handlers = ([] : Handler.t list)) proof_system s =
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
          Checked.Runner.run proof_system.check_inputs prover_state
        in
        let prover_state, a = run (proof_system.compute ()) prover_state in
        Option.iter prover_state.system ~f:(fun system ->
            let aux_input_size =
              !(prover_state.next_auxiliary) - (1 + num_inputs)
            in
            R1CS_constraint_system.set_auxiliary_input_size system
              aux_input_size ) ;
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

      let run_with_input ~run ~public_input ?system ?eval_constraints ?handlers
          proof_system s =
        let input =
          proof_system.provide_inputs (Field.Vector.create ()) public_input
        in
        let ({prover_state= s; _} as state), a =
          run_proof_system ~run ~input ?system ?eval_constraints ?handlers
            proof_system (Some s)
        in
        match s with
        | Some s -> (s, a, state)
        | None ->
            failwith
              "run_with_input: Expected a value from run_proof_system, got \
               None."

      let run_unchecked ~run ~public_input ?handlers proof_system s =
        let s, a, _ =
          run_with_input ~run ~public_input ?handlers proof_system s
        in
        (s, a)

      let run_checked' ~run ~public_input ?handlers proof_system s =
        let system = R1CS_constraint_system.create () in
        match
          run_with_input ~run ~public_input ~system ~eval_constraints:true
            ?handlers proof_system s
        with
        | exception e -> Or_error.of_exn e
        | s, x, state -> Ok (s, x, state)

      let run_checked ~run ~public_input ?handlers proof_system s =
        Or_error.map (run_checked' ~run ~public_input ?handlers proof_system s)
          ~f:(fun (s, x, state) ->
            let s', x = As_prover.run x (Checked.Runner.get_value state) s in
            (s', x) )

      let check ~run ~public_input ?handlers proof_system s =
        Or_error.map ~f:(Fn.const ())
          (run_checked' ~run ~public_input ?handlers proof_system s)

      let read_proving_key proof_system =
        match proof_system.proving_key_path with
        | Some path -> Some (Proving_key.of_string (In_channel.read_all path))
        | None -> None

      let read_verification_key proof_system =
        match proof_system.verification_key_path with
        | Some path ->
            Some (Verification_key.of_string (In_channel.read_all path))
        | None -> None

      let prove ~run ~public_input ?proving_key ?handlers proof_system s =
        let system = R1CS_constraint_system.create () in
        let _, _, state =
          run_with_input ~run ~public_input ~system ?handlers proof_system s
        in
        let {input; aux; _} = state in
        let proving_key =
          List.find_map_exn
            ~f:(fun f -> f ())
            [ (fun () -> proving_key)
            ; (fun () -> proof_system.proving_key)
            ; (fun () -> read_proving_key proof_system)
            ; (fun () -> Some (generate_keypair ~run proof_system).pk) ]
        in
        Proof.create proving_key ~primary:input ~auxiliary:aux

      let verify ~run ~public_input ?verification_key proof_system proof =
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
        Proof.verify proof verification_key input
    end

    let rec collect_input_constraints : type checked s r2 k1 k2.
        int ref -> (checked, r2, k1, k2) t -> k1 -> (checked, s) Checked.t =
     fun next_input t k ->
      match t with
      | [] -> Checked.return k
      | {alloc; check; _} :: t' ->
          let var = Typ.Alloc.run alloc (alloc_var next_input) in
          let r = collect_input_constraints next_input t' (k var) in
          let open Checked.Let_syntax in
          let%map () = Checked.with_state (As_prover.return ()) (check var)
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
        let state, x = Checked.Runner.run r state in
        run x state
      in
      Checked.constraint_system ~run:run_in_run ~num_inputs:(!next_input - 1) r

    let constraint_system (type a s checked k_var k_val) :
           run:(a, s, checked) Checked.Runner.run
        -> exposing:(checked, _, 'k_var, _) t
        -> 'k_var
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
           Proof.t
        -> Verification_key.t
        -> ('r_var, bool, 'k_var, 'k_value) t
        -> 'k_value =
     fun proof vk t0 ->
      let primary_input = Field.Vector.create () in
      let next_input = ref 1 in
      let store_field_elt = store_field_elt primary_input next_input in
      let rec go : type r_var k_var k_value.
          (r_var, bool, k_var, k_value) t -> k_value =
       fun t ->
        match t with
        | [] -> Proof.verify proof vk primary_input
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
        | [] -> cont0 k primary_input
        | {store; _} :: t' ->
            fun value ->
              let var = Typ.Store.run (store value) store_field_elt in
              go t' (k var)
      in
      go t0 k0

    let prove :
           run:('a, 's, 'checked) Checked.Runner.run
        -> Proving_key.t
        -> ('checked, Proof.t, 'k_var, 'k_value) t
        -> ?handlers:Handler.t list
        -> 's
        -> 'k_var
        -> 'k_value =
     fun ~run key t ?handlers s k ->
      conv
        (fun c primary ->
          let auxiliary =
            Checked.auxiliary_input ~run ?handlers
              ~num_inputs:(Field.Vector.length primary)
              c s primary
          in
          Proof.create key ~primary ~auxiliary )
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

    let reduce_to_prover : type a s r_value.
           ((a, s) Checked.t, Proof.t, 'k_var, 'k_value) t
        -> 'k_var
        -> Proving_key.t
        -> ?handlers:Handler.t list
        -> s
        -> 'k_value =
     fun t0 k0 ->
      let next_input = ref 1 in
      let alloc_var () =
        let v = !next_input in
        incr next_input ; Cvar.Unsafe.of_index v
      in
      let rec go : type k_var k_value.
          ((a, s) Checked.t, Proof.t, k_var, k_value) t -> k_var -> k_var =
       fun t k ->
        match t with
        | [] -> Checked.Runner.reduce_to_prover next_input k
        | {alloc; _} :: t' ->
            let var = Typ.Alloc.run alloc alloc_var in
            let ret = go t' (k var) in
            fun _ -> ret
      in
      let reduced = go t0 k0 in
      fun key ?handlers s ->
        prove ~run:Checked.Runner.run key t0 ?handlers s reduced
  end

  module Cvar1 = struct
    include Cvar

    let project (vars : Checked.Boolean.var list) =
      let rec go c acc = function
        | [] -> List.rev acc
        | v :: vs -> go (Field.add c c) ((c, v) :: acc) vs
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
             Cvar.Infix.(Cvar.constant (two_to_the bit_length) + b - a)
           in
           let%bind alpha = unpack alpha_packed ~length:(bit_length + 1) in
           let prefix, less_or_equal =
             match Core_kernel.List.split_n alpha bit_length with
             | p, [l] -> (p, l)
             | _ -> failwith "compare: Invalid alpha"
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
              | Lit x -> Lit x
              | And (x, And (y, t)) -> And [Lit x; Lit y; of_binary t]
              | Or (x, Or (y, t)) -> Or [Lit x; Lit y; of_binary t]
              | And (x, t) -> And [Lit x; of_binary t]
              | Or (x, t) -> Or [Lit x; of_binary t]

            let rec eval =
              let open Checked.Let_syntax in
              function
              | Lit x -> return x
              | And xs -> Checked.List.map xs ~f:eval >>= Boolean.all
              | Or xs -> Checked.List.map xs ~f:eval >>= Boolean.any
          end
        end in
        let rec lt_binary xs ys : Boolean.var Expr.Binary.t =
          match (xs, ys) with
          | [], [] -> Lit Boolean.false_
          | [_x], [false] -> Lit Boolean.false_
          | [x], [true] -> Lit (Boolean.not x)
          | [x1; _x2], [true; false] -> Lit (Boolean.not x1)
          | [_x1; _x2], [false; false] -> Lit Boolean.false_
          | x :: xs, false :: ys -> And (Boolean.not x, lt_binary xs ys)
          | x :: xs, true :: ys -> Or (Boolean.not x, lt_binary xs ys)
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
            Bigint.test_bit field_size (Field.size_in_bits - 1 - i) )
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
        | [], [] -> acc
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
          Checked.run_and_check ~run:Checked.Runner.run
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

    let create = create

    let digest (proof_system : _ t) = digest ~run:Runner.run proof_system

    let generate_keypair (proof_system : _ t) =
      generate_keypair ~run:Runner.run proof_system

    let run_unchecked ~public_input ?handlers (proof_system : _ t) =
      run_unchecked ~run:Runner.run ~public_input ?handlers proof_system

    let run_checked ~public_input ?handlers (proof_system : _ t) =
      run_checked ~run:Runner.run ~public_input ?handlers proof_system

    let check ~public_input ?handlers (proof_system : _ t) =
      check ~run:Runner.run ~public_input ?handlers proof_system

    let prove ~public_input ?proving_key ?handlers (proof_system : _ t) =
      prove ~run:Runner.run ~public_input ?proving_key ?handlers proof_system

    let verify ~public_input ?verification_key (proof_system : _ t) =
      verify ~run:Runner.run ~public_input ?verification_key proof_system
  end

  module Perform = struct
    type ('a, 't) t =
      't -> unit Checked.run_state -> unit Checked.run_state * 'a

    let generate_keypair ~run ~exposing k =
      Run.generate_keypair ~run ~exposing k

    let prove ~run key t k = Run.prove ~run key t () k

    let verify = Run.verify

    let constraint_system = Run.constraint_system

    let run_unchecked ~run t = snd (run_unchecked ~run t ())

    let run_and_check ~run t = Or_error.map (run_and_check ~run t ()) ~f:snd

    let check ~run t = check ~run t ()
  end

  let generate_keypair ~exposing k =
    Run.generate_keypair ~run:Runner.run ~exposing k

  let conv f = Run.conv (fun x _ -> f x)

  let prove key t s k = Run.prove ~run:Runner.run key t s k

  let generate_auxiliary_input t s k =
    Run.generate_auxiliary_input ~run:Runner.run t s k

  let verify = Run.verify

  let constraint_system ~exposing k =
    Run.constraint_system ~run:Runner.run ~exposing k

  let run_unchecked t s = run_unchecked ~run:Runner.run t s

  let run_and_check t s = run_and_check ~run:Runner.run t s

  let check t s = check ~run:Runner.run t s

  let reduce_to_prover = Run.reduce_to_prover

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
  module Basic = Make_basic (Backend)
  include Basic
  module Number = Number.Make (Basic)
  module Enumerable = Enumerable.Make (Basic)
end

module Run = struct
  module Make (Backend : Backend_intf.S) = struct
    module Snark = Make (Backend)
    open Types.Run_state
    open Snark

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
        ; run_special= None }

    let run checked =
      if !(!state.as_prover) then
        failwith
          "Can't run checked code as the prover: the verifier's constraint \
           system will not match." ;
      if not !state.is_running then
        failwith "This function can't be run outside of a checked computation." ;
      match !state.run_special with
      | Some f -> f checked
      | None ->
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

          let random = random

          module Mutable = Mutable

          let ( += ) = ( += )

          let ( -= ) = ( -= )

          let ( *= ) = ( *= )

          module Vector = Vector

          let negate = negate

          module Infix = Infix

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
          state := Runner.set_prover_state (Some s) !state ;
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
        state := Runner.set_prover_state (Some s) !state ;
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

      let create = create

      let digest (proof_system : _ t) = digest ~run:as_stateful proof_system

      let generate_keypair (proof_system : _ t) =
        generate_keypair ~run:as_stateful proof_system

      let run_unchecked ~public_input ?handlers (proof_system : _ t) =
        snd
          (run_unchecked ~run:as_stateful ~public_input ?handlers proof_system
             ())

      let run_checked ~public_input ?handlers (proof_system : _ t) =
        Or_error.map
          (run_checked' ~run:as_stateful ~public_input ?handlers proof_system
             ()) ~f:(fun (s, x, state) -> x )

      let check ~public_input ?handlers (proof_system : _ t) =
        Or_error.map ~f:(Fn.const ())
          (run_checked' ~run:as_stateful ~public_input ?handlers proof_system
             ())

      let prove ~public_input ?proving_key ?handlers (proof_system : _ t) =
        prove ~run:as_stateful ~public_input ?proving_key ?handlers
          proof_system ()

      let verify ~public_input ?verification_key (proof_system : _ t) =
        verify ~run:as_stateful ~public_input ?verification_key proof_system
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
      | None -> request_witness typ (fun () -> r)
      | Some such_that ->
          let x = request_witness typ (fun () -> r) in
          such_that x ; x

    let exists ?request ?compute typ =
      let request = Option.map request ~f:As_prover.run_prover in
      let compute = Option.map compute ~f:As_prover.run_prover in
      run (exists ?request ?compute typ)

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
          Runner.set_prover_state
            (Option.map prover_state ~f:(fun _ -> ()))
            state
        in
        let state, a = as_stateful x state in
        let state = Runner.set_prover_state prover_state state in
        (state, a)
      in
      Types.Checked.Direct (f, fun x -> Pure x)

    let constraint_system ~exposing x =
      Perform.constraint_system ~run:as_stateful ~exposing x

    let generate_keypair ~exposing x =
      Perform.generate_keypair ~run:as_stateful ~exposing x

    let prove pk x = Perform.prove ~run:as_stateful pk x

    let verify pf vk spec = verify pf vk spec

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
      let next_auxiliary = ref 1 in
      let run_special x =
        let count', a =
          constraint_count_aux ~log ~auxc:next_auxiliary !count x
        in
        count := count' ;
        a
      in
      let {run_special= old; _} = !state in
      state := {!state with run_special= Some run_special} ;
      ignore (x ()) ;
      state := {!state with run_special= old} ;
      !count
  end
end

type 'field m = (module Snark_intf.Run with type field = 'field)

let make (type field)
    (module Backend : Backend_intf.S with type Field.t = field) : field m =
  (module Run.Make (Backend))

let%test_module "snark0-test" =
  ( module struct
    include Make (Backends.Mnt4.GM)
  end )
