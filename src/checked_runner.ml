open Core_kernel

let eval_constraints = ref true

module Make_checked
    (Backend : Backend_extended.S)
    (As_prover : As_prover_intf.S with type 'f field := Backend.Field.t) =
struct
  type 'prover_state run_state = ('prover_state, Backend.Field.t) Run_state.t

  module Types = struct
    module Checked = struct
      type ('a, 's, 'f) t = 's run_state -> 's run_state * 'a
    end

    module As_prover = struct
      type ('a, 'f, 's) t = ('a, 'f, 's) As_prover.t
    end

    module Typ = struct
      include Types.Typ.T

      type ('var, 'value, 'f) t =
        ('var, 'value, 'f, (unit, unit, 'f) Checked.t) typ
    end

    module Provider = struct
      include Types.Provider.T

      type ('a, 'f, 's) t =
        (('a Request.t, 'f, 's) As_prover.t, ('a, 'f, 's) As_prover.t) provider
    end
  end

  type 'f field = Backend.Field.t

  type ('a, 's, 'f) t = ('a, 's, 'f field) Types.Checked.t

  include Monad_let.Make3 (struct
    type ('a, 's, 'f) t = ('a, 's, 'f field) Types.Checked.t

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
  open Checked

  let get_value {num_inputs; input; aux; _} : Cvar.t -> Field.t =
    let get_one i =
      if i <= num_inputs then Field.Vector.get input (i - 1)
      else Field.Vector.get aux (i - num_inputs - 1)
    in
    Cvar.eval (`Return_values_will_be_mutated get_one)

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
    | _, _ ->
        (state, None)

  let as_prover x s =
    let s', (_ : unit option) = run_as_prover (Some x) s in
    (s', ())

  let with_label lab t s =
    let {stack; _} = s in
    let s', y = t {s with stack= lab :: stack} in
    ({s' with stack}, y)

  let log_constraint c s =
    String.concat ~sep:"\n"
      (List.map c ~f:(fun {basic; _} ->
           match basic with
           | Boolean var ->
               Format.(
                 asprintf "Boolean %s" (Field.to_string (get_value s var)))
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
                   (Field.to_string (get_value s var3))) ))

  let add_constraint c s =
    if !(s.as_prover) then
      failwith
        "Cannot add a constraint as the prover: the verifier's constraint \
         system will not match." ;
    Option.iter s.log_constraint ~f:(fun f -> f c) ;
    if s.eval_constraints && not (Constraint.eval c (get_value s)) then
      failwithf
        "Constraint unsatisfied (unreduced):\n\
         %s\n\
         %s\n\n\
         Constraint:\n\
         %s\n\
         Data:\n\
         %s"
        (Constraint.annotation c)
        (Constraint.stack_to_string s.stack)
        (Sexp.to_string (Constraint.sexp_of_t c))
        (log_constraint c s) () ;
    Option.iter s.system ~f:(fun system ->
        Constraint.add ~stack:s.stack c system ) ;
    (s, ())

  let with_state p and_then t_sub s =
    let s, s_sub = run_as_prover (Some p) s in
    let s_sub, y = t_sub (set_prover_state s_sub s) in
    let s, (_ : unit option) =
      run_as_prover (Option.map ~f:and_then s_sub.prover_state) s
    in
    (s, y)

  let with_handler h t s =
    let {handler; _} = s in
    let s', y = t {s with handler= Request.Handler.push handler h} in
    ({s' with handler}, y)

  let clear_handler t s =
    let {handler; _} = s in
    let s', y = t {s with handler= Request.Handler.fail} in
    ({s' with handler}, y)

  let exists {Types.Typ.store; alloc; check; _} p s =
    if !(s.as_prover) then
      failwith
        "Cannot create a variable as the prover: the verifier's constraint \
         system will not match." ;
    match s.prover_state with
    | Some ps ->
        let old = !(s.as_prover) in
        s.as_prover := true ;
        let ps, value =
          As_prover.Provider.run p s.stack (get_value s) ps s.handler
        in
        s.as_prover := old ;
        let var = Typ_monads.Store.run (store value) (store_field_elt s) in
        (* TODO: Push a label onto the stack here *)
        let s, () = check var (set_prover_state (Some ()) s) in
        (set_prover_state (Some ps) s, {Handle.var; value= Some value})
    | None ->
        let var = Typ_monads.Alloc.run alloc (alloc_var s) in
        (* TODO: Push a label onto the stack here *)
        let s, () = check var (set_prover_state None s) in
        (set_prover_state None s, {Handle.var; value= None})

  let next_auxiliary s = (s, !(s.next_auxiliary))

  let with_lens (lens : ('whole, 'view) Lens.t) t rs =
    let s = rs.prover_state in
    let s' = Option.map ~f:(Lens.get lens) s in
    let rs, a = t (set_prover_state s' rs) in
    let s = Option.map2 ~f:(Lens.set lens) s s' in
    (set_prover_state s rs, a)

  let constraint_count ?log:_ t =
    (* TODO: Integrate log with log_constraint *)
    let count = ref 0 in
    let log_constraint c = count := !count + List.length c in
    let state =
      Run_state.
        { system= None
        ; input= Vector.null
        ; aux= Vector.null
        ; eval_constraints= false
        ; num_inputs= 0
        ; next_auxiliary= ref 1
        ; prover_state= None
        ; stack= []
        ; handler= Request.Handler.fail
        ; is_running= true
        ; as_prover= ref false
        ; log_constraint= Some log_constraint }
    in
    let _ = t state in
    !count
end

module type Run_extras = sig
  type field

  type cvar

  module Types : Types.Types

  val get_value : ('a, field) Run_state.t -> cvar -> field

  val store_field_elt : ('a, field) Run_state.t -> field -> cvar

  val alloc_var : ('a, 'b) Run_state.t -> unit -> cvar

  val run_as_prover :
       ('a, field, 'b) Types.As_prover.t option
    -> ('b, field) Run_state.t
    -> ('b, field) Run_state.t * 'a option
end

module Make (Backend : Backend_extended.S) = struct
  open Backend
  open Run_state

  let constraint_logger = ref None

  let set_constraint_logger f = constraint_logger := Some f

  let clear_constraint_logger () = constraint_logger := None

  module Checked_runner = Make_checked (Backend) (As_prover)
  open Checked_runner

  type 'prover_state run_state = 'prover_state Checked_runner.run_state

  type 's state = 's run_state

  type ('a, 's, 't) run = 't -> 's run_state -> 's run_state * 'a

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

  module Types = Checked.Types

  (* INVARIANT: run _ s = (s', _) gives
       (s'.prover_state = Some _) iff (s.prover_state = Some _) *)
  let rec run : type a s.
      (a, s, Field.t) Checked.t -> s run_state -> s run_state * a =
   fun t s ->
    match t with
    | As_prover (x, k) ->
        let s, () = as_prover x s in
        run k s
    | Pure x ->
        (s, x)
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
        let s, y = with_label lab (run t) s in
        run (k y) s
    | Add_constraint (c, t) ->
        let s, () = add_constraint c s in
        run t s
    | With_state (p, and_then, t_sub, k) ->
        let s, y = with_state p and_then (run t_sub) s in
        run (k y) s
    | With_handler (h, t, k) ->
        let s, y = with_handler h (run t) s in
        run (k y) s
    | Clear_handler (t, k) ->
        let s, y = clear_handler (run t) s in
        run (k y) s
    | Exists (typ, p, k) ->
        let typ =
          { Types.Typ.store= typ.store
          ; read= typ.read
          ; alloc= typ.alloc
          ; check= (fun var -> run (typ.check var)) }
        in
        let s, y = exists typ p s in
        run (k y) s
    | Next_auxiliary k ->
        let s, y = next_auxiliary s in
        run (k y) s

  let dummy_vector = Field.Vector.create ()

  let fake_state next_auxiliary stack =
    { system= None
    ; input= dummy_vector
    ; aux= dummy_vector
    ; eval_constraints= false
    ; num_inputs= 0
    ; next_auxiliary
    ; prover_state= None
    ; stack
    ; handler= Request.Handler.fail
    ; is_running= true
    ; as_prover= ref false
    ; log_constraint= None }

  let rec flatten_as_prover : type a s.
         int ref
      -> string list
      -> (a, s, Field.t) Checked.t
      -> (s run_state -> s run_state) * a =
   fun next_auxiliary stack t ->
    match t with
    | As_prover (x, k) ->
        let f, a = flatten_as_prover next_auxiliary stack k in
        ( (fun s ->
            let s', (_ : unit option) = run_as_prover (Some x) s in
            f s' )
        , a )
    | Pure x ->
        (Fn.id, x)
    | Direct (d, k) ->
        let _, y = d (fake_state next_auxiliary stack) in
        let f, a = flatten_as_prover next_auxiliary stack (k y) in
        ( (fun s ->
            let {prover_state; _} = s in
            let s, _y = d s in
            f (set_prover_state prover_state s) )
        , a )
    | Reduced (t, _d, _res, k) ->
        let f, y = flatten_as_prover next_auxiliary stack t in
        let g, a = flatten_as_prover next_auxiliary stack (k y) in
        ((fun s -> g (f s)), a)
    | With_label (lab, t, k) ->
        let f, y = flatten_as_prover next_auxiliary (lab :: stack) t in
        let g, a = flatten_as_prover next_auxiliary stack (k y) in
        ((fun s -> g (f s)), a)
    | Add_constraint (c, t) ->
        let f, y = flatten_as_prover next_auxiliary stack t in
        ( (fun s ->
            Option.iter s.log_constraint ~f:(fun f -> f c) ;
            if s.eval_constraints && not (Constraint.eval c (get_value s)) then
              failwithf
                "Constraint unsatisfied:\n%s\n%s\n\nConstraint:\n%s\nData:\n%s"
                (Constraint.annotation c)
                (Constraint.stack_to_string stack)
                (Sexp.to_string (Constraint.sexp_of_t c))
                (log_constraint c s) () ;
            f s )
        , y )
    | With_state (p, and_then, t_sub, k) ->
        let f_sub, y = flatten_as_prover next_auxiliary stack t_sub in
        let f, a = flatten_as_prover next_auxiliary stack (k y) in
        ( (fun s ->
            let s, s_sub = run_as_prover (Some p) s in
            let s_sub = f_sub (set_prover_state s_sub s) in
            let s, (_ : unit option) =
              run_as_prover (Option.map ~f:and_then s_sub.prover_state) s
            in
            f s )
        , a )
    | With_handler (h, t, k) ->
        let f, y = flatten_as_prover next_auxiliary stack t in
        let g, a = flatten_as_prover next_auxiliary stack (k y) in
        ( (fun s ->
            let {handler; _} = s in
            let s' = f {s with handler= Request.Handler.push handler h} in
            g {s' with handler} )
        , a )
    | Clear_handler (t, k) ->
        let f, y = flatten_as_prover next_auxiliary stack t in
        let g, a = flatten_as_prover next_auxiliary stack (k y) in
        ( (fun s ->
            let {handler; _} = s in
            let s' = f {s with handler= Request.Handler.fail} in
            g {s' with handler} )
        , a )
    | Exists ({store; alloc; check; _}, p, k) ->
        let var =
          Typ_monads.Alloc.run alloc
            (alloc_var (fake_state next_auxiliary stack))
        in
        let f, () = flatten_as_prover next_auxiliary stack (check var) in
        let handle = {Handle.var; value= None} in
        let g, a = flatten_as_prover next_auxiliary stack (k handle) in
        ( (fun s ->
            let old = !(s.as_prover) in
            s.as_prover := true ;
            let ps, value =
              As_prover.Provider.run p s.stack (get_value s)
                (Option.value_exn s.prover_state)
                s.handler
            in
            s.as_prover := old ;
            let _var =
              Typ_monads.Store.run (store value) (store_field_elt s)
            in
            let s = f (set_prover_state (Some ()) s) in
            handle.value <- Some value ;
            g (set_prover_state (Some ps) s) )
        , a )
    | Next_auxiliary k ->
        flatten_as_prover next_auxiliary stack (k !next_auxiliary)

  let reduce_to_prover (type a s) next_auxiliary
      (t : (a, s, Field.t) Checked.t) : (a, s, Field.t) Checked.t =
    let f, a = flatten_as_prover next_auxiliary [] t in
    Reduced (t, f, a, Checked.return)

  module State = struct
    let make ~num_inputs ~input ~next_auxiliary ~aux ?system
        ?(eval_constraints = !eval_constraints) ?handler (s0 : 's option) =
      next_auxiliary := 1 + num_inputs ;
      (* We can't evaluate the constraints if we are not computing over a value. *)
      let eval_constraints = eval_constraints && Option.is_some s0 in
      Option.iter system ~f:(fun system ->
          R1CS_constraint_system.set_primary_input_size system num_inputs ) ;
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
      ; log_constraint= !constraint_logger }
  end
end

module type S = sig
  include Run_extras

  type constr

  type r1cs

  val set_constraint_logger : (constr -> unit) -> unit

  val clear_constraint_logger : unit -> unit

  type 'prover_state run_state = ('prover_state, field) Run_state.t

  type 's state = 's run_state

  type ('a, 's, 't) run = 't -> 's run_state -> 's run_state * 'a

  val run :
    ('a, 's, field) Types.Checked.t -> 's run_state -> 's run_state * 'a

  val flatten_as_prover :
       int ref
    -> string list
    -> ('a, 's, field) Types.Checked.t
    -> ('s run_state -> 's run_state) * 'a

  val reduce_to_prover :
       int ref
    -> ('a, 'b, field) Types.Checked.t
    -> ('a, 'b, field) Types.Checked.t

  module State : sig
    val make :
         num_inputs:int
      -> input:field Vector.t
      -> next_auxiliary:int ref
      -> aux:field Vector.t
      -> ?system:r1cs
      -> ?eval_constraints:bool
      -> ?handler:Request.Handler.t
      -> 's option
      -> ('s, field) Run_state.t
  end
end
