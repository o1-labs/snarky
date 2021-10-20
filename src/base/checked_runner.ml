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
               "Snarky.Checked_runner.Runtime_error(_, _, _, _)\n\n%s" message)
      | _ ->
          None )

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
      if i <= num_inputs then Run_state.Vector.get input (i - 1)
      else Run_state.Vector.get aux (i - num_inputs - 1)
    in
    Cvar.eval (`Return_values_will_be_mutated get_one)

  let store_field_elt {next_auxiliary; aux; _} x =
    let v = !next_auxiliary in
    incr next_auxiliary ;
    Run_state.Vector.emplace_back aux x ;
    Cvar.Unsafe.of_index v

  let store_field_elt_later {next_auxiliary; aux; _} () =
    let v = !next_auxiliary in
    incr next_auxiliary ;
    (* Place zero to ensure that the vector length stays in sync. *)
    Run_state.Vector.emplace_back aux Field.zero ;
    (Run_state.Vector.set aux v, Cvar.Unsafe.of_index v)

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

  let mk_lazy x s =
    let old_stack = s.stack in
    ( s
    , Lazy.from_fun (fun () ->
          let {stack; prover_state; _} = s in
          let prover_state = Option.map prover_state ~f:ignore in
          let s = Run_state.set_prover_state prover_state s in
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
          let _s', y = x {s with stack= old_stack @ (label :: stack)} in
          y ) )

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
                   (Field.to_string (get_value s var3)))
           | _ ->
               Format.asprintf
                 !"%{sexp:(Field.t, Field.t) Constraint0.basic}"
                 (Constraint0.Basic.map basic ~f:(get_value s)) ))

  let stack_to_string = String.concat ~sep:"\n"

  let add_constraint ~stack (t : Constraint.t)
      (Constraint_system.T ((module C), system) : Field.t Constraint_system.t)
      =
    List.iter t ~f:(fun {basic; annotation} ->
        let label = Option.value annotation ~default:"<unknown>" in
        C.add_constraint system basic ~label:(stack_to_string (label :: stack))
    )

  let add_constraint c s =
    if !(s.as_prover) then
      (* Don't add constraints as the prover, or the constraint system won't match! *)
      (s, ())
    else (
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
          (Constraint.annotation c) (stack_to_string s.stack)
          (Sexp.to_string (Constraint.sexp_of_t c))
          (log_constraint c s) () ;
      if not !(s.as_prover) then
        Option.iter s.system ~f:(fun system ->
            add_constraint ~stack:s.stack c system ) ;
      (s, ()) )

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
    match s.prover_state with
    | Some ps ->
        let old = !(s.as_prover) in
        s.as_prover := true ;
        let ps, value =
          As_prover.Provider.run p s.stack (get_value s) ps s.handler
        in
        s.as_prover := old ;
        let var =
          if !(s.as_prover) then
            (* If we're nested in a prover block, create constants instead of
               storing.
            *)
            Typ_monads.Store.run (store value) Cvar.constant (fun () ->
                failwith
                  "Cannot defer the creation of a variable while not \
                  generating constraints" )
          else
            Typ_monads.Store.run (store value) (store_field_elt s)
              (store_field_elt_later s)
        in
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

  let constraint_count ?(weight = List.length)
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
      count := !count + weight c
    in
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
                 "Encountered an error while evaluating the checked \
                  computation:\n\
                 \  %s\n\n\
                  Label stack trace:\n\
                  %s\n\n\n\
                  %s"
                 (Exn.to_string exn) (stack_to_string s.stack) bt
             , s.stack
             , exn
             , bt ))

  (* INVARIANT: run _ s = (s', _) gives
       (s'.prover_state = Some _) iff (s.prover_state = Some _) *)
  let rec run : type a s.
      (a, s, Field.t) Checked.t -> s run_state -> s run_state * a =
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
    | Reduced (t, d, res, k) ->
        let s, y =
          if
            (not !(s.as_prover))
            && Option.is_some s.prover_state
            && Option.is_none s.system
          then
            (* In reduced mode, we only evaluate prover code and use it to fill
               the public and auxiliary input vectors. Thus, these three
               conditions are important:
               - if there is no prover state, we can't run the prover code
               - if there is an R1CS to be filled, we need to run the original
                 computation to add the constraints to it
               - if we are running a checked computation inside a prover block,
                 we need to be sure that we aren't allocating R1CS variables
                 that aren't present in the original constraint system.
                 See the comment in the [Exists] branch of [flatten_as_prover]
                 below for more context.
            *)
            (handle_error s (fun () -> d s), res)
          else run t s
        in
        let k = handle_error s (fun () -> k y) in
        run k s
    | Lazy (x, k) ->
        let s, y = mk_lazy (run x) s in
        let k = handle_error s (fun () -> k y) in
        run k s
    | With_label (lab, t, k) ->
        let s, y = with_label lab (run t) s in
        let k = handle_error s (fun () -> k y) in
        run k s
    | Add_constraint (c, t) ->
        let s, () = handle_error s (fun () -> add_constraint c s) in
        run t s
    | With_state (p, and_then, t_sub, k) ->
        let t_sub = run t_sub in
        let s, y = handle_error s (fun () -> with_state p and_then t_sub s) in
        let k = handle_error s (fun () -> k y) in
        run k s
    | With_handler (h, t, k) ->
        let s, y = with_handler h (run t) s in
        let k = handle_error s (fun () -> k y) in
        run k s
    | Clear_handler (t, k) ->
        let s, y = clear_handler (run t) s in
        let k = handle_error s (fun () -> k y) in
        run k s
    | Exists (typ, p, k) ->
        let typ =
          { Types.Typ.store= typ.store
          ; read= typ.read
          ; alloc= typ.alloc
          ; check= (fun var -> run (typ.check var)) }
        in
        let s, y = handle_error s (fun () -> exists typ p s) in
        let k = handle_error s (fun () -> k y) in
        run k s
    | Next_auxiliary k ->
        let s, y = next_auxiliary s in
        let k = handle_error s (fun () -> k y) in
        run k s

  let dummy_vector = Run_state.Vector.null

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
    | Lazy (x, k) ->
        let flattened =
          Lazy.from_fun (fun () ->
              (* We don't know the stack at forcing time, so just announce that
                 we're forcing.
              *)
              let label = "Lazy value forced (reduced):" in
              flatten_as_prover next_auxiliary (label :: stack) x )
        in
        let y = Lazy.map ~f:snd flattened in
        let f s =
          if Lazy.is_val flattened then
            (* The lazy value has been forced somewhere later in the checked
               computation, so we need to do the prover parts of it.
            *)
            let f, _ = Lazy.force flattened in
            let {prover_state; _} = s in
            let prover_state' = Option.map prover_state ~f:ignore in
            let s = f (set_prover_state prover_state' s) in
            set_prover_state prover_state s
          else s
        in
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
                (Constraint.annotation c) (stack_to_string stack)
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
            if !(s.as_prover) then
              (* If we are running inside a prover block, any call to [exists]
                 will cause a difference between the expected layout in the
                 R1CS and the actual layout that the prover puts data into:

                 R1CS layout:
                        next R1CS variable to be allocated
                                      \/
                 ... [ var{n-1} ] [ var{n} ] [ var{n+1} ] [ var{n+2} ] ...

                 Prover block layout:
                                 prover writes values here due to [exists]
                                         \/         ...        \/
                 ... [ var{n-1} ] [ prover_var{1} ] ... [ prover_var{k} ] [ var{n} ] ...

                 To avoid a divergent layout (and thus unsatisfied constraint
                 system), we run the original checked computation instead.

                 Note: this currently should never happen, because this
                 function is only invoked on a complete end-to-end checked
                 computation using the proving API.
                 By definition, this cannot be wrapped in a prover block.
              *)
              failwith
                "Internal error: attempted to store field elements for a \
                 variable that is not known to the constraint system." ;
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

  val set_constraint_logger :
    (?at_label_boundary:[`Start | `End] * string -> constr -> unit) -> unit

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
      -> input:field Run_state.Vector.t
      -> next_auxiliary:int ref
      -> aux:field Run_state.Vector.t
      -> ?system:r1cs
      -> ?eval_constraints:bool
      -> ?handler:Request.Handler.t
      -> 's option
      -> ('s, field) Run_state.t
  end
end
