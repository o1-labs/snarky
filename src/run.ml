open Core_kernel

module type S = sig
  include Checked_intf.Backend_types

  type ('a, 's) t

  type 's prover_state

  val assert_ : ?label:string -> Field.Var.t Constraint.t -> (unit, 's) t

  val assert_all :
    ?label:string -> Field.Var.t Constraint.t list -> (unit, 's) t

  val assert_r1cs :
    ?label:string -> Field.Var.t -> Field.Var.t -> Field.Var.t -> (unit, _) t

  val assert_square :
    ?label:string -> Field.Var.t -> Field.Var.t -> (unit, _) t

  val as_prover :
       (unit, Field.Var.t -> Field.t, 's prover_state) As_prover0.t
    -> (unit, 's) t

  val with_state :
       ?and_then:(   's1 prover_state
                  -> ( unit
                     , Field.Var.t -> Field.t
                     , 's prover_state )
                     As_prover0.t)
    -> ('s1 prover_state, Field.Var.t -> Field.t, 's prover_state) As_prover0.t
    -> ('a, 's1) t
    -> ('a, 's) t

  val next_auxiliary : (int, 's) t

  val request_witness :
       run:((unit, unit, Field.t, Field.Var.t) Checked.t -> (unit, unit) t)
    -> ('var, 'value, Field.t, Field.Var.t) Typ.t
    -> ('value Request.t, Field.Var.t -> Field.t, 's prover_state) As_prover0.t
    -> ('var, 's) t

  val perform :
       run:((unit, unit, Field.t, Field.Var.t) Checked.t -> (unit, unit) t)
    -> (unit Request.t, Field.Var.t -> Field.t, 's prover_state) As_prover0.t
    -> (unit, 's) t

  val request :
       run:((unit, unit, Field.t, Field.Var.t) Checked.t -> (unit, unit) t)
    -> ?such_that:('var -> (unit, 's) t)
    -> ('var, 'value, Field.t, Field.Var.t) Typ.t
    -> 'value Request.t
    -> ('var, 's) t
  (** TODO: Come up with a better name for this in relation to the above *)

  val exists :
       run:((unit, unit, Field.t, Field.Var.t) Checked.t -> (unit, unit) t)
    -> ?request:( 'value Request.t
                , Field.Var.t -> Field.t
                , 's prover_state )
                As_prover0.t
    -> ?compute:('value, Field.Var.t -> Field.t, 's prover_state) As_prover0.t
    -> ('var, 'value, Field.t, Field.Var.t) Typ.t
    -> ('var, 's) t

  val exists_provider :
       run:((unit, unit, Field.t, Field.Var.t) Checked.t -> (unit, unit) t)
    -> ('var, 'value, Field.t, Field.Var.t) Typ.t
    -> ('value, Field.Var.t -> Field.t, 's prover_state) Provider.t
    -> (('var, 'value) Handle.t, 's) t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  module Handler : sig
    type t = request -> response
  end

  val handle : ('a, 's) t -> Handler.t -> ('a, 's) t

  val with_label : string -> ('a, 's) t -> ('a, 's) t
end

module Make
    (M : Checked_intf.Backend_types)
    (State : Checked_intf.Runner_state(M).S) :
  S
  with type ('a, 's) t = 's State.t -> 's State.t * 'a
   and type 'a prover_state = 'a State.prover_state
   and type Field.t = M.Field.t
   and type Field.Var.t = M.Field.Var.t = struct
  module Constraint0 = Constraint
  include M
  open State

  type ('a, 's) t = 's State.t -> 's State.t * 'a

  module Let_syntax = struct
    module Let_syntax = struct
      let map ~(f : 'a -> 'b) (a : ('a, 's) t) : ('b, 's) t =
       fun s ->
        let s, a = a s in
        (s, f a)

      let bind ~(f : 'a -> ('b, 's) t) (a : ('a, 's) t) : ('b, 's) t =
       fun s ->
        let s, a = a s in
        f a s

      let return (a : 'a) : ('a, 's) t = fun s -> (s, a)
    end
  end

  include Let_syntax.Let_syntax

  type 'a prover_state = 'a State.prover_state

  let run_as_prover x state =
    match (x, prover_state state) with
    | Some x, Some s ->
        let s', y = As_prover.run x (get_value state) s in
        (set_prover_state (Some s') state, Some y)
    | _, _ -> (state, None)

  let as_prover x state =
    let state, (_ : unit option) = run_as_prover (Some x) state in
    (state, ())

  let with_label label f state =
    let stack = stack state in
    let state, b = f (set_stack (label :: stack) state) in
    (set_stack stack state, b)

  let add_constraint c state =
    if eval_constraints state && not (Constraint.eval c state) then
      failwithf "Constraint unsatisfied:\n%s\n%s\n" (Constraint0.annotation c)
        (Constraint0.stack_to_string (stack state))
        () ;
    Constraint.add ~stack:(stack state) c state ;
    (state, ())

  let assert_ ?label c =
    add_constraint
      (List.map c ~f:(fun c -> Constraint0.override_label c label))

  let assert_r1cs ?label a b c = assert_ (Constraint0.r1cs ?label a b c)

  let assert_square ?label a c = assert_ (Constraint0.square ?label a c)

  let assert_all =
    let map_concat_rev xss ~f =
      let rec go acc xs xss =
        match (xs, xss) with
        | [], [] -> acc
        | [], xs :: xss -> go acc xs xss
        | x :: xs, _ -> go (f x :: acc) xs xss
      in
      go [] [] xss
    in
    fun ?label cs ->
      add_constraint
        (map_concat_rev ~f:(fun c -> Constraint0.override_label c label) cs)

  let assert_equal ?label x y = assert_ (Constraint0.equal ?label x y)

  let do_nothing _ = As_prover0.return ()

  let with_state ?(and_then = do_nothing) as_prover f state =
    let state, s_sub = run_as_prover (Some as_prover) state in
    let sub_state, y = f (set_prover_state s_sub state) in
    let sub_prover_state = Option.map ~f:and_then (prover_state sub_state) in
    let state, (_ : unit option) = run_as_prover sub_prover_state state in
    (state, y)

  type response = Request.response

  let unhandled = Request.unhandled

  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  module Handler = struct
    type t = request -> response
  end

  let with_handler ~f h state =
    let handler = handler state in
    let state, y = f (set_handler (Request.Handler.push handler h) state) in
    (set_handler handler state, y)

  let handle f k = with_handler ~f (Request.Handler.create_single k)

  let clear_handler ~f state =
    let handler = handler state in
    let state, y = f (set_handler Request.Handler.fail state) in
    (set_handler handler state, y)

  let exists_provider
      ~(run : (unit, unit, Field.t, Field.Var.t) Checked.t -> (unit, unit) t)
      {Types.Typ.store; alloc; check; _} provider state =
    match prover_state state with
    | Some s ->
        let s', value =
          Provider.run provider (stack state) (get_value state) s
            (handler state)
        in
        let var = Typ_monads.Store.run (store value) (store_field_elt state) in
        let state = set_prover_state (Some (to_prover_state ())) state in
        let state, () = run (check var) state in
        let state = set_prover_state (Some s') state in
        (state, {Handle.var; value= Some value})
    | None ->
        let var = Typ_monads.Alloc.run alloc (alloc_var state) in
        let state = set_prover_state None state in
        let state, () = run (check var) state in
        let state = set_prover_state None state in
        (state, {Handle.var; value= None})

  let exists ~run ?request ?compute typ =
    let provider =
      let request =
        Option.value request ~default:(As_prover0.return Request.Fail)
      in
      match compute with
      | None -> Provider.Request request
      | Some c -> Provider.Both (request, c)
    in
    bind (exists_provider ~run typ provider) ~f:(fun h -> return (Handle.var h))

  let request_witness ~run
      (typ : ('var, 'value, Field.t, Field.Var.t) Types.Typ.t)
      (r : ('value Request.t, Field.Var.t -> Field.t, 's) As_prover0.t) =
    bind (exists_provider ~run typ (Request r)) ~f:(fun h -> return (Handle.var h))

  let request ~run ?such_that typ r =
    match such_that with
    | None -> request_witness ~run typ (As_prover0.return r)
    | Some such_that ->
        let open Let_syntax in
        let%bind x = request_witness ~run typ (As_prover0.return r) in
        let%map () = such_that x in
        x

  let perform ~run req = request_witness ~run (Typ.unit ()) req

  let next_auxiliary state = (state, !(next_auxiliary state))
end
