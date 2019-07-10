open Core_kernel

module Types = struct
  module Checked = struct
    type ('a, 's, 'f) t = ('s, 'f) Run_state.t -> ('s, 'f) Run_state.t * 'a
  end

  module As_prover = struct
    type ('a, 'f, 's) t = ('a, 'f, 's) As_prover0.t
  end

  module Typ = struct
    include Types.Typ.T

    type ('var, 'value, 'f) t =
      ('var, 'value, 'f, (unit, unit, 'f) Checked.t) typ
  end

  module Provider = struct
    include Types.Provider.T

    type ('a, 'f, 's) t =
      (('a Request.t, 'f, 's) As_prover0.t, ('a, 'f, 's) As_prover0.t) provider
  end
end

type ('a, 's, 'f) t = ('a, 's, 'f) Types.Checked.t

(* Non-field-dependent interface, used for ppx's. *)
module Basic = struct
  open Run_state

  let with_label lab t s =
    let {stack; _} = s in
    let s', y = t {s with stack= lab :: stack} in
    ({s' with stack}, y)

  let with_handler h t s =
    let {handler; _} = s in
    let s', y = t {s with handler= Request.Handler.push handler h} in
    ({s' with handler}, y)

  let clear_handler t s =
    let {handler; _} = s in
    let s', y = t {s with handler= Request.Handler.fail} in
    ({s' with handler}, y)

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

  include Monad_let.Make3 (struct
    type ('a, 's, 'f) t = ('a, 's, 'f) Types.Checked.t

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
end

module Make
    (Basic : Checked_intf.Basic)
    (As_prover : As_prover_intf.Basic
                 with type ('a, 'f, 's) t =
                             ('a, 'f, 's) Basic.Types.As_prover.t
                  and type 'f field := 'f Basic.field) :
  Checked_intf.S
  with module Types = Basic.Types
  with type 'f field = 'f Basic.field = struct
  include Basic

  let request_witness (typ : ('var, 'value, 'f field) Types.Typ.t)
      (r : ('value Request.t, 'f field, 's) As_prover.t) =
    let%map h = exists typ (Request r) in
    Handle.var h

  let request ?such_that typ r =
    match such_that with
    | None ->
        request_witness typ (As_prover.return r)
    | Some such_that ->
        let open Let_syntax in
        let%bind x = request_witness typ (As_prover.return r) in
        let%map () = such_that x in
        x

  let exists_handle ?request ?compute typ =
    let provider =
      let request =
        Option.value request ~default:(As_prover.return Request.Fail)
      in
      match compute with
      | None ->
          Types.Provider.Request request
      | Some c ->
          Types.Provider.Both (request, c)
    in
    exists typ provider

  let exists ?request ?compute typ =
    let%map h = exists_handle ?request ?compute typ in
    Handle.var h

  type response = Request.response

  let unhandled = Request.unhandled

  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  let handle t k = with_handler (Request.Handler.create_single k) t

  let handle_as_prover t k =
    let handler = ref None in
    let%bind () =
      as_prover
        As_prover.(
          let%map h = k in
          handler := Some h)
    in
    handle t (fun request -> (Option.value_exn !handler) request)

  let do_nothing _ = As_prover.return ()

  let with_state ?(and_then = do_nothing) f sub = with_state f and_then sub

  let assert_ ?label c =
    add_constraint (List.map c ~f:(fun c -> Constraint.override_label c label))

  let assert_r1cs ?label a b c = assert_ (Constraint.r1cs ?label a b c)

  let assert_square ?label a c = assert_ (Constraint.square ?label a c)

  let assert_all =
    let map_concat_rev xss ~f =
      let rec go acc xs xss =
        match (xs, xss) with
        | [], [] ->
            acc
        | [], xs :: xss ->
            go acc xs xss
        | x :: xs, _ ->
            go (f x :: acc) xs xss
      in
      go [] [] xss
    in
    fun ?label cs ->
      add_constraint
        (map_concat_rev ~f:(fun c -> Constraint.override_label c label) cs)

  let assert_equal ?label x y = assert_ (Constraint.equal ?label x y)
end

include Basic
