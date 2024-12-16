open Core_kernel

module Make (Field : sig
  type t [@@deriving sexp]

  val equal : t -> t -> bool
end)
(Types : Types.Types)
(Basic : Checked_intf.Basic with type field = Field.t with module Types := Types)
(As_prover : As_prover_intf.Basic
               with type field := Basic.field
               with module Types := Types) :
  Checked_intf.S with module Types := Types with type field = Field.t = struct
  include Basic

  let request_witness (typ : ('var, 'value) Types.Typ.t)
      (r : 'value Request.t As_prover.t) =
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
        { request : 'a Request.t; respond : 'a Request.Response.t -> response }
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

  let assert_ ?label c = add_constraint (Constraint.override_label c label)

  let assert_r1cs ?label a b c = assert_ (Constraint.r1cs ?label a b c)

  let assert_square ?label a c = assert_ (Constraint.square ?label a c)

  let assert_all ?label cs =
    List.fold_right cs ~init:(return ()) ~f:(fun c (acc : _ t) ->
        bind acc ~f:(fun () ->
            add_constraint (Constraint.override_label c label) ) )

  let assert_equal ?label x y =
    match (x, y) with
    | Cvar.Constant x, Cvar.Constant y ->
        if Field.equal x y then return ()
        else
          failwithf !"assert_equal: %{sexp: Field.t} != %{sexp: Field.t}" x y ()
    | _ ->
        assert_ (Constraint.equal ?label x y)
end
