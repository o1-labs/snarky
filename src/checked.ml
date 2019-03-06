open Core_kernel
open Types.Checked

type ('a, 's, 'field, 'r) t = ('a, 's, 'field, 'r) Types.Checked.t

module T0 = struct
  let return x = Pure x

  let as_prover x = As_prover (x, return ())

  let rec map : type s a b field r.
      (a, s, field, r) t -> f:(a -> b) -> (b, s, field, r) t =
   fun t ~f ->
    match t with
    | Pure x -> Pure (f x)
    | Direct (d, k) -> Direct (d, fun b -> map (k b) ~f)
    | With_label (s, t, k) -> With_label (s, t, fun b -> map (k b) ~f)
    | As_prover (x, k) -> As_prover (x, map k ~f)
    | Add_constraint (c, t1) -> Add_constraint (c, map t1 ~f)
    | With_state (p, and_then, t_sub, k) ->
        With_state (p, and_then, t_sub, fun b -> map (k b) ~f)
    | With_handler (h, t, k) -> With_handler (h, t, fun b -> map (k b) ~f)
    | Clear_handler (t, k) -> Clear_handler (t, fun b -> map (k b) ~f)
    | Exists (typ, c, k) -> Exists (typ, c, fun v -> map (k v) ~f)
    | Next_auxiliary k -> Next_auxiliary (fun x -> map (k x) ~f)

  let map = `Custom map

  let rec bind : type s a b field r.
      (a, s, field, r) t -> f:(a -> (b, s, field, r) t) -> (b, s, field, r) t =
   fun t ~f ->
    match t with
    | Pure x -> f x
    | Direct (d, k) -> Direct (d, fun b -> bind (k b) ~f)
    | With_label (s, t, k) -> With_label (s, t, fun b -> bind (k b) ~f)
    | As_prover (x, k) -> As_prover (x, bind k ~f)
    (* Someday: This case is probably a performance bug *)
    | Add_constraint (c, t1) -> Add_constraint (c, bind t1 ~f)
    | With_state (p, and_then, t_sub, k) ->
        With_state (p, and_then, t_sub, fun b -> bind (k b) ~f)
    | With_handler (h, t, k) -> With_handler (h, t, fun b -> bind (k b) ~f)
    | Clear_handler (t, k) -> Clear_handler (t, fun b -> bind (k b) ~f)
    | Exists (typ, c, k) -> Exists (typ, c, fun v -> bind (k v) ~f)
    | Next_auxiliary k -> Next_auxiliary (fun x -> bind (k x) ~f)
end

let rec all_unit = function
  | [] -> T0.return ()
  | t :: ts -> T0.bind t ~f:(fun () -> all_unit ts)

module Let_syntax = struct
  let return = T0.return

  let bind = T0.bind

  let map =
    match T0.map with
    | `Define_using_bind -> fun ma ~f -> bind ma ~f:(fun a -> T0.return (f a))
    | `Custom x -> x

  let ( >>= ) t f = bind t ~f

  let ( >>| ) t f = map t ~f

  let both a b = bind a ~f:(fun a -> map b ~f:(fun b -> (a, b)))

  module Let_syntax = struct
    let return = return

    let bind = bind

    let map = map

    let both a b = a >>= fun a -> b >>| fun b -> (a, b)

    module Open_on_rhs = struct end
  end
end

module T = struct
  include T0

  let request_witness (typ : ('var, 'value, 'field, 'r) Types.Typ.t)
      (r : ('value Request.t, 'field, 's) As_prover0.t) =
    Exists (typ, Request r, fun h -> return (Handle.var h))

  let request ?such_that typ r =
    match such_that with
    | None -> request_witness typ (As_prover0.return r)
    | Some such_that ->
        let open Let_syntax in
        let%bind x = request_witness typ (As_prover0.return r) in
        let%map () = such_that x in
        x

  let exists ?request ?compute typ =
    let provider =
      let request =
        Option.value request ~default:(As_prover0.return Request.Fail)
      in
      match compute with
      | None -> Provider.Request request
      | Some c -> Provider.Both (request, c)
    in
    Exists (typ, provider, fun h -> return (Handle.var h))

  type response = Request.response

  let unhandled = Request.unhandled

  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  let handle t k = With_handler (Request.Handler.create_single k, t, return)

  let next_auxiliary = Next_auxiliary return

  let with_label s t = With_label (s, t, return)

  let do_nothing _ = As_prover0.return ()

  let with_state ?(and_then = do_nothing) f sub =
    With_state (f, and_then, sub, return)

  let assert_ ?label c =
    Add_constraint
      (List.map c ~f:(fun c -> Constraint.override_label c label), return ())

  let assert_r1cs ?label a b c = assert_ (Constraint.r1cs ?label a b c)

  let assert_square ?label a c = assert_ (Constraint.square ?label a c)

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
      Add_constraint
        ( map_concat_rev ~f:(fun c -> Constraint.override_label c label) cs
        , return () )

  let assert_equal ?label x y = assert_ (Constraint.equal ?label x y)
end

include T
