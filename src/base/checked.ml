open Core_kernel
open Types.Checked

type ('a, 'field) t = ('a, 'field) Types.Checked.t

(** Monad instance for [Types.Checked.t]. *)
module T0 = struct
  type nonrec ('a, 'field) t = ('a, 'field) t

  let return x = Pure x

  let rec map : type a b field. (a, field) t -> f:(a -> b) -> (b, field) t =
   fun t ~f ->
    match t with
    | Pure x ->
        Pure (f x)
    | Direct (d, k) ->
        Direct (d, fun b -> map (k b) ~f)
    | With_label (s, t, k) ->
        With_label (s, t, fun b -> map (k b) ~f)
    | As_prover (x, k) ->
        As_prover (x, map k ~f)
    | Lazy (x, k) ->
        Lazy (x, fun b -> map (k b) ~f)
    | Add_constraint (c, t1) ->
        Add_constraint (c, map t1 ~f)
    | With_handler (h, t, k) ->
        With_handler (h, t, fun b -> map (k b) ~f)
    | Clear_handler (t, k) ->
        Clear_handler (t, fun b -> map (k b) ~f)
    | Exists (typ, c, k) ->
        Exists (typ, c, fun v -> map (k v) ~f)
    | Next_auxiliary k ->
        Next_auxiliary (fun x -> map (k x) ~f)

  let map = `Custom map

  let rec bind :
      type a b field. (a, field) t -> f:(a -> (b, field) t) -> (b, field) t =
   fun t ~f ->
    match t with
    | Pure x ->
        f x
    | Direct (d, k) ->
        Direct (d, fun b -> bind (k b) ~f)
    | With_label (s, t, k) ->
        With_label (s, t, fun b -> bind (k b) ~f)
    | As_prover (x, k) ->
        As_prover (x, bind k ~f)
    | Lazy (x, k) ->
        Lazy (x, fun b -> bind (k b) ~f)
    (* Someday: This case is probably a performance bug *)
    | Add_constraint (c, t1) ->
        Add_constraint (c, bind t1 ~f)
    | With_handler (h, t, k) ->
        With_handler (h, t, fun b -> bind (k b) ~f)
    | Clear_handler (t, k) ->
        Clear_handler (t, fun b -> bind (k b) ~f)
    | Exists (typ, c, k) ->
        Exists (typ, c, fun v -> bind (k v) ~f)
    | Next_auxiliary k ->
        Next_auxiliary (fun x -> bind (k x) ~f)
end

module Types = struct
  module Checked = struct
    type ('a, 'f) t = ('a, 'f) Types.Checked.t
  end

  module As_prover = struct
    type ('a, 'f) t = ('a, 'f) As_prover0.t
  end

  module Typ = struct
    include Types.Typ.T

    type ('var, 'value, 'f) t = ('var, 'value, 'f, (unit, 'f) Checked.t) typ
  end

  module Provider = struct
    include Types.Provider.T

    type ('a, 'f) t =
      (('a Request.t, 'f) As_prover0.t, ('a, 'f) As_prover0.t) provider
  end
end

module Basic :
  Checked_intf.Basic with module Types = Types with type 'f field = 'f = struct
  module Types = Types

  type ('a, 'f) t = ('a, 'f) Types.Checked.t

  type 'f field = 'f

  include Monad_let.Make2 (T0)

  let add_constraint c = Add_constraint (c, return ())

  let as_prover x = As_prover (x, return ())

  let mk_lazy x = Lazy (x, return)

  let with_label lbl x = With_label (lbl, x, return)

  let with_handler h x = With_handler (h, x, return)

  let clear_handler x = Clear_handler (x, return)

  let exists typ p = Exists (typ, p, return)

  let next_auxiliary = Next_auxiliary return

  (** Count the constraints generated in the circuit definition.

      This evaluates the circuit definition, and will run code in the same
      fashion as generating the constraint system.
      The 'weight' parameter defines the contribution of any particular
      constraint to the total count.
  *)
  let rec constraint_count_aux :
      type a f.
         weight:((f field Cvar.t, f field) Constraint.t -> int)
      -> log:(?start:_ -> _)
      -> auxc:_
      -> int
      -> (a, f) Types.Checked.t
      -> int * a =
   fun ~weight ~log ~auxc count t0 ->
    match t0 with
    | Pure x ->
        (count, x)
    | Direct (d, k) ->
        let count = ref count in
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
            { system = None
            ; input = Vector.null
            ; aux = Vector.null
            ; eval_constraints = false
            ; num_inputs = 0
            ; next_auxiliary = ref 1
            ; has_witness = false
            ; stack = []
            ; handler = Request.Handler.fail
            ; is_running = true
            ; as_prover = ref false
            ; log_constraint = Some log_constraint
            }
        in
        let _, x = d state in
        constraint_count_aux ~weight ~log ~auxc !count (k x)
    | As_prover (_x, k) ->
        constraint_count_aux ~weight ~log ~auxc count k
    | Lazy (x, k) ->
        let lazy_count, x = constraint_count_aux ~weight ~log ~auxc count x in
        let forced = ref false in
        let x =
          Lazy.from_fun (fun () ->
              forced := true ;
              x )
        in
        let count, y = constraint_count_aux ~weight ~log ~auxc count (k x) in
        ((if !forced then count + lazy_count else count), y)
    | Add_constraint (c, t) ->
        constraint_count_aux ~weight ~log ~auxc (count + weight c) t
    | Next_auxiliary k ->
        constraint_count_aux ~weight ~log ~auxc count (k !auxc)
    | With_label (s, t, k) ->
        log ~start:true s count ;
        let count', y = constraint_count_aux ~weight ~log ~auxc count t in
        log s count' ;
        constraint_count_aux ~weight ~log ~auxc count' (k y)
    | With_handler (_h, t, k) ->
        let count, x = constraint_count_aux ~weight ~log ~auxc count t in
        constraint_count_aux ~weight ~log ~auxc count (k x)
    | Clear_handler (t, k) ->
        let count, x = constraint_count_aux ~weight ~log ~auxc count t in
        constraint_count_aux ~weight ~log ~auxc count (k x)
    | Exists
        ( Typ
            { size_in_field_elements
            ; var_of_fields
            ; check
            ; constraint_system_auxiliary
            ; _
            }
        , _c
        , k ) ->
        let var =
          var_of_fields
            ( Array.init size_in_field_elements ~f:(fun _ -> Cvar.Var 1)
            , constraint_system_auxiliary () )
        in
        (* TODO: Push a label onto the stack here *)
        let count, () =
          constraint_count_aux ~weight ~log ~auxc count (check var)
        in
        constraint_count_aux ~weight ~log ~auxc count
          (k { Handle.var; value = None })

  let constraint_count (type f)
      ?(weight : ((f field Cvar.t, f field) Constraint.t -> int) option)
      ?(log = fun ?start:_ _ _ -> ()) (t : (_, f field) Types.Checked.t) : int =
    let next_auxiliary = ref 1 in
    let weight = match weight with None -> List.length | Some w -> w in
    fst (constraint_count_aux ~weight ~log ~auxc:next_auxiliary 0 t)
end

module Make
    (Basic : Checked_intf.Basic)
    (As_prover : As_prover_intf.Basic
                   with type ('a, 'f) t = ('a, 'f) Basic.Types.As_prover.t
                    and type 'f field := 'f Basic.field) :
  Checked_intf.S
    with module Types = Basic.Types
    with type 'f field = 'f Basic.field = struct
  include Basic

  let request_witness (typ : ('var, 'value, 'f field) Types.Typ.t)
      (r : ('value Request.t, 'f field) As_prover.t) =
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

module T = struct
  include (
    Make (Basic) (As_prover0) :
        Checked_intf.S
          with module Types := Types
          with type ('a, 'f) t := ('a, 'f) Types.Checked.t
           and type 'f field = 'f )
end

include T
