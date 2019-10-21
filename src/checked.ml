open Core_kernel
open Types.Checked

type ('a, 's, 'field) t = ('a, 's, 'field) Types.Checked.t

module T0 = struct
  type nonrec ('a, 's, 'field) t = ('a, 's, 'field) t

  let return x = Pure x

  let rec map : type s a b field.
      (a, s, field) t -> f:(a -> b) -> (b, s, field) t =
   fun t ~f ->
    match t with
    | Pure x ->
        Pure (f x)
    | Direct (d, k) ->
        Direct (d, fun b -> map (k b) ~f)
    | Reduced (t, d, res, k) ->
        Reduced (t, d, res, fun b -> map (k b) ~f)
    | With_label (s, t, k) ->
        With_label (s, t, fun b -> map (k b) ~f)
    | As_prover (x, k) ->
        As_prover (x, map k ~f)
    | Lazy (x, k) ->
        Lazy (x, fun b -> map (k b) ~f)
    | Add_constraint (c, t1) ->
        Add_constraint (c, map t1 ~f)
    | With_state (p, and_then, t_sub, k) ->
        With_state (p, and_then, t_sub, fun b -> map (k b) ~f)
    | With_handler (h, t, k) ->
        With_handler (h, t, fun b -> map (k b) ~f)
    | Clear_handler (t, k) ->
        Clear_handler (t, fun b -> map (k b) ~f)
    | Exists (typ, c, k) ->
        Exists (typ, c, fun v -> map (k v) ~f)
    | Next_auxiliary k ->
        Next_auxiliary (fun x -> map (k x) ~f)

  let map = `Custom map

  let rec bind : type s a b field.
      (a, s, field) t -> f:(a -> (b, s, field) t) -> (b, s, field) t =
   fun t ~f ->
    match t with
    | Pure x ->
        f x
    | Direct (d, k) ->
        Direct (d, fun b -> bind (k b) ~f)
    | Reduced (t, d, res, k) ->
        Reduced (t, d, res, fun b -> bind (k b) ~f)
    | With_label (s, t, k) ->
        With_label (s, t, fun b -> bind (k b) ~f)
    | As_prover (x, k) ->
        As_prover (x, bind k ~f)
    | Lazy (x, k) ->
        Lazy (x, fun b -> bind (k b) ~f)
    (* Someday: This case is probably a performance bug *)
    | Add_constraint (c, t1) ->
        Add_constraint (c, bind t1 ~f)
    | With_state (p, and_then, t_sub, k) ->
        With_state (p, and_then, t_sub, fun b -> bind (k b) ~f)
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
    type ('a, 's, 'f) t = ('a, 's, 'f) Types.Checked.t
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

module Basic :
  Checked_intf.Basic with module Types = Types with type 'f field = 'f = struct
  module Types = Types

  type ('a, 's, 'f) t = ('a, 's, 'f) Types.Checked.t

  type 'f field = 'f

  include Monad_let.Make3 (T0)

  let add_constraint c = Add_constraint (c, return ())

  let as_prover x = As_prover (x, return ())

  let mk_lazy x = Lazy (x, return)

  let with_label lbl x = With_label (lbl, x, return)

  let with_state p and_then sub = With_state (p, and_then, sub, return)

  let with_handler h x = With_handler (h, x, return)

  let clear_handler x = Clear_handler (x, return)

  let exists typ p = Exists (typ, p, return)

  let next_auxiliary = Next_auxiliary return

  let rec with_lens : type a whole view.
      (whole, view) Lens.t -> (a, view, 'field) t -> (a, whole, 'field) t =
   fun lens t ->
    match t with
    | Pure x ->
        Pure x
    | Direct (d, k) ->
        let d rs =
          let s = rs.Run_state.prover_state in
          let s' = Option.map ~f:(Lens.get lens) s in
          let rs, a = d (Run_state.set_prover_state s' rs) in
          let s = Option.map2 ~f:(Lens.set lens) s s' in
          (Run_state.set_prover_state s rs, a)
        in
        Direct (d, fun b -> with_lens lens (k b))
    | Reduced (t, d, res, k) ->
        let d rs =
          let s = rs.Run_state.prover_state in
          let s' = Option.map ~f:(Lens.get lens) s in
          let rs = d (Run_state.set_prover_state s' rs) in
          let s = Option.map2 ~f:(Lens.set lens) s s' in
          Run_state.set_prover_state s rs
        in
        Reduced (with_lens lens t, d, res, fun b -> with_lens lens (k b))
    | With_label (s, t, k) ->
        With_label (s, with_lens lens t, fun b -> with_lens lens (k b))
    | As_prover (x, k) ->
        As_prover (As_prover0.with_lens lens x, with_lens lens k)
    | Lazy (x, k) ->
        Lazy (x, fun b -> with_lens lens (k b))
    | Add_constraint (c, t1) ->
        Add_constraint (c, with_lens lens t1)
    | With_state (p, and_then, t_sub, k) ->
        With_state
          ( As_prover0.with_lens lens p
          , (fun s -> As_prover0.with_lens lens (and_then s))
          , t_sub
          , fun b -> with_lens lens (k b) )
    | With_handler (h, t, k) ->
        With_handler (h, with_lens lens t, fun b -> with_lens lens (k b))
    | Clear_handler (t, k) ->
        Clear_handler (with_lens lens t, fun b -> with_lens lens (k b))
    | Exists (typ, c, k) ->
        Exists
          ( typ
          , As_prover0.Provider.with_lens lens c
          , fun b -> with_lens lens (k b) )
    | Next_auxiliary k ->
        Next_auxiliary (fun b -> with_lens lens (k b))

  let rec constraint_count_aux : type a s.
         log:(?start:_ -> _)
      -> auxc:_
      -> int
      -> (a, s, _) Types.Checked.t
      -> int * a =
   fun ~log ~auxc count t0 ->
    match t0 with
    | Pure x ->
        (count, x)
    | Direct (d, k) ->
        let count = ref count in
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
        let _, x = d state in
        constraint_count_aux ~log ~auxc !count (k x)
    | Reduced (t, _, _, k) ->
        let count, y = constraint_count_aux ~log ~auxc count t in
        constraint_count_aux ~log ~auxc count (k y)
    | As_prover (_x, k) ->
        constraint_count_aux ~log ~auxc count k
    | Lazy (x, k) ->
        let lazy_count, x = constraint_count_aux ~log ~auxc count x in
        let forced = ref false in
        let x =
          Lazy.from_fun (fun () ->
              forced := true ;
              x )
        in
        let count, y = constraint_count_aux ~log ~auxc count (k x) in
        ((if !forced then count + lazy_count else count), y)
    | Add_constraint (c, t) ->
        constraint_count_aux ~log ~auxc (count + List.length c) t
    | Next_auxiliary k ->
        constraint_count_aux ~log ~auxc count (k !auxc)
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
        let alloc_var () = Cvar.Var 1 in
        let var = Typ_monads.Alloc.run alloc alloc_var in
        (* TODO: Push a label onto the stack here *)
        let count, () = constraint_count_aux ~log ~auxc count (check var) in
        constraint_count_aux ~log ~auxc count (k {Handle.var; value= None})

  let constraint_count ?(log = fun ?start:_ _ _ -> ())
      (t : (_, _, _) Types.Checked.t) : int =
    let next_auxiliary = ref 1 in
    fst (constraint_count_aux ~log ~auxc:next_auxiliary 0 t)
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

module T = struct
  include (
    Make (Basic) (As_prover0) :
      Checked_intf.S
      with module Types := Types
      with type ('a, 's, 'f) t := ('a, 's, 'f) Types.Checked.t
       and type 'f field = 'f )
end

include T
