open Core_kernel

module Read = struct
  module T = struct
    type ('k, 'field) t = Read of 'field Cvar.t * ('field -> 'k)

    let map t ~f = match t with Read (v, k) -> Read (v, fun x -> f (k x))
  end

  include Free_monad.Make2 (T)

  let read v = Free (T.Read (v, return))

  let rec run t f =
    match t with Pure x -> x | Free (T.Read (x, k)) -> run (k (f x)) f

  (** Aggregate [Cvar.t]s used in the underlying monad. *)
  let make_cvars t =
    let rec make_cvars acc t =
      match t with
      | Pure _ ->
          Pure (List.rev acc)
      | Free (T.Read (x, k)) ->
          Free (T.Read (x, fun y -> make_cvars (x :: acc) (k y)))
    in
    make_cvars [] t
end

module Alloc = struct
  module T = struct
    type ('k, 'field) t = Alloc of ('field Cvar.t -> 'k)

    let map t ~f = match t with Alloc k -> Alloc (fun v -> f (k v))
  end

  include Free_monad.Make2 (T)

  let alloc = Free (T.Alloc (fun v -> Pure v))

  let rec run t f =
    match t with Pure x -> x | Free (T.Alloc k) -> run (k (f ())) f

  let size t =
    let dummy = Cvar.Var 0 in
    let rec go acc = function
      | Pure _ ->
          acc
      | Free (T.Alloc k) ->
          go (acc + 1) (k dummy)
    in
    go 0 t
end

module Store = struct
  module T = struct
    type ('k, 'field) t =
      | Store of 'field * ('field Cvar.t -> 'k)
      | Store_later of (('field -> unit) -> 'field Cvar.t -> 'k)

    let map t ~f =
      match t with
      | Store (x, k) ->
          Store (x, fun v -> f (k v))
      | Store_later k ->
          Store_later (fun store v -> f (k store v))
  end

  include Free_monad.Make2 (T)

  let store x = Free (T.Store (x, fun v -> Pure v))

  let store_later = Free (T.Store_later (fun f v -> Pure (f, v)))

  let rec run t f f_later =
    match t with
    | Pure x ->
        x
    | Free (T.Store (x, k)) ->
        run (k (f x)) f f_later
    | Free (T.Store_later k) ->
        let store, v = f_later () in
        run (k store v) f f_later

  let alloc_later_of_read
      (type var field value checked_unit checked_var) ~(zero : field)
      ~(map : checked_unit -> f:(unit -> var) -> checked_var) ~bind
      ~(assert_equal : field Cvar.t -> field Cvar.t -> checked_unit)
      ~(read : var -> (value, field) Read.t) var res =
    Staged.stage (fun x ->
        let rec go read_x read_var res =
          match (read_x, read_var) with
          | Read.Pure _, Read.Pure _ ->
              map res ~f:(fun _ -> x)
          | Read.Free (Read (cvar1, k1)), Read.Free (Read (cvar2, k2)) ->
              go (k1 zero) (k2 zero)
                (bind res ~f:(fun () -> assert_equal cvar1 cvar2))
          | _ ->
              failwith "Inconsistency when reading"
        in
        go (read x) (read var) res)

  let store_later_of_alloc
      (type var field value as_prover_unit as_prover_value checked_unit
      checked_var) ~(zero : field)
      ~(make_checked : (unit -> checked_var) -> checked_var)
      ~(as_prover : (unit -> as_prover_unit) -> checked_unit)
      ~(map : checked_unit -> f:(unit -> var) -> checked_var) ~bind
      ~(prover_read : (var -> (value, field) Read.t) -> var -> as_prover_value)
      ~(prover_map : as_prover_value -> f:(value -> unit) -> as_prover_unit)
      ~(assert_equal : field Cvar.t -> field Cvar.t -> checked_unit)
      ~(alloc : (var, field) Alloc.t) ~(store : value -> (var, field) t)
      ~(read : var -> (value, field) Read.t) () =
    let stores = ref [] in
    let rec store_later_of_alloc alloc =
      match alloc with
      | Alloc.Pure x ->
          let fn var =
            let rem_stores = ref (List.rev !stores) in
            let full_stores = ref [] in
            let pop () =
              match !rem_stores with
              | [] ->
                  failwith "More field elements allocated by store than in alloc"
              | (store, cvar) :: rest ->
                  rem_stores := rest ;
                  (store, cvar)
            in
            let store_now x =
              let store, cvar = pop () in
              full_stores := (cvar, Some x) :: !full_stores ;
              store x ;
              cvar
            in
            let store_later () =
              let store, cvar = pop () in
              full_stores := (cvar, None) :: !full_stores ;
              (store, cvar)
            in
            make_checked (fun () ->
                Staged.unstage
                  (alloc_later_of_read ~zero ~map ~bind ~assert_equal ~read var
                     (as_prover (fun () ->
                          prover_map (prover_read read var) ~f:(fun value ->
                              let _res =
                                run (store value) store_now store_later
                              in
                              match !stores with
                              | [] ->
                                  ()
                              | _ :: _ ->
                                  failwith
                                    "More field elements allocated by alloc than \
                                     in store"))))
                  x)
          in
          Pure fn
      | Alloc.Free (Alloc.T.Alloc k) ->
          Free
            (T.Store_later
               (fun store cvar ->
                 stores := (store, cvar) :: !stores ;
                 store_later_of_alloc (k cvar)))
    in
    store_later_of_alloc alloc
end
