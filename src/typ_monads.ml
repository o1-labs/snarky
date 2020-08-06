open Snarky_backendless

module Store = struct
  module T = struct
    type ('k, 'field) t = Store of 'field * ('field Cvar.t -> 'k)

    let map t ~f = match t with Store (x, k) -> Store (x, fun v -> f (k v))
  end

  include Free_monad.Make2 (T)

  let store x = Free (T.Store (x, fun v -> Pure v))

  let rec run t f =
    match t with Pure x -> x | Free (T.Store (x, k)) -> run (k (f x)) f
end

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
