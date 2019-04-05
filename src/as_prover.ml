open Core_kernel

type ('a, 'f, 's) t = ('a, 'f, 's) As_prover0.t

module Make
    (Checked : Checked_intf.S)
    (As_prover : As_prover_intf.Basic
                 with type 'f field := 'f Checked.field
                  and module Types := Checked.Types) =
struct
  type 'f field = 'f Checked.field

  module Types = Checked.Types

  include As_prover

  include Monad_let.Make3 (struct
    include As_prover

    let map = `Custom map
  end)

  let get_state () = wrap (fun s -> (s, s))

  let set_state s = wrap (fun _ -> (s, ()))

  let modify_state f = wrap (fun s -> (f s, ()))

  let map2 x y ~f =
    let%map x = x and y = y in
    f x y

  let read_var v = with_read (fun read_var -> read_var v)

  let read ({read; _} : ('var, 'value, 'f field) Types.Typ.t) (var : 'var) :
      ('value, 'f field, 'prover_state) t =
    with_read (fun read_var -> Typ_monads.Read.run (read var) read_var)

  module Ref = struct
    type 'a t = 'a option ref

    let create (x : ('a, 'f field, 's) Types.As_prover.t) :
        ('a t, 's, 'f field) Checked.t =
      let r = ref None in
      let open Checked in
      let%map () =
        Checked.as_prover (As_prover.map x ~f:(fun x -> r := Some x))
      in
      r

    let get (r : 'a t) = wrap (fun s -> (s, Option.value_exn !r))

    let set (r : 'a t) x = wrap (fun s -> (s, (r := Some x)))
  end

  module Provider = struct
    include Types.Provider

    let run t stack tbl s (handler : Request.Handler.t) =
      match t with
      | Request rc ->
          let s', r = run rc tbl s in
          (s', Request.Handler.run handler stack r)
      | Compute c -> run c tbl s
      | Both (rc, c) -> (
          let s', r = run rc tbl s in
          match Request.Handler.run handler stack r with
          | exception _ -> run c tbl s
          | x -> (s', x) )
  end
end

module T :
  As_prover_intf.S' with type 'f field := 'f with module Types := Checked.Types =
  Make
    (Checked)
    (struct
      include As_prover0

      type 'f field = 'f

      module Types = Checked.Types
    end)

include T

module Make_extended (Env : sig
  type field
end)
(As_prover : As_prover_intf.S with type 'f field := Env.field) =
struct
  type ('a, 's) t = ('a, Env.field, 's) As_prover.t

  type ('a, 's) as_prover = ('a, 's) t

  include Env

  include (
    As_prover :
      As_prover_intf.S'
      with type 'f field := field
      with module Types = As_prover.Types )
end
