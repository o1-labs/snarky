module type S = sig
  module Impl : Snarky.Snark_intf.Run

  open Impl

  module type Input = sig
    type t

    module Constant : sig
      type t [@@deriving of_yojson]
    end

    val typ : (t, Constant.t) Typ.t
  end

  type ('var, 'value) input =
    (module Input with type t = 'var and type Constant.t = 'value)

  type ('r_var, 'r_value, 'k_var, 'k_value) t =
    | ( :: ) :
        ('var, 'value) input * ('r_var, 'r_value, 'k_var, 'k_value) t
        -> ('r_var, 'r_value, 'var -> 'k_var, 'value -> 'k_value) t
    | [] : ('r_var, 'r_value, 'r_var, 'r_value) t

  module type Witness_intf = sig
    type t

    module Constant : sig
      type t [@@deriving of_yojson]
    end

    val typ : (t, Constant.t) Typ.t
  end

  val run_main :
       (unit -> 'result, unit, 'computation, 'public_input) t
    -> (Yojson.Safe.json -> (Impl.prover_state, string) result)
    -> 'computation
    -> unit
end

module Make (Impl : Snarky.Snark_intf.Run) : S with module Impl := Impl =
struct
  open Impl

  module type Input = sig
    type t

    module Constant : sig
      type t [@@deriving of_yojson]
    end

    val typ : (t, Constant.t) Typ.t
  end

  type ('var, 'value) input =
    (module Input with type t = 'var and type Constant.t = 'value)

  type ('r_var, 'r_value, 'k_var, 'k_value) t =
    | ( :: ) :
        ('var, 'value) input * ('r_var, 'r_value, 'k_var, 'k_value) t
        -> ('r_var, 'r_value, 'var -> 'k_var, 'value -> 'k_value) t
    | [] : ('r_var, 'r_value, 'r_var, 'r_value) t

  let rec read_input' : type r_var k_var k_value.
         (r_var, unit, k_var, k_value) t
      -> Yojson.Safe.json list
      -> (unit, k_value) Snarky.H_list.t =
   fun t json ->
    match (t, json) with
    | [], [] ->
        []
    | (module I) :: t, j :: json -> (
      match I.Constant.of_yojson j with
      | Ok res ->
          res :: read_input' t json
      | Error e ->
          failwith e )
    | _ ->
        failwith "Bad JSON for input"

  let read_input : type r_var k_var k_value.
         (r_var, unit, k_var, k_value) t
      -> Yojson.Safe.json
      -> (unit, k_value) Snarky.H_list.t =
   fun t json ->
    match json with
    | `List xs ->
        read_input' t xs
    | _ ->
        failwith "Bad JSON for input: expected list"

  let rec to_data_spec : type r_var r_value k_var k_value.
         (r_var, r_value, k_var, k_value) t
      -> (r_var, r_value, k_var, k_value) Data_spec.t = function
    | [] ->
        []
    | (module I) :: t ->
        I.typ :: to_data_spec t

  module type Witness_intf = sig
    type t

    module Constant : sig
      type t [@@deriving of_yojson]
    end

    val typ : (t, Constant.t) Typ.t
  end

  let run_main (type result computation public_input)
      (input : (unit -> result, unit, computation, public_input) t)
      (read_prover_state :
        Yojson.Safe.json -> (Impl.prover_state, string) Stdlib.result)
      (main : computation) =
    let module F =
      Snarky.Toplevel.Make_json
        (Impl)
        (struct
          type nonrec result = result

          type nonrec computation = computation

          type nonrec public_input = public_input

          let public_input = to_data_spec input

          let read_input = read_input input

          let read_prover_state = read_prover_state

          let main = main
        end)
    in
    F.main ()
end
