module type S = sig
  module Impl : Snarky.Snark_intf.Run with type prover_state = unit

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
       (unit -> unit, unit, 'arg0 -> 'computation0, 'public_input) t
    -> (module Witness_intf with type t = 'witness)
    -> ('witness -> 'arg0 -> 'computation0)
    -> unit
end

module Make (Impl : Snarky.Snark_intf.Run with type prover_state = unit) :
  S with module Impl := Impl = struct
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
      -> Yojson.Safe.t list
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
      -> Yojson.Safe.t
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

  let run_main (type witness arg0 computation0 public_input)
      (input : (unit -> unit, unit, arg0 -> computation0, public_input) t)
      (module Witness : Witness_intf with type t = witness)
      (main : witness -> arg0 -> computation0) =
    let module F =
      Snarky.Toplevel.Make_json
        (Impl)
        (struct
          type nonrec public_input = public_input

          type nonrec arg0 = arg0

          type nonrec computation0 = computation0

          type computation = arg0 -> computation0

          let public_input = to_data_spec input

          let read_input = read_input input

          module Witness = Witness

          let main = main
        end)
    in
    F.main ()
end
