open Core_kernel
open Fold_lib
open Tuple_lib

module type S = sig
  type curve

  module Digest : sig
    (* TODO: version *)
    type t [@@deriving bin_io, sexp, eq, compare]

    val ( = ) : t -> t -> bool
  end

  module Params : sig
    type t = curve Quadruple.t array

    val load : string -> t
  end

  module State : sig
    type t = {triples_consumed: int; acc: curve; params: Params.t}

    val create : ?triples_consumed:int -> ?init:curve -> Params.t -> t

    val update_fold : t -> bool Triple.t Fold.t -> t

    val digest : t -> Digest.t

    val salt : Params.t -> string -> t
  end

  val hash_fold : State.t -> bool Triple.t Fold.t -> State.t

  val digest_fold : State.t -> bool Triple.t Fold.t -> Digest.t
end

module type Inputs_intf = sig
  module Field : sig
    type t [@@deriving sexp, bin_io, eq, compare]

    include Stringable.S with type t := t
  end

  module Curve : sig
    type t [@@deriving sexp]

    val to_affine_coordinates : t -> Field.t * Field.t

    val of_affine_coordinates : Field.t * Field.t -> t

    val zero : t

    val ( + ) : t -> t -> t

    val negate : t -> t
  end
end

module Make (Inputs : Inputs_intf) :
  S with type curve := Inputs.Curve.t and type Digest.t = Inputs.Field.t =
struct
  open Inputs

  module Digest = struct
    type t = Field.t [@@deriving sexp, bin_io, eq, compare]

    let ( = ) = equal
  end

  module Params = struct
    type t = Curve.t Quadruple.t array

    let load path =
      let comma = ',' in
      let semi_colon = ';' in
      let err =
        "Bad params. Each line should consist of 4 ; separated pairs."
      in
      let read_pair s =
        match String.split ~on:comma s with
        | [x; y] ->
            Curve.of_affine_coordinates (Field.of_string x, Field.of_string y)
        | _ ->
            failwith err
      in
      let strs = Array.of_list (In_channel.read_lines path) in
      Array.map
        ~f:(fun s ->
          match List.map ~f:read_pair (String.split ~on:semi_colon s) with
          | [x1; x2; x3; x4] ->
              (x1, x2, x3, x4)
          | _ ->
              failwith err )
        strs
  end

  module State = struct
    type t = {triples_consumed: int; acc: Curve.t; params: Params.t}

    let create ?(triples_consumed = 0) ?(init = Curve.zero) params =
      {acc= init; triples_consumed; params}

    let local_function params i triple =
      let g = params.(i) in
      let a0, a1, sign = triple in
      let res = Quadruple.get g (Four.of_bits_lsb (a0, a1)) in
      if sign then Curve.negate res else res

    let update_fold (t : t) (fold : bool Triple.t Fold.t) =
      let params = t.params in
      let acc, triples_consumed =
        fold.fold ~init:(t.acc, t.triples_consumed) ~f:(fun (acc, i) triple ->
            let term = local_function params i triple in
            (Curve.(acc + term), i + 1) )
      in
      {t with acc; triples_consumed}

    let digest t =
      let x, _y = Curve.to_affine_coordinates t.acc in
      x

    let salt params s = update_fold (create params) (Fold.string_triples s)
  end

  let hash_fold s fold = State.update_fold s fold

  let digest_fold s fold = State.digest (hash_fold s fold)
end
