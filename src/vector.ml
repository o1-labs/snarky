open Ctypes
open Foreign
open Core

module type S = sig
  type elt

  type t

  val typ : t Ctypes.typ

  val delete : t -> unit

  val create : unit -> t

  val get : t -> int -> elt

  val emplace_back : t -> elt -> unit

  val length : t -> int
end

module type S_binable = sig
  include S

  include Binable.S with type t := t
end

let with_prefix prefix s = sprintf "%s_%s" prefix s

module Make (Elt : sig
  type t

  val typ : t Ctypes.typ

  val schedule_delete : t -> unit

  val prefix : string
end) : S with type elt = Elt.t = struct
  type elt = Elt.t

  type t = unit ptr

  let typ = ptr void

  let func_name = with_prefix Elt.prefix

  let delete = foreign (func_name "delete") (typ @-> returning void)

  let create =
    let stub = foreign (func_name "create") (void @-> returning typ) in
    fun () ->
      let t = stub () in
      Caml.Gc.finalise delete t ; t

  let get =
    let stub = foreign (func_name "get") (typ @-> int @-> returning Elt.typ) in
    fun t i ->
      let x = stub t i in
      Elt.schedule_delete x ; x

  let length = foreign (func_name "length") (typ @-> returning int)

  let emplace_back =
    foreign (func_name "emplace_back") (typ @-> Elt.typ @-> returning void)
end

module Make_binable (Elt : sig
  type t [@@deriving bin_io]

  val typ : t Ctypes.typ

  val schedule_delete : t -> unit

  val prefix : string
end) : S_binable with type elt = Elt.t = struct
  include Make (Elt)

  module Minmal = struct
    type nonrec t = t

    let arch_sixtyfour = Sys.word_size = 64

    let bin_read_t buf ~pos_ref =
      let open Bin_prot.Common in
      let start_pos = !pos_ref in
      let len = (Bin_prot.Read.bin_read_nat0 buf ~pos_ref :> int) in
      if len = 0 then create ()
      else (
        if arch_sixtyfour then (
          if len > Caml.Sys.max_array_length then
            raise_read_error ReadError.Array_too_long start_pos )
        else if len > Caml.Sys.max_array_length / 2 then
          raise_read_error ReadError.Array_too_long start_pos ;
        let res = create () in
        for _ = 0 to len - 1 do
          let el = Elt.bin_read_t buf ~pos_ref in
          emplace_back res el
        done ;
        res )

    let bin_shape_t =
      let open Bin_prot.Shape in
      basetype (Uuid.of_string "array") [Elt.bin_shape_t]

    let bin_size_len len =
      let plen = Bin_prot.Nat0.unsafe_of_int len in
      Bin_prot.Size.bin_size_nat0 plen

    (* TODO: This could be optimized to allocate less *)
    let bin_size_t_loop ar ~total_len ~n =
      let total_len_ref = ref total_len in
      for i = 0 to n - 1 do
        let el = get ar i in
        total_len_ref := !total_len_ref + Elt.bin_size_t el
      done ;
      !total_len_ref

    let bin_size_t ar =
      let n = length ar in
      let total_len = bin_size_len n in
      bin_size_t_loop ar ~total_len ~n

    let bin_write_t_loop buf ~els_pos ~n ar =
      let els_pos_ref = ref els_pos in
      for i = 0 to n - 1 do
        els_pos_ref := Elt.bin_write_t buf ~pos:!els_pos_ref (get ar i)
      done ;
      !els_pos_ref

    let bin_write_t buf ~pos ar =
      let n = length ar in
      let pn = Bin_prot.Nat0.unsafe_of_int n in
      let els_pos = Bin_prot.Write.bin_write_nat0 buf ~pos pn in
      bin_write_t_loop buf ~els_pos ~n ar

    let __bin_read_t__ _buf ~pos_ref _vint =
      Bin_prot.Common.raise_variant_wrong_type "array" !pos_ref
  end

  include Bin_prot.Utils.Of_minimal (Minmal)
end
