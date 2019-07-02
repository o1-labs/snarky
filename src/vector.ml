open Ctypes
open Core

type 'a t = unit ptr

let null = null

module type Bound = sig
  type 'a return

  type 'a result

  type elt

  type nonrec t = elt t

  val typ : t Ctypes.typ

  val delete : (t -> unit return) result

  val create : (unit -> t return) result

  val get : (t -> int -> elt return) result

  val length : (t -> int return) result

  val emplace_back : (t -> elt -> unit return) result

  val elt_schedule_delete : (elt -> unit return) result
end

let with_prefix prefix s = sprintf "%s_%s" prefix s

module Bind
    (F : Ctypes.FOREIGN) (Elt : sig
        type t

        val typ : t Ctypes.typ

        val schedule_delete : (t -> unit F.return) F.result

        val prefix : string
    end) :
  Bound
  with type 'a return = 'a F.return
   and type 'a result = 'a F.result
   and type elt = Elt.t = struct
  include F

  type elt = Elt.t

  type nonrec t = elt t

  let typ = ptr void

  let func_name = with_prefix Elt.prefix

  let delete = foreign (func_name "delete") (typ @-> returning void)

  let create = foreign (func_name "create") (void @-> returning typ)

  let get = foreign (func_name "get") (typ @-> int @-> returning Elt.typ)

  let length = foreign (func_name "length") (typ @-> returning int)

  let emplace_back =
    foreign (func_name "emplace_back") (typ @-> Elt.typ @-> returning void)

  let elt_schedule_delete = Elt.schedule_delete
end

module type S = sig
  type elt

  type nonrec t = elt t

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

module Make (Bindings : Bound with type 'a return = 'a and type 'a result = 'a) :
  S with type elt = Bindings.elt = struct
  include Bindings

  let create () =
    let t = create () in
    Caml.Gc.finalise delete t ; t

  let get t i =
    let x = get t i in
    elt_schedule_delete x ; x
end

module Make_binable (Elt : sig
  type t [@@deriving bin_io]
end)
(Bindings : Bound
            with type 'a return = 'a
             and type 'a result = 'a
             and type elt = Elt.t) : S_binable with type elt = Elt.t = struct
  include Make (Bindings)

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
