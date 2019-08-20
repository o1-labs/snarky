open Ctypes
open Foreign

(* Linking stub *)
external _stub : unit -> unit = "mnt4753_cuda_make_proof"

let _stub = ()

module type Foreign_intf = sig
  type t

  val typ : t typ
end

module type Deletable_intf = sig
  include Foreign_intf

  val delete : t -> unit
end

module type Foreign_vector_intf = sig
  include Deletable_intf

  module Vector : Deletable_intf with type t = t Camlsnark_c_bindings.Vector.t
end

module Make_foreign (X : sig end) : Foreign_intf = struct
  type t = unit ptr

  let typ = ptr void
end

module Ptr (X : sig end) : sig
  type _ t

  module Make (M : sig
    type t
  end) : sig
    type nonrec t = M.t t

    val typ : t typ
  end
end = struct
  type _ t = unit ptr

  module Make (M : sig end) = struct
    type t = unit ptr

    let typ = ptr void
  end
end

module Raw = Ptr ()

module Params = Ptr ()

module Inputs = Ptr ()

module CFile = struct
  include Make_foreign ()

  let create = foreign "fopen" (string @-> string @-> returning typ)

  let destroy = foreign "fclose" (typ @-> returning void)
end

module type Snarky_intf = sig
  module Field : Foreign_vector_intf

  module G1 : Foreign_vector_intf

  module G2 : Foreign_vector_intf

  module Default : sig
    module Proving_key : Foreign_intf

    module Proof : Deletable_intf
  end
end

module Make (M : sig
  val prefix : string
end)
(Backend : Snarky_intf) =
struct
  let with_prefix s = M.prefix ^ "_" ^ s

  let foreign_prefixed s = foreign (with_prefix s)

  let load_scalars =
    foreign_prefixed "load_scalars" (size_t @-> string @-> returning (ptr void))

  module Params = struct
    include Params.Make (Backend.Field)

    let load = foreign_prefixed "groth16_params" (string @-> returning typ)

    let d = foreign_prefixed "params_d" (typ @-> returning size_t)

    let m = foreign_prefixed "params_m" (typ @-> returning size_t)
  end

  module Inputs = struct
    include Inputs.Make (Backend.Field)

    let load =
      let stub =
        foreign_prefixed "groth16_inputs"
          (size_t @-> size_t @-> string @-> returning typ)
      in
      fun ~d ~m filename -> stub d m filename

    let witness =
      foreign_prefixed "get_input_witness"
        (typ @-> returning Backend.Field.Vector.typ)
  end

  module Raw_vector = struct
    module G1 = Raw.Make (Backend.G1)
    module G2 = Raw.Make (Backend.G2)
  end

  module Preprocess = struct
    let a =
      let stub =
        foreign_prefixed "preprocess_A"
          ( int @-> Backend.Default.Proving_key.typ
          @-> returning Backend.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Backend.G1.Vector.delete t ;
        t

    let b1 =
      let stub =
        foreign_prefixed "preprocess_B1"
          ( int @-> Backend.Default.Proving_key.typ
          @-> returning Backend.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Backend.G1.Vector.delete t ;
        t

    let b2 =
      let stub =
        foreign_prefixed "preprocess_B2"
          ( int @-> Backend.Default.Proving_key.typ
          @-> returning Backend.G2.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Backend.G2.Vector.delete t ;
        t

    let l =
      let stub =
        foreign_prefixed "preprocess_L"
          ( int @-> Backend.Default.Proving_key.typ
          @-> returning Backend.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Backend.G1.Vector.delete t ;
        t

    let h =
      let stub =
        foreign_prefixed "preprocess_H"
          ( int @-> Backend.Default.Proving_key.typ
          @-> returning Backend.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Backend.G1.Vector.delete t ;
        t

    let reduce_g1_vector =
      let stub =
        foreign_prefixed "reduce_g1_vector"
          (Backend.G1.Vector.typ @-> returning Raw_vector.G1.typ)
      in
      (* Use free: the memory is [malloc]ed. *)
      let free = foreign "free" (Raw_vector.G1.typ @-> returning void) in
      fun x ->
        let t = stub x in
        Caml.Gc.finalise free t ; t

    let reduce_g2_vector =
      let stub =
        foreign_prefixed "reduce_g2_vector"
          (Backend.G2.Vector.typ @-> returning Raw_vector.G2.typ)
      in
      (* Use free: the memory is [malloc]ed. *)
      let free = foreign "free" (Raw_vector.G2.typ @-> returning void) in
      fun x ->
        let t = stub x in
        Caml.Gc.finalise free t ; t
  end

  let load_points_affine =
    let stub =
      foreign_prefixed "cuda_load_points_affine"
        (size_t @-> CFile.typ @-> returning Raw_vector.G1.typ)
    in
    let release =
      foreign "cuda_release_var_ptr" (Raw_vector.G1.typ @-> returning void)
    in
    fun size file ->
      let t = stub size file in
      Caml.Gc.finalise release t ; t

  let load_extension_points_affine =
    let stub =
      foreign_prefixed "cuda_load_extension_points_affine"
        (size_t @-> CFile.typ @-> returning Raw_vector.G2.typ)
    in
    let release =
      foreign "cuda_release_var_ptr" (Raw_vector.G2.typ @-> returning void)
    in
    fun size file ->
      let t = stub size file in
      Caml.Gc.finalise release t ; t

  let make_groth16_proof =
    let stub =
      foreign_prefixed "cuda_make_proof"
        ( Raw_vector.G1.typ @-> Raw_vector.G2.typ @-> Raw_vector.G1.typ
        @-> Backend.Field.Vector.typ @-> Backend.Field.Vector.typ
        @-> Backend.Default.Proving_key.typ
        @-> returning Backend.Default.Proof.typ )
    in
    fun ~b1_mults ~b2_mults ~l_mults ~public_input ~auxiliary_input pk ->
      let t = stub b1_mults b2_mults l_mults public_input auxiliary_input pk in
      Caml.Gc.finalise Backend.Default.Proof.delete t ;
      t
end

module Mnt4753 = Make (struct
  let prefix = "mnt4753"
end)

module Mnt6753 = Make (struct
  let prefix = "mnt6753"
end)
