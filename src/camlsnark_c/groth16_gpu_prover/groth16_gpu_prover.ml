open Snarky
open Ctypes
open Foreign

module Ptr () : sig
  type t

  val typ : t typ
end = struct
  type t = unit ptr

  let typ = ptr void
end

module CFile = struct
  module T = Ptr ()

  include T

  let create = foreign "fopen" (string @-> string @-> returning typ)

  let destroy = foreign "fclose" (typ @-> returning void)
end

module Mnt4753 = struct
  (* Linking stub *)
  external _stub : unit -> unit = "mnt4753_cuda_make_proof"

  let _stub = ()

  let load_scalars =
    foreign "mnt4753_load_scalars" (size_t @-> string @-> returning (ptr void))

  module Params = struct
    include Ptr ()

    let load = foreign "mnt4753_groth16_params" (string @-> returning typ)

    let d = foreign "mnt4753_params_d" (typ @-> returning size_t)

    let m = foreign "mnt4753_params_m" (typ @-> returning size_t)
  end

  module Inputs = struct
    include Ptr ()

    let load =
      let stub =
        foreign "mnt4753_groth16_inputs"
          (size_t @-> size_t @-> string @-> returning typ)
      in
      fun ~d ~m filename -> stub d m filename

    let witness =
      foreign "mnt4753_get_input_witness"
        (typ @-> returning Libsnark.Mnt4753.Field.Vector.typ)
  end

  module Preprocess = struct
    module Raw () : sig
      type raw

      val raw : raw typ
    end = struct
      type raw = unit ptr

      let raw = ptr void
    end

    module G1 = Raw ()

    module G2 = Raw ()

    let a =
      let stub =
        foreign "mnt4753_preprocess_A"
          ( int @-> Libsnark.Mnt4753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt4753.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt4753.G1.Vector.delete t ;
        t

    let b1 =
      let stub =
        foreign "mnt4753_preprocess_B1"
          ( int @-> Libsnark.Mnt4753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt4753.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt4753.G1.Vector.delete t ;
        t

    let b2 =
      let stub =
        foreign "mnt4753_preprocess_B2"
          ( int @-> Libsnark.Mnt4753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt4753.G2.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt4753.G2.Vector.delete t ;
        t

    let l =
      let stub =
        foreign "mnt4753_preprocess_L"
          ( int @-> Libsnark.Mnt4753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt4753.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt4753.G1.Vector.delete t ;
        t

    let h =
      let stub =
        foreign "mnt4753_preprocess_H"
          ( int @-> Libsnark.Mnt4753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt4753.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt4753.G1.Vector.delete t ;
        t

    let reduce_g1_vector =
      let stub =
        foreign "mnt4753_reduce_g1_vector"
          (Libsnark.Mnt4753.G1.Vector.typ @-> returning G1.raw)
      in
      let free = foreign "free" (G1.raw @-> returning void) in
      fun x ->
        let t = stub x in
        Caml.Gc.finalise free t ; t

    let reduce_g2_vector =
      let stub =
        foreign "mnt4753_reduce_g2_vector"
          (Libsnark.Mnt4753.G2.Vector.typ @-> returning G2.raw)
      in
      let free = foreign "free" (G2.raw @-> returning void) in
      fun x ->
        let t = stub x in
        Caml.Gc.finalise free t ; t
  end

  let load_points_affine =
    let stub =
      foreign "mnt4753_cuda_load_points_affine"
        (size_t @-> CFile.typ @-> returning Preprocess.G1.raw)
    in
    let release =
      foreign "cuda_release_var_ptr" (Preprocess.G1.raw @-> returning void)
    in
    fun size file ->
      let t = stub size file in
      Caml.Gc.finalise release t ; t

  let load_extension_points_affine =
    let stub =
      foreign "mnt4753_cuda_load_extension_points_affine"
        (size_t @-> CFile.typ @-> returning Preprocess.G2.raw)
    in
    let release =
      foreign "cuda_release_var_ptr" (Preprocess.G2.raw @-> returning void)
    in
    fun size file ->
      let t = stub size file in
      Caml.Gc.finalise release t ; t

  let make_groth16_proof =
    let stub =
      foreign "mnt4753_cuda_make_proof"
        ( Preprocess.G1.raw @-> Preprocess.G2.raw @-> Preprocess.G1.raw
        @-> Libsnark.Mnt4753.Field.Vector.typ
        @-> Libsnark.Mnt4753.Field.Vector.typ
        @-> Libsnark.Mnt4753.Default.Proving_key.typ
        @-> returning Libsnark.Mnt4753.Default.Proof.typ )
    in
    fun ~b1_mults ~b2_mults ~l_mults ~public_input ~auxiliary_input pk ->
      let t = stub b1_mults b2_mults l_mults public_input auxiliary_input pk in
      (* TODO: expose delete on Proof so that we don't leak memory. *)
      (*Caml.Gc.finalise Libsnark.Mnt4753.Default.Proof.delete t;*)
      t
end

module Mnt6753 = struct
  let load_scalars =
    foreign "mnt6753_load_scalars" (size_t @-> string @-> returning (ptr void))

  module Params = struct
    include Ptr ()

    let load = foreign "mnt6753_groth16_params" (string @-> returning typ)

    let d = foreign "mnt6753_params_d" (typ @-> returning size_t)

    let m = foreign "mnt6753_params_m" (typ @-> returning size_t)
  end

  module Inputs = struct
    include Ptr ()

    let load =
      let stub =
        foreign "mnt6753_groth16_inputs"
          (size_t @-> size_t @-> string @-> returning typ)
      in
      fun ~d ~m filename -> stub d m filename

    let witness =
      foreign "mnt6753_get_input_witness"
        (typ @-> returning Libsnark.Mnt6753.Field.Vector.typ)
  end

  module Preprocess = struct
    module Raw () : sig
      type raw

      val raw : raw typ
    end = struct
      type raw = unit ptr

      let raw = ptr void
    end

    module G1 = Raw ()

    module G2 = Raw ()

    let a =
      let stub =
        foreign "mnt6753_preprocess_A"
          ( int @-> Libsnark.Mnt6753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt6753.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt6753.G1.Vector.delete t ;
        t

    let b1 =
      let stub =
        foreign "mnt6753_preprocess_B1"
          ( int @-> Libsnark.Mnt6753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt6753.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt6753.G1.Vector.delete t ;
        t

    let b2 =
      let stub =
        foreign "mnt6753_preprocess_B2"
          ( int @-> Libsnark.Mnt6753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt6753.G2.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt6753.G2.Vector.delete t ;
        t

    let l =
      let stub =
        foreign "mnt6753_preprocess_L"
          ( int @-> Libsnark.Mnt6753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt6753.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt6753.G1.Vector.delete t ;
        t

    let h =
      let stub =
        foreign "mnt6753_preprocess_H"
          ( int @-> Libsnark.Mnt6753.Default.Proving_key.typ
          @-> returning Libsnark.Mnt6753.G1.Vector.typ )
      in
      fun c pk ->
        let t = stub c pk in
        Caml.Gc.finalise Libsnark.Mnt6753.G1.Vector.delete t ;
        t

    let reduce_g1_vector =
      let stub =
        foreign "mnt6753_reduce_g1_vector"
          (Libsnark.Mnt6753.G1.Vector.typ @-> returning G1.raw)
      in
      let free = foreign "free" (G1.raw @-> returning void) in
      fun x ->
        let t = stub x in
        Caml.Gc.finalise free t ; t

    let reduce_g2_vector =
      let stub =
        foreign "mnt6753_reduce_g2_vector"
          (Libsnark.Mnt6753.G2.Vector.typ @-> returning G2.raw)
      in
      let free = foreign "free" (G2.raw @-> returning void) in
      fun x ->
        let t = stub x in
        Caml.Gc.finalise free t ; t
  end

  let load_points_affine =
    let stub =
      foreign "mnt6753_cuda_load_points_affine"
        (size_t @-> CFile.typ @-> returning Preprocess.G1.raw)
    in
    let release =
      foreign "cuda_release_var_ptr" (Preprocess.G1.raw @-> returning void)
    in
    fun size file ->
      let t = stub size file in
      Caml.Gc.finalise release t ; t

  let load_extension_points_affine =
    let stub =
      foreign "mnt6753_cuda_load_extension_points_affine"
        (size_t @-> CFile.typ @-> returning Preprocess.G2.raw)
    in
    let release =
      foreign "cuda_release_var_ptr" (Preprocess.G2.raw @-> returning void)
    in
    fun size file ->
      let t = stub size file in
      Caml.Gc.finalise release t ; t

  let make_groth16_proof =
    let stub =
      foreign "mnt6753_cuda_make_proof"
        ( Preprocess.G1.raw @-> Preprocess.G2.raw @-> Preprocess.G1.raw
        @-> Libsnark.Mnt6753.Field.Vector.typ
        @-> Libsnark.Mnt6753.Field.Vector.typ
        @-> Libsnark.Mnt6753.Default.Proving_key.typ
        @-> returning Libsnark.Mnt6753.Default.Proof.typ )
    in
    fun ~b1_mults ~b2_mults ~l_mults ~public_input ~auxiliary_input pk ->
      let t = stub b1_mults b2_mults l_mults public_input auxiliary_input pk in
      (* TODO: expose delete on Proof so that we don't leak memory. *)
      (*Caml.Gc.finalise Libsnark.Mnt6753.Default.Proof.delete t;*)
      t
end
