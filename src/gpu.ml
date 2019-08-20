let has_gpu = true

module Mnt4753_backend :
  Groth16_gpu_prover.Prover_intf with module Backend := Libsnark.Mnt4753 =
struct
  include Groth16_gpu_prover.Mnt4753 (Libsnark.Mnt4753)
end

module Mnt6753_backend :
  Groth16_gpu_prover.Prover_intf with module Backend := Libsnark.Mnt6753 =
struct
  include Groth16_gpu_prover.Mnt6753 (Libsnark.Mnt6753)
end

module Base_proof_system
    (Backend : Groth16_gpu_prover.Prover_intf)
    (Basic : Snark_intf.Basic
             with type field = Backend.Backend.Field.t
              and type Proof.t = Backend.Backend.Default.Proof.t
              and type Proving_key.t = Backend.Backend.Default.Proving_key.t) =
struct
  module Preprocessed_proving_key = struct
    (* Representation of the preprocessed proving key. These values can be
       manipulated by the libsnark bindings (e.g. to store/load from a file),
       but need to be converted raw values before passing to the GPU.

       NOTE: preprocessed values that aren't used by the prover are set to
             [unit] here, so that it's obvious that we don't need them.
    *)
    type t =
      { a: unit
      ; b1: Backend.Backend.G1.Vector.t
      ; b2: Backend.Backend.G2.Vector.t
      ; l: Backend.Backend.G1.Vector.t
      ; h: unit }

    module Raw = struct
      (* Opaque representation of the preprocessed proving key. The 'raw
         vectors' are each a memory region containing only the raw data to be
         passed to the GPU.

         NOTE: preprocessed values that aren't used by the prover are set to
               [unit] here, so that it's obvious that we don't need them.
      *)
      type t =
        { a: unit
        ; b1: Backend.Raw_vector.G1.t
        ; b2: Backend.Raw_vector.G2.t
        ; l: Backend.Raw_vector.G1.t
        ; h: unit }
    end

    let preprocess c pk =
      { a= ()
      ; b1= Backend.Preprocess.b1 c pk
      ; b2= Backend.Preprocess.b2 c pk
      ; l= Backend.Preprocess.l c pk
      ; h= () }

    let to_raw {a= (); b1; b2; l; h= ()} =
      { Raw.a= ()
      ; b1= Backend.Preprocess.reduce_g1_vector b1
      ; b2= Backend.Preprocess.reduce_g2_vector b2
      ; l= Backend.Preprocess.reduce_g1_vector l
      ; h= () }
  end

  module Toplevel = struct
    let gpu_prove ?(message : Basic.Proof.message option) pk
        { Preprocessed_proving_key.Raw.a= ()
        ; b1= b1_mults
        ; b2= b2_mults
        ; l= l_mults
        ; h= () } data_spec state k =
      ignore message ;
      let { Basic.Proof_inputs.public_inputs= public_input
          ; auxiliary_inputs= auxiliary_input } =
        Basic.generate_witness data_spec state k
      in
      Backend.make_groth16_proof ~b1_mults ~b2_mults ~l_mults ~public_input
        ~auxiliary_input pk
  end

  module Perform = struct
    let gpu_prove ~run ?(message : Basic.Proof.message option) pk
        { Preprocessed_proving_key.Raw.a= ()
        ; b1= b1_mults
        ; b2= b2_mults
        ; l= l_mults
        ; h= () } data_spec state k =
      ignore message ;
      let { Basic.Proof_inputs.public_inputs= public_input
          ; auxiliary_inputs= auxiliary_input } =
        Basic.Perform.generate_witness ~run data_spec state k
      in
      Backend.make_groth16_proof ~b1_mults ~b2_mults ~l_mults ~public_input
        ~auxiliary_input pk
  end

  (*module Proof_system = struct
    (* TODO: Expose the proving key from Proof_system and add the relevant
       methods here. *)
    type ('a, 's, 'public_input) t =
    { proof_system: ('a, 's, 'public_input) Basic.Proof_system.t
    ; preprocessed_proving_key: Preprocessed_proving_key.Raw.t option }
  end*)
end

module Make_basic
    (Backend : Groth16_gpu_prover.Prover_intf)
    (Basic : Snark_intf.Basic
             with type field = Backend.Backend.Field.t
              and type Proof.t = Backend.Backend.Default.Proof.t
              and type Proving_key.t = Backend.Backend.Default.Proving_key.t) =
struct
  module GPU = Base_proof_system (Backend) (Basic)
  module Preprocessed_proving_key = GPU.Preprocessed_proving_key

  module Perform = struct
    include Basic.Perform
    include GPU.Perform
  end

  include (
    Basic :
      module type of Basic
      with type field = Basic.field
       and type Bigint.t = Basic.Bigint.t
       and type R1CS_constraint_system.t = Basic.R1CS_constraint_system.t
       and type Var.t = Basic.Var.t
       and type ('a, 'b, 'c, 'd) Data_spec.t =
                  ('a, 'b, 'c, 'd) Basic.Data_spec.t
       and type Field.Vector.t = Basic.Field.Vector.t
       and type Verification_key.t = Basic.Verification_key.t
       and type Proving_key.t = Basic.Proving_key.t
       and type Keypair.t = Basic.Keypair.t
       and type Proof.t = Basic.Proof.t
       and type Proof.message = Basic.Proof.message
       and type Proof_inputs.t = Basic.Proof_inputs.t
       and type 's Runner.state = 's Basic.Runner.state
      with module Perform := Perform )

  include GPU.Toplevel
end

module Make
    (Backend : Groth16_gpu_prover.Prover_intf)
    (Snark : Snark_intf.S
             with type field = Backend.Backend.Field.t
              and type Proof.t = Backend.Backend.Default.Proof.t
              and type Proving_key.t = Backend.Backend.Default.Proving_key.t) =
struct
  module GPU = Base_proof_system (Backend) (Snark)
  module Preprocessed_proving_key = GPU.Preprocessed_proving_key

  module Perform = struct
    include Snark.Perform
    include GPU.Perform
  end

  include (
    Snark :
      module type of Snark
      with type field = Snark.field
       and type Bigint.t = Snark.Bigint.t
       and type R1CS_constraint_system.t = Snark.R1CS_constraint_system.t
       and type Var.t = Snark.Var.t
       and type ('a, 'b, 'c, 'd) Data_spec.t =
                  ('a, 'b, 'c, 'd) Snark.Data_spec.t
       and type Field.Vector.t = Snark.Field.Vector.t
       and type Verification_key.t = Snark.Verification_key.t
       and type Proving_key.t = Snark.Proving_key.t
       and type Keypair.t = Snark.Keypair.t
       and type Proof.t = Snark.Proof.t
       and type Proof.message = Snark.Proof.message
       and type Proof_inputs.t = Snark.Proof_inputs.t
       and type 's Runner.state = 's Snark.Runner.state
      with module Perform := Perform )

  include GPU.Toplevel
end

module Make_Mnt4753_Basic = Make_basic (struct
  module Backend = Libsnark.Mnt4753
  include Mnt4753_backend
end)

module Make_Mnt4753 = Make (struct
  module Backend = Libsnark.Mnt4753
  include Mnt4753_backend
end)

module Make_Mnt6753_Basic = Make_basic (struct
  module Backend = Libsnark.Mnt6753
  include Mnt6753_backend
end)

module Make_Mnt6753 = Make (struct
  module Backend = Libsnark.Mnt6753
  include Mnt6753_backend
end)
