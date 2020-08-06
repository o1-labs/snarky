open Core_kernel
open Snarky

let%test_module "snark0-test" =
  ( module struct
    open Snarky_libsnark_bindings

    let bin_io_id m = Fn.compose (Binable.of_string m) (Binable.to_string m)

    let swap b (x, y) = if b then (y, x) else (x, y)

    module Run (Backend : Backend_intf.S) = struct
      include Snark0.Make (Backend)

      let main x =
        let open Checked.Let_syntax in
        let%bind y = exists Field.typ ~compute:(As_prover.return Field.zero) in
        let rec go b acc i =
          if i = 0 then return acc
          else
            let%bind z =
              Tuple2.uncurry Field.Checked.mul
                (swap b (Field.Var.add y acc, x))
            in
            go b z (i - 1)
        in
        let%bind _ = go false x 19 in
        let%bind _ = go true y 20 in
        return ()

      let kp = generate_keypair ~exposing:[Field.typ] main

      let%test_unit "proving" =
        let input = Field.one in
        let proof = prove (Keypair.pk kp) [Field.typ] () main input in
        assert (verify proof (Keypair.vk kp) [Field.typ] input)

      let%test_unit "key serialization" =
        let vk = Keypair.vk kp |> bin_io_id (module Verification_key) in
        let pk = Keypair.pk kp |> bin_io_id (module Proving_key) in
        let input = Field.one in
        let proof = prove pk [Field.typ] () main input in
        assert (verify proof vk [Field.typ] input)
    end

    module M0 = Run (Backends.Mnt4.Default)

    module M1 = Run (struct
      module Full = Backends.Mnt4
      module Field = Full.Field
      module Bigint = Full.Bigint
      module Var = Full.Var

      module R1CS_constraint_system = struct
        include Full.R1CS_constraint_system

        let finalize = swap_AB_if_beneficial
      end

      let field_size = Full.field_size

      include Libsnark.Make_bowe_gabizon
                (Backends.Mnt4)
                (struct
                  let hash ?message:_ ~a:_ ~b:_ ~c:_ ~delta_prime:_ =
                    Backends.Mnt4.G1.one
                end)
    end)
  end )
