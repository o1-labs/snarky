open Snarky_curve

let%test_unit "mnt4" =
  let module T = For_native_base_field (struct
    module Impl =
      Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core.Unit)

    module F = struct
      include (
        Impl.Field :
          module type of Impl.Field with module Constant := Impl.Field.Constant )

      module Constant = struct
        include Impl.Field.Constant

        let inv_exn = inv
      end

      let assert_r1cs a b c = Impl.assert_r1cs a b c

      let assert_square a b = Impl.assert_square a b

      let negate x = zero - x

      let inv_exn = inv
    end

    module Params = struct
      let one = Snarky.Backends.Mnt6.G1.(to_affine_exn one)

      include Snarky.Backends.Mnt6.G1.Coefficients

      let group_size_in_bits = Snarky.Backends.Mnt6.Field.size_in_bits
    end

    module Constant = Snarky.Backends.Mnt6.G1
  end) in
  ()
