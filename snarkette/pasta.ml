open Fields
module N = Nat

module Fq =
  Make_fp
    (N)
    (struct
      let order =
        N.of_string
          "28948022309329048855892746252171976963363056481941647379679742748393362948097"
    end)

module Fp =
  Make_fp
    (N)
    (struct
      let order =
        N.of_string
          "28948022309329048855892746252171976963363056481941560715954676764349967630337"
    end)

module Vesta = struct
  module Params = struct
    let a = Fq.of_string "0"

    let b = Fq.of_string "5"
  end

  include Elliptic_curve.Make (N) (Fq) (Params)

  let one =
    of_affine
      ( Fq.of_string "1"
      , Fq.of_string
          "11426906929455361843568202299992114520848200991084027513389447476559454104162"
      )
end

module Pallas = struct
  module Params = struct
    let a = Fp.of_string "0"

    let b = Fp.of_string "5"
  end

  include Elliptic_curve.Make (N) (Fp) (Params)

  let one =
    of_affine
      ( Fp.of_string "1"
      , Fp.of_string
          "12418654782883325593414442427049395787963493412651469444558597405572177144507"
      )
end
