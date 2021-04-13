open Fields
module N = Nat

module Fq =
  Make_fp
    (N)
    (struct
      let order =
        N.of_string
          "28948022309329048855892746252171976963322203655954433126947083963168578338817"
    end)

module Fp =
  Make_fp
    (N)
    (struct
      let order =
        N.of_string
          "28948022309329048855892746252171976963322203655955319056773317069363642105857"
    end)

module Dee = struct
  module Params = struct
    let a = Fq.of_string "0"

    let b = Fq.of_string "5"
  end

  include Elliptic_curve.Make (N) (Fq) (Params)

  let one =
    of_affine
      ( Fq.of_string "1"
      , Fq.of_string
          "14240188643175251183985684255458419213835105645119662786317263805424119994471"
      )
end

module Dum = struct
  module Params = struct
    let a = Fp.of_string "0"

    let b = Fp.of_string "5"
  end

  include Elliptic_curve.Make (N) (Fp) (Params)

  let one =
    of_affine
      ( Fp.of_string "1"
      , Fp.of_string
          "385654983219305453067387443941241858913435815837190103938162313975739315615"
      )
end
