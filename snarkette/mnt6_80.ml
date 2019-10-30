open Core_kernel
open Fields
module N = Nat

module Fq =
  Make_fp
    (N)
    (struct
      let order =
        N.of_string
          "475922286169261325753349249653048451545124878552823515553267735739164647307408490559963137"
    end)

let non_residue = Fq.of_int 5

module Fq3 = struct
  module Params = struct
    let non_residue = non_residue

    let frobenius_coeffs_c1 =
      [| Fq.of_string "1"
       ; Fq.of_string
           "471738898967521029133040851318449165997304108729558973770077319830005517129946578866686956"
       ; Fq.of_string
           "4183387201740296620308398334599285547820769823264541783190415909159130177461911693276180"
      |]

    let frobenius_coeffs_c2 =
      [| Fq.of_string "1"
       ; Fq.of_string
           "4183387201740296620308398334599285547820769823264541783190415909159130177461911693276180"
       ; Fq.of_string
           "471738898967521029133040851318449165997304108729558973770077319830005517129946578866686956"
      |]
  end

  include Make_fp3 (Fq) (Params)
end

module Fq2 =
  Make_fp2
    (Fq)
    (struct
      let non_residue = non_residue
    end)

module Fq6 = struct
  module Params = struct
    let non_residue = non_residue

    let frobenius_coeffs_c1 =
      Array.map ~f:Fq.of_string
        [| "1"
         ; "471738898967521029133040851318449165997304108729558973770077319830005517129946578866686957"
         ; "471738898967521029133040851318449165997304108729558973770077319830005517129946578866686956"
         ; "475922286169261325753349249653048451545124878552823515553267735739164647307408490559963136"
         ; "4183387201740296620308398334599285547820769823264541783190415909159130177461911693276180"
         ; "4183387201740296620308398334599285547820769823264541783190415909159130177461911693276181"
        |]
  end

  include Make_fp6 (N) (Fq) (Fq2) (Fq3) (Params)
end

module G1 = struct
  include Elliptic_curve.Make (N) (Fq)
            (struct
              let a = Fq.of_string "11"

              let b =
                Fq.of_string
                  "106700080510851735677967319632585352256454251201367587890185989362936000262606668469523074"
            end)

  let one : t =
    { x=
        Fq.of_string
          "336685752883082228109289846353937104185698209371404178342968838739115829740084426881123453"
    ; y=
        Fq.of_string
          "402596290139780989709332707716568920777622032073762749862342374583908837063963736098549800"
    ; z= Fq.one }
end

module G2 = struct
  include Elliptic_curve.Make (N) (Fq3)
            (struct
              let a : Fq3.t = (Fq.zero, Fq.zero, G1.Coefficients.a)

              let b : Fq3.t =
                (Fq.(G1.Coefficients.b * Fq3.non_residue), Fq.zero, Fq.zero)
            end)

  let one : t =
    let open Fq in
    { z= Fq3.one
    ; x=
        ( of_string
            "421456435772811846256826561593908322288509115489119907560382401870203318738334702321297427"
        , of_string
            "103072927438548502463527009961344915021167584706439945404959058962657261178393635706405114"
        , of_string
            "143029172143731852627002926324735183809768363301149009204849580478324784395590388826052558"
        )
    ; y=
        ( of_string
            "464673596668689463130099227575639512541218133445388869383893594087634649237515554342751377"
        , of_string
            "100642907501977375184575075967118071807821117960152743335603284583254620685343989304941678"
        , of_string
            "123019855502969896026940545715841181300275180157288044663051565390506010149881373807142903"
        ) }
end

module Pairing_info = struct
  let twist : Fq3.t = Fq.(zero, one, zero)

  let loop_count = N.of_string "689871209842287392837045615510547309923794944"

  let is_loop_count_neg = true

  let final_exponent =
    N.of_string
      "24416320138090509697890595414313438768353977489862543935904010715439066975957855922532159264213056712140358746422742237328406558352706591021642230618060502855451264045397444793186876199015256781648746888625527075466063075011307800862173764236311342105211681121426931616843635215852236649271569251468773714424208521977615548771268520882870120900360322044218806712027729351845307690474985502587527753847200130592058098363641559341826790559426614919168"

  let final_exponent_last_chunk_abs_of_w0 =
    N.of_string "689871209842287392837045615510547309923794944"

  let final_exponent_last_chunk_is_w0_neg = true

  let final_exponent_last_chunk_w1 = N.of_string "1"
end

module Pairing = Pairing.Make (Fq) (Fq3) (Fq6) (G1) (G2) (Pairing_info)

module Groth_maller = Groth_maller.Make (struct
  module N = N
  module G1 = G1
  module G2 = G2
  module Fq = Fq
  module Fqe = Fq3
  module Fq_target = Fq6
  module Pairing = Pairing
end)

module Make_bowe_gabizon (M : sig
  val hash :
       ?message:Fq.t array
    -> a:G1.t
    -> b:G2.t
    -> c:G1.t
    -> delta_prime:G2.t
    -> G1.t
end) =
Bowe_gabizon.Make (struct
  module N = N
  module G1 = G1
  module G2 = G2
  module Fq = Fq
  module Fqe = Fq3
  module Fq_target = Fq6
  module Pairing = Pairing

  let hash = M.hash
end)
