open Core_kernel
open Group_map

module Fp = struct
  include
    Snarkette.Fields.Make_fp
      (Snarkette.Nat)
      (struct
        let order =
          Snarkette.Nat.of_string
            "5543634365110765627805495722742127385843376434033820803590214255538854698464778703795540858859767700241957783601153"
      end)

  let b = of_int 7
end

module Make_tests (F : sig
  include Field_intf.S_unchecked

  val gen : t Quickcheck.Generator.t

  val b : t
end) =
struct
  module F = struct
    include F

    let constant = Fn.id
  end

  let params : F.t Group_map.Bw19.Params.t =
    Bw19.Params.create (module F) { Bw19.Spec.b = F.b }

  let curve_eqn (u : F.t) : F.t =
    let open F in
    (u * u * u) + F.b

  (* Filter the two points which cause the group-map to blow up. This
     is not an issue in practice because the points we feed into this function
     will be the output of poseidon, and thus (modeling poseidon as a random oracle)
     will not be either of those two points. *)
  let gen =
    let open F in
    Quickcheck.Generator.filter F.gen ~f:(fun t ->
        let t2 = t * t in
        let fu_val = match params with { Bw19.Params.fu; _ } -> fu in
        let alpha_inv = (t2 + constant fu_val) * t2 in
        not (equal alpha_inv zero) )

  module M =
    Bw19.Make (F) (F)
      (struct
        let params = params
      end)

  let test_full_map_works () =
    Quickcheck.test ~sexp_of:F.sexp_of_t gen ~f:(fun t ->
        let open F in
        let (x, y) : F.t * F.t = Bw19.to_group (module F) ~params t in
        let res : F.t = curve_eqn x in
        Alcotest.(check bool)
          "curve equation holds for mapped point" true
          (equal res (y * y)) )
end

module T0 = Make_tests (Fp)

let () =
  Alcotest.run "Group_map"
    [ ( "Bw19"
      , [ Alcotest.test_case "full map works" `Quick T0.test_full_map_works ] )
    ]
