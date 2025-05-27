open Core_kernel
open Group_map

module Fp = struct
  include
    Snarkette.Fields.Make_fp
      (Snarkette.Nat)
      (struct
        let order = Snarkette.Nat.of_int 100003
      end)

  let a = of_int 1

  let b = of_int 3
end

module F13 = struct
  type t = int [@@deriving sexp]

  let p = 13

  let ( + ) x y = (x + y) mod p

  let ( * ) x y = x * y mod p

  let negate x = (p - x) mod p

  let ( - ) x y = (x - y + p) mod p

  let equal = Int.equal

  let ( / ) x y =
    let rec go i = if equal x (i * y) then i else go (i + 1) in
    if equal y 0 then failwith "Divide by 0" else go 1

  let sqrt' x =
    let rec go i =
      if Int.equal i p then None
      else if equal (i * i) x then Some i
      else go Int.(i + 1)
    in
    go 0

  let sqrt x = Option.value_exn (sqrt' x)

  let is_square x = Option.is_some (sqrt' x)

  let zero = 0

  let one = 1

  let of_int = Fn.id

  let gen = Int.gen_incl 0 Int.(p - 1)

  let a = 1

  let b = 3
end

module Make_tests (F : sig
  include Field_intf.S_unchecked

  val gen : t Quickcheck.Generator.t

  val a : t

  val b : t
end) =
struct
  module F = struct
    include F

    let constant = Fn.id
  end

  open F

  let spec = { Spec.a = F.a; b = F.b }

  let params = Params.create (module F) spec

  let curve_eqn u = (u * u * u) + (spec.a * u) + spec.b

  let conic_d = negate (curve_eqn params.u)

  let on_conic { Conic.z; y } =
    F.(equal ((z * z) + (params.conic_c * y * y)) conic_d)

  let on_s { S.u; v; y } =
    F.(equal conic_d (y * y * ((u * u) + (u * v) + (v * v) + spec.a)))

  let on_v (x1, x2, x3, x4) =
    F.(equal (curve_eqn x1 * curve_eqn x2 * curve_eqn x3) (x4 * x4))

  (* Filter the two points which cause the group-map to blow up. This
     is not an issue in practice because the points we feed into this function
     will be the output of blake2s, and thus (modeling blake2s as a random oracle)
     will not be either of those two points. *)
  let gen =
    Quickcheck.Generator.filter F.gen ~f:(fun t ->
        not F.(equal ((params.conic_c * t * t) + one) zero) )

  module M =
    Make (F) (F)
      (struct
        let params = params
      end)

  let test_projection_point_well_formed () =
    Alcotest.(check bool)
      "projection point is well-formed" true
      (on_conic params.projection_point)

  let test_field_to_conic () =
    Quickcheck.test ~sexp_of:F.sexp_of_t gen ~f:(fun t ->
        Alcotest.(check bool)
          "field to conic maps to a point on the conic" true
          (on_conic (M.field_to_conic t)) )

  let test_conic_to_s () =
    let conic_gen =
      Quickcheck.Generator.filter_map F.gen ~f:(fun y ->
          let z2 = conic_d - (params.conic_c * y * y) in
          if is_square z2 then Some { Conic.z = sqrt z2; y } else None )
    in
    Quickcheck.test conic_gen ~f:(fun p ->
        Alcotest.(check bool)
          "conic to S maps to a point satisfying the S equation" true
          (on_s (M.conic_to_s p)) )

  let test_field_to_s () =
    Quickcheck.test ~sexp_of:F.sexp_of_t gen ~f:(fun t ->
        Alcotest.(check bool)
          "field to S maps to a point satisfying the S equation" true
          (on_s (Fn.compose M.conic_to_s M.field_to_conic t)) )

  (* Schwarz-zippel says if this tests succeeds once, then the probability that
     the implementation is correct is at least 1 - (D / field-size), where D is
     the total degree of the polynomial defining_equation_of_V(s_to_v(t)) which should
     be less than, say, 10. So, this test succeeding gives good evidence of the
     correctness of the implementation (assuming that the implementation is just a
     polynomial, which it is by parametricity!) *)
  let test_field_to_v () =
    Quickcheck.test ~sexp_of:F.sexp_of_t gen ~f:(fun t ->
        let s = M.conic_to_s (M.field_to_conic t) in
        Alcotest.(check bool)
          "field to V maps to a point satisfying the V equation" true
          (on_v (M._s_to_v s)) )

  let test_full_map_works () =
    Quickcheck.test ~sexp_of:F.sexp_of_t gen ~f:(fun t ->
        let x, y = to_group (module F) ~params t in
        Alcotest.(check bool)
          "curve equation holds for mapped point" true
          (equal (curve_eqn x) (y * y)) )
end

module T0 = Make_tests (F13)
module T1 = Make_tests (Fp)

let () =
  Alcotest.run "Group_map"
    [ ( "Group_map F13"
      , [ Alcotest.test_case "projection point well-formed" `Quick
            T0.test_projection_point_well_formed
        ; Alcotest.test_case "field to conic" `Quick T0.test_field_to_conic
        ; Alcotest.test_case "conic to S" `Quick T0.test_conic_to_s
        ; Alcotest.test_case "field to S" `Quick T0.test_field_to_s
        ; Alcotest.test_case "field to V" `Quick T0.test_field_to_v
        ; Alcotest.test_case "full map works" `Quick T0.test_full_map_works
        ] )
    ; ( "Group_map Fp"
      , [ Alcotest.test_case "projection point well-formed" `Quick
            T1.test_projection_point_well_formed
        ; Alcotest.test_case "field to conic" `Quick T1.test_field_to_conic
        ; Alcotest.test_case "conic to S" `Quick T1.test_conic_to_s
        ; Alcotest.test_case "field to S" `Quick T1.test_field_to_s
        ; Alcotest.test_case "field to V" `Quick T1.test_field_to_v
        ; Alcotest.test_case "full map works" `Quick T1.test_full_map_works
        ] )
    ]
