open Core_kernel
open Interval_union

let int_pair = Alcotest.(pair int int)

let interval_union = Alcotest.(list int_pair)

let gen_from ?(min_size = 0) start =
  let open Quickcheck.Generator.Let_syntax in
  let rec go acc size start =
    if size = 0 then return (of_intervals_exn (List.rev acc))
    else
      let%bind ((_, y) as i) = Interval.gen_from start in
      go (i :: acc) (size - 1) y
  in
  let%bind size = Quickcheck.Generator.small_positive_int in
  go [] (min_size + size) start

let gen = gen_from Int.min_value

let gen_disjoint_pair =
  let open Quickcheck.Generator.Let_syntax in
  let%bind t1 = gen in
  let y = List.last_exn t1 |> snd in
  let%map t2 = gen_from y in
  (t1, t2)

let invariant t =
  let rec go = function
    | [ (a, b) ] ->
        assert (a <= b)
    | [] ->
        ()
    | (a1, b1) :: ((a2, _) :: _ as t) ->
        assert (a1 <= b1) ;
        assert (b1 < a2) ;
        go t
  in
  go t

let test_interval_gen () =
  Quickcheck.test Interval.gen ~f:(fun (x, y) ->
      Alcotest.(check bool) "x <= y" true (x <= y) )

let test_canonicalize () =
  Alcotest.(check interval_union)
    "canonicalize works correctly"
    [ (1, 3) ]
    (canonicalize [ (1, 2); (2, 3) ])

let test_invariant () =
  Quickcheck.test gen ~f:(fun t ->
      try
        invariant t ;
        Alcotest.(check bool) "invariant holds" true true
      with _ -> Alcotest.fail "invariant violated" )

let test_disjoint_union_order () =
  Quickcheck.test gen_disjoint_pair ~f:(fun (a, b) ->
      Alcotest.(check interval_union)
        "disjoint_union_exn is commutative" (disjoint_union_exn a b)
        (disjoint_union_exn b a) )

let test_disjoint_union_invariant () =
  Quickcheck.test gen_disjoint_pair ~f:(fun (a, b) ->
      try
        invariant (disjoint_union_exn a b) ;
        Alcotest.(check bool) "invariant holds" true true
      with _ -> Alcotest.fail "invariant violated" )

let test_disjoint_union_with_holes () =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let s = 1000000 in
    let%bind y0 = Int.gen_incl 0 s in
    let%bind y1 = Int.gen_incl (y0 + 1) (y0 + s) in
    let%bind y2 = Int.gen_incl (y1 + 1) (y1 + s) in
    let%bind y3 = Int.gen_incl (y2 + 1) (y2 + s) in
    return (of_interval (y1, y2), of_intervals_exn [ (y0, y1); (y2, y3) ])
  in
  Quickcheck.test gen ~f:(fun (x, y) ->
      try
        invariant (disjoint_union_exn x y) ;
        Alcotest.(check bool) "invariant holds" true true
      with _ -> Alcotest.fail "invariant violated" )

let () =
  Alcotest.run "Interval_union"
    [ ( "Interval"
      , [ Alcotest.test_case "gen_is_correct" `Quick test_interval_gen ] )
    ; ( "Interval_union"
      , [ Alcotest.test_case "canonicalize" `Quick test_canonicalize
        ; Alcotest.test_case "invariant" `Quick test_invariant
        ; Alcotest.test_case "disjoint_union_order" `Quick
            test_disjoint_union_order
        ; Alcotest.test_case "disjoint_union_invariant" `Quick
            test_disjoint_union_invariant
        ; Alcotest.test_case "disjoint_union_with_holes" `Quick
            test_disjoint_union_with_holes
        ] )
    ]
