let f __implicit2__ (x : 'a) =
  let f __implicit1__ x = (testing_show __implicit1__) x in
  (f __implicit2__) x

let g __implicit7__ __implicit5__ __implicit3__ (x : 'a) (y : 'a) =
  let a = (testing_show __implicit3__) x in
  let b = (testing_show __implicit3__) y in
  let c = (testing_show __implicit5__) 15 in
  let d = (testing_show __implicit5__) 18 in
  let e = (testing_show __implicit7__) true in
  let f = (testing_show __implicit7__) false in
  (a, b, c, d, e, f)

let h __implicit15__ __implicit11__ __implicit9__ (x : int) (y : bool)
    (z : float) =
  ( (g __implicit11__ __implicit9__ __implicit9__) x x
  , (g __implicit11__ __implicit9__ __implicit11__) y y
  , (g __implicit11__ __implicit9__ __implicit15__) z z )
