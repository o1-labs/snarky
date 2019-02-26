let x __implicit2__ (x : 'a) =
  let f __implicit1__ x = (testing_show __implicit1__) x in
  (f __implicit2__) x

let y __implicit3__ (x : 'a) (y : 'a) =
  let a = (testing_show __implicit3__) x in
  let b = (testing_show __implicit3__) y in
  let c = (testing_show __implicit5__) 15 in
  let d = (testing_show __implicit5__) 18 in
  let e = (testing_show __implicit7__) true in
  let f = (testing_show __implicit7__) false in
  (a, b, c, d, e, f)
