let x __implicit2__ (x : 'a) =
  let f __implicit1__ x = (testing_show __implicit1__) x in
  (f __implicit2__) x

let y __implicit11__ __implicit9__ __implicit3__ (x : 'a) (y : 'a) =
  let a = (testing_show __implicit3__) x in
  let b = (testing_show __implicit3__) y in
  let c __implicit5__ = (testing_show __implicit5__) 15 in
  let d __implicit6__ = (testing_show __implicit6__) 18 in
  let e __implicit7__ = (testing_show __implicit7__) true in
  let f __implicit8__ = (testing_show __implicit8__) false in
  (a, b, c __implicit9__, d __implicit9__, e __implicit11__, f __implicit11__)
