let a f =
  let x = (1, 2, (1, 2, (1, 2, ()))) in
  let y = (3, 4, (3, 4, (3, 4, ()))) in
  let z = f x y in
  f x z
