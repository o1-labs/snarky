let x = 15

let y = x

let z = x

let a = 15

let b =
  let y = x in
  y

let c (ignore : int -> unit) = ignore x ; y

let d x = x

let e =
  let e =
    let e = x in
    e
  in
  e

let f =
  let f x = x in
  let g = d f f in
  (g f f) f

let (g : _ -> _) =
  let (f : _ -> _) = fun x -> x in
  f
