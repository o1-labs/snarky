type 'a showable = {show: 'a -> string}

let show {show; _} = show

let f __implicit2__ (x : 'a) =
  let f __implicit1__ x = (show __implicit1__) x in
  (f __implicit2__) x

let g __implicit7__ __implicit5__ __implicit3__ (x : 'a) (y : 'a) =
  let a = (show __implicit3__) x in
  let b = (show __implicit3__) y in
  let c = (show __implicit5__) 15 in
  let d = (show __implicit5__) 18 in
  let e = (show __implicit7__) true in
  let f = (show __implicit7__) false in
  (a, b, c, d, e, f)

let h __implicit17__ __implicit9__ __implicit10__ (x : int) (y : bool)
    (z : float) =
  ( (g __implicit9__ __implicit10__ __implicit10__) x x
  , (g __implicit9__ __implicit10__ __implicit9__) y y
  , (g __implicit9__ __implicit10__ __implicit17__) z z )

type ('a, 'b) conv = {conv: 'a -> 'b}

let conv {conv; _} = conv

let conv_bool_int = {conv= (fun x -> match x with true -> 1 | false -> 0)}

let i (b : bool) (f : int -> 'a) = f ((conv conv_bool_int) b)

module T = struct
  let conv_int_bool = {conv= (fun x -> match x with 0 -> false | _ -> true)}
end

let j (i : int) (f : bool -> 'a) = f ((conv T.conv_int_bool) i)

type ('a, 'b) equiv = Equiv of ('a -> 'b) * ('b -> 'a)

let equiv_eq = Equiv ((fun x -> x), fun x -> x)

let conv_equiv (Equiv (conv, _)) = {conv}

let k (i : int) (f : 'a -> 'a -> 'a) = f i ((conv (conv_equiv equiv_eq)) i)
