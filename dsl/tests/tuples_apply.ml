let (a :
         (   int * int * (int * int * (int * int * unit))
          -> int * int * (int * int * (int * int * unit))
          -> int * int * (int * int * (int * int * unit)))
      -> int * int * (int * int * (int * int * unit))) =
 fun (f :
          int * int * (int * int * (int * int * unit))
       -> int * int * (int * int * (int * int * unit))
       -> int * int * (int * int * (int * int * unit))) ->
  let x = (1, 2, (1, 2, (1, 2, ()))) in
  let y = (3, 4, (3, 4, (3, 4, ()))) in
  let z = f x y in
  f x z
