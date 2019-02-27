type ('a, 'b, 'c) t = {a: 'a; b: 'b; c: 'c}

let x = {a= 15; b= 20; c= 25}

let y = {a= true; b= false; c= true}

let z = {a= x.a; b= y.b; c= ()}

module X = struct
  type 'a t = {a: 'a; b: 'a; c: 'a}

  let x = {a= 1; b= 1; c= 1}
end

let b = {X.a= 1; b= 1; c= 1}
