let random_bigint = Bigint.random;
open Core;
open Backend;

let random_bits = () => {
  Random.bits();
};

let random_field = () => {
  Bigint.(to_field(of_bignum_bigint(random_bigint(Field.Constant.size))));
};

let rec random_curve_point = () => {
  let x = random_field();
  let y2 = Field.Constant.(x * x * x + Curve.a * x + Curve.b);
  if (Field.Constant.is_square(y2)) {
    (x, Field.Constant.sqrt(y2));
  } else {
    random_curve_point();
  };
};

let gen = () => {
  let s = Caml.Random.get_state();
  Random.init(0);
  let r = Array.init(200, ~f=_ => random_curve_point());
  Caml.Random.set_state(s);
  r;
};

let gens = gen();
let params =
  Array.map(
    gens,
    ~f=g => {
      open Curve.Constant;
      let g2 = double(g);
      (g, g2, add(g, g2), double(g2));
    },
  );
