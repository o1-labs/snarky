open Backend;

let a = Field.Constant.of_string("7296080957279758407415468581752425029516121466805344781232734728849116493472");
let b = Field.Constant.of_string("16213513238399463127589930181672055621146936592900766180517188641980520820846");

type t = (Field.t , Field.t )

let double ((x, y)) = {
  open Field;
  let x_squared = square(x);
  let lambda =
    exists_field(() =>
      Constant.(read_var(x) / read_var(y)));
  let bx =
    exists_field(() => {
        let x = read_var(x);
        Constant.(square(read_var(lambda)) - (x + x))
      });
  let by =
    exists_field(() =>
      Constant.(read_var(lambda) * (read_var(x) - read_var(bx)) - read_var(y)));
  assert_ ((lambda + lambda) *: y == of_int(3) * x_squared + constant(a));
  assert_ (lambda *: lambda == bx + x + x);
  assert_ (lambda *: (x - bx) == by + y);
  (bx, by)
};

let div_unsafe (x, y) = {
  let z =
    exists_field(
        () =>
          Field.Constant.(read_var(x) / read_var(y)));
  assert_(z *: y == x);
  z
};

let add_unsafe = ((ax, ay), (bx, by)) => {
  let lambda = div_unsafe(Field.(by - ay), Field.(bx - ax));
  let cx =
    exists_field(
      () =>
        Field.Constant.(read_var(lambda) + read_var(ax) + read_var(bx))
    );

  assert_square(lambda, Field.(cx + ax + bx));
  let cy =
    exists_field(() => {
        let lambda = read_var(lambda)
        and ax = read_var(ax)
        and cx = read_var(cx)
        and ay = read_var(ay);
        Field.Constant.(lambda * (ax - cx) - ay);
      },
    );

  assert_(lambda *: Field.(ax - cx) == Field.(cy + ay));
  (cx, cy)
};

module Constant = {
  type t = (Field.Constant.t , Field.Constant.t)
  let double((x, y)) = {
    open Field.Constant;
    let lambda = x / y;
    let bx = square(lambda) - (x + x);
    let by = lambda * (x - bx) - y;
    (bx, by)
  };

  let add((ax, bx), (ay, by)) = {
    open Field.Constant;
    let lambda = (by - ay) / (bx - ax);
    let cx = lambda + ax + bx;
    let cy = lambda * (ax - cx) - ay;
    (cx, cy)
  };

  let negate((x, y)) = (x, Field.Constant.negate(y));
};
