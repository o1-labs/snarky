open Snarky;
open Snark;

module I = Run.Make(Backends.Bn128.Default);
open I;

open As_prover;

let ( *: ) (x, y) =
  fun (z) => Constraint.r1cs (x, y, z);

let (==) (lhs, rhs) = lhs (rhs);

let exists_field(f) = exists(Field.typ, ~compute=f);

let a = failwith("TODO");

let double ((x, y)) = {
  open Field;
  let x_squared = square(x);
  let lambda =
    exists_field(fun () =>
      Constant.(read_var(x) / read_var(y)));
  let bx =
    exists_field(fun () => {
        let x = read_var(x);
        Constant.(square(read_var(lambda)) - (x + x))
      });
  let by =
    exists_field(fun () =>
      Constant.(read_var(lambda) * (read_var(x) - read_var(bx)) - read_var(y)));
  assert_ ((lambda + lambda) *: y == of_int(3) * x_squared + a);
  assert_ (lambda *: lambda == bx + x + x);
  assert_ (lambda *: (x - bx) == by + y);
  (x, y)
};

let div_unsafe = (type f, ~m as (module I): m(f), x, y) => {
  open I;
  let z =
    exists(
      Field.typ,
      ~compute=
        As_prover.(fun () =>
          Field.Constant.(read_var(x) / read_var(y))),
    );

  assert_r1cs(z, y, x);
  z;
};

module Curve = {
  type params('f) = {
    a: 'f,
    b: 'f,
  };

  let double = (type f, ~m as (module I): m(f), ~params, (ax, ay)) => {
    open I;
    let x_squared = Field.square(ax);
    let lambda =
      exists(
        Field.typ,
        ~compute=fun () => {
          open As_prover;
          let x_squared = read_var(x_squared)
          and ay = read_var(ay);
          Field.Constant.(
            (x_squared + x_squared + x_squared + params.a) / (ay + ay)
          );
        },
      );

    let bx =
      exists(
        Field.typ,
        ~compute=fun () => {
          open As_prover;
          let lambda = read_var(lambda)
          and ax = read_var(ax);
          Field.Constant.(square(lambda) - (ax + ax));
        },
      );

    let by =
      exists(
        Field.typ,
        ~compute=fun () => {
          open As_prover;
          let lambda = read_var(lambda)
          and ax = read_var(ax)
          and ay = read_var(ay)
          and bx = read_var(bx);
          Field.Constant.(lambda * (ax - bx) - ay);
        },
      );

    open Field;
    assert_r1cs(
      lambda + lambda,
      ay,
      of_int(3) * x_squared + constant(params.a),
    );
    assert_square(lambda, bx + ax + ax);
    assert_r1cs(lambda, ax - bx, by + ay);
    (bx, by);
  };

  let add_unsafe = (type f, ~m: m(f), (ax, ay), (bx, by)) => {
    let (module I) = m;
    open I;
    let lambda = div_unsafe(~m, Field.(by - ay), Field.(bx - ax));
    let cx =
      exists(
        Field.typ,
        ~compute=fun () => {
          open As_prover;
          let lambda = read_var(lambda)
          and s = read_var(Field.(ax + bx));
          Field.Constant.(lambda + s);
        },
      );

    assert_square(lambda, Field.(cx + ax + bx));
    let cy =
      exists(
        Field.typ,
        ~compute=fun () => {
          open As_prover;
          let lambda = read_var(lambda)
          and ax = read_var(ax)
          and cx = read_var(cx)
          and ay = read_var(ay);
          Field.Constant.(lambda * (ax - cx) - ay);
        },
      );

    I.assert_r1cs(lambda, Field.(ax - cx), Field.(cy + ay));
    (cx, cy);
  };

  let choose = (type f, ~m as (module I): m(f), b, (x0, x1), (y0, y1)) =>
    I.Field.(if_(b, ~then_=x0, ~else_=x1), if_(b, ~then_=y0, ~else_=y1));

  let scale = (~m, ~params, bits, pt, init) => {
    let (+) = add_unsafe(~m);
    let rec go = (two_to_the_i, acc) =>
      fun
      | [] => acc
      | [b] => choose(~m, b, two_to_the_i + acc, acc)
      | [b, ...bs] => {
          let acc = choose(~m, b, two_to_the_i + acc, acc);
          go(double(~m, ~params, two_to_the_i), acc, bs);
        };

    go(pt, init, bits);
  };
};
