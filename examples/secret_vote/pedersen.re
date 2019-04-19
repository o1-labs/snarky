open Core;
open Tuple_lib;
open Backend;

let coords = ((t1, t2, t3, t4)) => {
  let (x1, y1) = t1;
  let (x2, y2) = t2;
  let (x3, y3) = t3;
  let (x4, y4) = t4;
  ((x1, x2, x3, x4), (y1, y2, y3, y4));
};

let lookup = ((s0, s1, s2): Triple.t(Boolean.var), q) => {
  let s_and = Boolean.(s0 && s1);
  let lookup_one = ((a1, a2, a3, a4)) => {
    open Field;
    let ( * ) = Fn.flip(scale);
    constant(a1)
    + Constant.(a2 - a1)
    * (s0 :> Field.t)
    + Constant.(a3 - a1)
    * (s1 :> Field.t)
    + Constant.(a4 + a1 - a2 - a3)
    * (s_and :> Field.t);
  };
  let (x_q, y_q) = coords(q);
  let x = lookup_one(x_q);
  let y = {
    let sign = Field.(one - scale((s2 :> Field.t), Constant.of_int(2)));
    Field.(sign * lookup_one(y_q));
  };
  (x, y);
};

let hash = (triples: list((Boolean.var, Boolean.var, Boolean.var))) => {
  switch (triples) {
  | [] => failwith("TODO: digest empty")
  | [x, ...xs] =>
    List.foldi(xs, ~init=lookup(x, Params.params[0]), ~f=(i, acc, t) =>
      Curve.add_unsafe(acc, lookup(t, Params.params[i + 1]))
    )
  };
};

let digest = Fn.compose(fst, hash);

module Digest = {
  type t = Field.t;

  let typ = Field.typ;

  let to_bits = (x: t) =>
    Field.choose_preimage_var(~length=Field.Constant.size_in_bits, x);

  let to_triples = Fn.compose(triples_of_bits(Boolean.false_), to_bits);

  module Constant = {
    type t = Field.Constant.t;

    let to_triples =
      Fn.compose(triples_of_bits(false), Field.Constant.unpack);
  };
};

module Constant = {
  let lookup = ((s0, s1, s2), (q1, q2, q3, q4)): Curve.Constant.t => {
    let p =
      switch (s0, s1) {
      | (false, false) => q1
      | (true, false) => q2
      | (false, true) => q3
      | (true, true) => q4
      };
    if (s2) {
      Curve.Constant.negate(p);
    } else {
      p;
    };
  };

  let hash = (triples: list(Triple.t(bool))) => {
    switch (triples) {
    | [] => failwith("TODO: digest empty")
    | [x, ...xs] =>
      List.foldi(xs, ~init=lookup(x, Params.params[0]), ~f=(i, acc, t) =>
        Curve.Constant.add(acc, lookup(t, Params.params[i + 1]))
      )
    };
  };

  let digest = Fn.compose(fst, hash);
};
