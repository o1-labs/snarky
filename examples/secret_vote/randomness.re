open Core;
open Backend;

let length = 128;

type t = list(Boolean.var);

let typ = Typ.list(~length, Boolean.typ);

let to_triples = triples_of_bits(Boolean.false_);

module Constant = {
  type t = list(bool);

  let create = () => List.init(length, ~f=_ => Random.bool());

  let to_triples = triples_of_bits(false);
};
