open Backend;

module Digest = {
  type t = Field.t;

  module Constant = {
    type t = Field.Constant.t
  };

  let typ = Field.typ;
};

let digest = Pedersen.digest
