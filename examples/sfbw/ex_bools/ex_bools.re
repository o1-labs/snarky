module Universe = (val Snarky_universe.default ());

open! Universe.Impl;
open! Universe;

module Witness = Field;

let input = InputSpec.[(module Hash)];

/* Let's say field element is "special" if it is either
  - A perfect square and less than 2^32 and even.
  - Not a perfect square and less than 256 = 2^8
*/

let assertSpecial = (x) => {
  /* This implicitly asserts that x fits in 32 bits, which means it's < 2^32. */
  let bits = Field.toBits(~length=32, x);
  let fitsIn8Bits = Bool.negate(Bool.any(Array.to_list(Array.sub(bits, 8, 24))));
  let isSquare = Field.isSquare(x);
  Bool.assertAny([
    Bool.all([ isSquare, Bool.negate(bits[31]) ]),
    Bool.all([ Bool.negate(isSquare), fitsIn8Bits ])
  ]);
};

/*
Exercise ideas
- arithmetiziation: implement boolean ops
- internal handlers

Examples with integers
- 

/* Floating point arithmetic */
Fun exercises to give people to do
- 
*/

/* Proves that we know a preimage to the given hash which is special */
let main = (preimage: Witness.t, h, ()) => {
  Field.assertEqual(Hash.hash([|preimage|]), h);
  assertSpecial(preimage);
};

InputSpec.run_main(input, (module Witness), main);
