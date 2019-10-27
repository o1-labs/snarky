module Universe = (val Snarky_universe.default());

open! Universe.Impl;
open! Universe;

/* Inside of SNARKs, the only primitives we have to work with are field addition and multiplication.
   In this tutorial, we'll see how boolean operations can be expressed using field arithmetic. */

module MyBool = {
  /* A bool is represented using a field element which is either 0 (for false) or 1 (for true) */
  type t = Field.t;

  open Field;

  /* we can "and" two booleans by multiplying them as field elements. */
  let myAnd = (x, y) => Field.mul(x, y);

  /* we can negate a boolean x as 1 - x (since this swaps 0 and 1) */
  let myNot = x => one - x;

  /* we can "or" booleans using the identity
     x or y =  not (not x and not y) */

  let myOr = (x, y) => myNot(myAnd(myNot(x), myNot(y)));

  /* In many SNARKs, addition is "free". So, we can efficiently "and" a list of bools by
     adding them all up and checking if the result is equal to the length of the list,
     which it can only be if all elements of the list are 1. */
  let myAll = bools =>
    Field.equal(
      List.fold_left(Field.add, Field.zero, bools),
      Field.ofInt(List.length(bools)),
    );
};
