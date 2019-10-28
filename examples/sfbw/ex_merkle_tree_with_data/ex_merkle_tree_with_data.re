module Universe = (val Snarky_universe.default());
open! Universe.Impl;
open! Universe;

let depth = 8;

module Age = Field;

module Byte = {
  type t = array(Bool.t);
  module Constant = {
    [@deriving yojson]
    type t = array(bool);
  }

  let typ = Typ.array(~length=8, Bool.typ);

  let assertEqual = (b1, b2) => {
    for (i in 0 to 7) {
      Bool.assertEqual(b1[i], b2[i]);
    }
  };
};

module Color = {
  [@deriving yojson]
  type triple('a) = ('a, 'a, 'a);

  type t = triple(Byte.t);

  module Constant = {
    [@deriving yojson]
    type t = triple(Byte.Constant.t)
  };

  let typ = Typ.tuple3(Byte.typ, Byte.typ, Byte.typ);

  let assertEqual = ((r1, g1, b1), (r2, g2, b2)) => {
    Byte.assertEqual(r1, r2);
    Byte.assertEqual(g1, g2);
    Byte.assertEqual(b1, b2);
  }
}

module Person = {
  /* A tuple of age and their favorite color. */
  type t = (Age.t, Color.t);

  module Constant = {
    [@deriving yojson]
    type t = (Age.Constant.t, Color.Constant.t);
  };

  let typ : Typ.t(t, Constant.t) = Typ.tuple2(Age.typ, Color.typ);

  let hash = ((age, (r,g,b))) => {
    Hash.hash([| age, Field.ofBits(Array.concat([r, g, b])) |])
  }
};

/* We'll prove, "I know a person in this merkle tree whose favorite color is
   the given public input and whose age is > 30." */
module Witness = {
  type t = (Person.t, MerkleTree.MembershipProof.t);
  
  module Constant = {
    [@deriving yojson]
    type t = (Person.Constant.t, MerkleTree.MembershipProof.Constant.t);
  }

  let typ = Typ.tuple2(Person.typ, MerkleTree.MembershipProof.typ(~depth));
}

let statement = InputSpec.[
  (module Hash), /* Merkle root */
  (module Color),
];

let checkOver30 = (age : Age.t) => {
  /* An age should fit in 10 bits */

  let bits = Field.toBits(~length=10, age);
  Bool.assertTrue(
    Integer.(>)(Integer.ofBits(bits), Integer.ofInt(30)));
};

let main = ((person, merkleProof): Witness.t, root, color, ()) => {
  let (age, favoriteColor) = person;
  Color.assertEqual(color, favoriteColor);
  let personHash = Person.hash(person);
  Bool.assertTrue(
    MerkleTree.MembershipProof.check(merkleProof, root, personHash));
  checkOver30(age);
};

runMain(statement, (module Witness), main);
