module Universe = Snarky_universe.Bn128 ()
open Universe.Impl
open Universe

module Witness = Field

let main (preimage : Witness.t) h  =
  Field.assertEqual (Hash.hash [|preimage|]) h
