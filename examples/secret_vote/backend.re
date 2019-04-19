open Core;
open Snarky;
open Snark;
open Fold_lib;

include Run.Make(Backends.Bn128.Default);

let read_var = As_prover.read_var;
let read = As_prover.read;

let ( *: ) = (x, y, z) => Constraint.r1cs(x, y, z);

let (==) = (lhs, rhs) => lhs(rhs);

let exists_field = f => exists(Field.typ, ~compute=f);

let triples_of_bits = (default, xs) =>
  Fold.(to_list(group3(~default, of_list(xs))));

let boolean_triples =
  List.map(~f=((x, y, z)) =>
    Boolean.(var_of_value(x), var_of_value(y), var_of_value(z))
  );
