---
id: overview
title: Overview
sidebar_label: Overview
---

## What is Snarky?

Snarky is a library for writing verifiable programs in [OCaml](http://www.ocaml.org/) or
[ReasonML](https://reasonml.github.io/). We'll try to do examples in Reason, as the
syntax tends to be more readable for most programmers.

The code you write in Snarky builds up **zkSNARKs**, cryptographic proofs of
the integrity of the output of your program, which anybody is able to check. The zkSNARK
acronym describes proofs which are:
* **Zero Knowledge**: the verifier doesn't get any extra knowledge from the
  proof, only that the program was run on some inputs and gave a particular output.
* **Succinctness**: the proof is short and easy to check.
* **Non-interactive**: the verifier doesn't need to communicate with the prover
  to check the proof -- everything they need is inside the proof already!
* **ARgument of Knowledge**: the proof is really a proof, in the sense that
  if someone produced a proof, then they really ran the program on some values
  to get the claimed output.

Our goal is to handle the SNARKs so that you don't have to: we want to write
programs that look as natural as the unchecked code we're used to writing! For
example, adding up a list and checking the result is how you would expect:
```reasonml
let check_sum = (l : Number.t list, sum : Number.t) => {
  open Number;
  let actual_sum = List.fold(l, ~init=zero, ~f=(+));
  Boolean.Assert.is_true(actual_sum = sum)
};
```
