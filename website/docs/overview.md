---
id: overview
title: Overview
sidebar_label: Overview
---

## What is Snarky?

Snarky is a library for writing verifiable programs in OCaml.

The code you write in Snarky builds up **zkSNARKs**, cryptographic proofs of
the integrity of your program, which anybody is able to check. The zkSNARK
acronym describes proofs which are:
* **Zero Knowledge**: the verifier doesn't get any extra knowledge from the
  proof, only that the program was run.
* **Succinctness**: the proof is short and easy to check.
* **Non-interactive**: the verifier doesn't need to communicate with the prover
  to check the proof -- everything they need is inside the proof already!
* **ARgument of Knowledge**: the proof demonstrates that the prover has the
  knowledge of running the program -- the values it computes along the way --
  in a way that is infeasible to fake.

Our goal is to handle the SNARKs so that you don't have to: we want to write
programs that look as natural as the unchecked code we're used to writing! For
example, adding up a list and checking the result is how you would expect:
```ocaml
let check_sum (l : Number.t list) (sum : Number.t) =
  let open Number in
  Boolean.Assert.is_true (List.fold ~init:zero l ~f:(+) = sum)
```
