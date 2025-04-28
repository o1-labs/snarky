# snarky

`snarky` is an OCaml front-end for writing R1CS SNARKs.
It is modular over the backend SNARK library, and comes with backends
from [libsnark](https://github.com/scipr-lab/libsnark).

Disclaimer: This code has not been thoroughly audited and should not
be used in production systems.

**CAVEAT** This repository contains a substantial amount of obsolete
code. Earlier versions of the Mina project (the primary user of this
code) used the C/C++ backend implemented in `src/`; most of that code
is no longer used. The exceptions are `src/intf` and `src/base`.

- [snarky](#snarky)
  - [Getting started](#getting-started)
  - [Design](#design)
    - [Example: Merkle trees](#example-merkle-trees)
  - [Implementation](#implementation)
  - [Building](#building)

## Getting started

- First install libsnark's dependencies by running [scripts/depends.sh](scripts/depends.sh), or following the instructions [here](https://github.com/scipr-lab/libsnark#dependencies).
- Then, make sure you have [opam](https://opam.ocaml.org/doc/Install.html) installed.
- Finally, install `snarky` and its dependencies by running
```bash
opam pin add git@github.com:o1-labs/snarky.git
```
and answering yes to the prompts.

## Design

The intention of this library is to allow writing snarks by writing what looks
like normal programs (whose executions the snarks verify). If you're an experienced
functional programmer, the basic idea (simplifying somewhat) is that there is a monad
`Checked.t` so that a value of type `'a Checked.t` is an `'a` whose computation is
certified by the snark. For example, we have a function
```ocaml
mul : var -> var -> (var, _) Checked.t.
```
Given `v1, v2 : var`, `mul v1 v2` is a variable containing the product of v1 and v2,
and the snark will ensure that this is so.


### Example: Merkle trees

One computation useful in snarks is verifying membership in a list. This is
typically accomplished using authentication paths in Merkle trees. Given a
hash `entry_hash`, an address (i.e., a list of booleans) `addr0` and an
authentication path (i.e., a list of hashes) `path0`, we can write a checked
computation for computing the implied Merkle root:

```ocaml
  let implied_root entry_hash addr0 path0 =
    let rec go acc addr path =
      let open Let_syntax in
      match addr, path with
      | [], [] -> return acc
      | b :: bs, h :: hs ->
        let%bind l = Hash.if_ b ~then_:h ~else_:acc
        and r = Hash.if_ b ~then_:acc ~else_:h
        in
        let%bind acc' = Hash.hash l r in
        go acc' bs hs
      | _, _ -> failwith "Merkle_tree.Checked.implied_root: address, path length mismatch"
    in
    go entry_hash addr0 path0
```

The type of this function is
```ocaml
val implied_root : Hash.var -> Boolean.var list -> Hash.var list -> (Hash.var, 'prover_state) Checked.t
```
The return type `(Hash.var, 'prover_state) Checked.t` indicates that the function
returns a "checked computation" producing a variable containing a hash, and can be
run by a prover with an arbitrary state type `'prover_state`. 

Compare this definition to the following "unchecked" OCaml function (assuming a function `hash`):
```ocaml
let implied_root_unchecked entry_hash addr0 path0 =
  let rec go acc addr path =
    match addr, path with
    | [], [] -> acc
    | b :: bs, h :: hs ->
      let l = if b then h else acc
      and r = if b then acc else h
      in
      let acc' = hash l r in
      go acc' bs hs
    | _, _ ->
      failwith "Merkle_tree.implied_root_unchecked: address, path length mismatch"
  in
  go entry_hash addr0 path0
;;
```
The two obviously look very similar, but the first one can be run to generate an R1CS
(and also an "auxiliary input") to verify that computation. 

## Setting up Development Environment

```bash
# Install the OPAM package manager if it is not already installed.
# Follow the official instructions: https://opam.ocaml.org/doc/Install.html

# Create a local switch using OCaml version 4.14.0
opam switch create ./ 4.14.0 -y 

# Alternatively, create a named switch (e.g., "snarky") with the same version
opam switch create snarky 4.14.0 -y

# Activate the switch in the current shell session
eval $(opam env)
```

Please refer to `.github/workflows/build.yml` for the OCaml version.

## Building

```
$ dune build
```
