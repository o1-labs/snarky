---
id: installation
title: Installation
sidebar_label: Installation
---

Snarky is written in [OCaml](https://ocaml.org/) and built on the
[libsnark](https://github.com/scipr-lab/libsnark) C++ library.

## Libsnark

To install the dependencies for libsnark on Ubuntu, simply run:
```sh
sudo apt-get install build-essential cmake git libgmp3-dev libprocps4-dev python-markdown libboost-all-dev libssl-dev
```
(for other platforms, see these
[instructions](https://github.com/scipr-lab/libsnark#dependencies)).

## opam

The best way to install Snarky and OCaml is via opam &mdash; the OCaml package
manager. To install opam, run
```sh
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
```
or see the [opam install page](https://opam.ocaml.org/doc/Install.html).

## OCaml

Set up a new 'switch' in opam with the [latest version of
OCaml](https://ocaml.org/docs/install.html):
```sh
# environment setup
opam init
eval `opam env`
# install given version of the compiler
opam switch create 4.07.1
eval `opam env`
# check you got what you want
which ocaml
ocaml --version
```

## ReasonML
Install the Reason tooling with `opam install reason rtop`.

## Snarky

Now, simply install snarky from [our GitHub repo](https://github.com/o1-labs/snarky):
```sh
opam pin add git@github.com:o1-labs/snarky.git
```
