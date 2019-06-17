#!/bin/bash
which opam
HAS_OPAM=$?

if [ $HAS_OPAM ] ; then
  echo "Already had opam installed"
else
  sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
  opam init
fi

git clone https://github.com/o1-labs/snarky.git
cd snarky
git checkout zk-workshop

./scripts/depends.sh
./INSTALL.sh
