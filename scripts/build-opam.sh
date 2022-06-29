#!/bin/bash

set -xeo

opam init
# Built-in opam repository.. We have to update manually
pushd /home/opam/opam-repository && git pull && popd
opam update
eval $(opam env)
opam switch create 4.14.2 || true
opam switch 4.14.2

# Generate opam.test from the current opam switch
opam switch export opam.test
# Check if the switch is up-to-date
if diff opam.test opam.export; then
    exit 0
fi

opam switch import opam.export
