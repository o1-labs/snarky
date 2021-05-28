#!/bin/bash

set -xeo

opam init
eval $(opam env)
opam switch create 4.07.1 || true
opam switch 4.07.1
# Built-in opam repository.. We have to update manually
pushd /home/opam/opam-repository && git pull && popd

# Generate opam.test from the current opam switch
opam switch export opam.test
# Check if the switch is up-to-date
if diff opam.test opam.export; then
    exit 0
fi

opam switch import opam.export
