#!/bin/bash

set -eo

# Check if the switch is up-to-date
opam switch export opam.test
if diff opam.test opam.export; then
    exit 0
fi

opam init
opam update
opam switch create 4.07.1 || true
opam switch 4.07.1
opam switch import opam.export
