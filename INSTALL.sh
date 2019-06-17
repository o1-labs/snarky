#!/bin/bash
opam pin add fold_lib .
opam pin add tuple_lib .
opam pin add bitstring_lib .
opam pin add interval_union .
opam pin add ppx_snarky .
opam pin add meja .
opam pin add snarky .

echo "Make sure $OPAM_SWITCH_PREFIX/bin is on your path by adding the following line to"
echo "one of your ~/.profile, ~/.bash_profile, ~/.bashrc, etc.:"
echo
echo "export PATH=\"$OPAM_SWITCH_PREFIX/bin:$PATH\""
