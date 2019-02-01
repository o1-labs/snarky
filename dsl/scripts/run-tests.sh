#!/bin/bash

run_dune() {
  dune $1 --display quiet --root=.. ${@:2}
}

declare -i passes
declare -i fails

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

check_diff() {
    diff -N "tests/$1" "tests/out/$1"
    local STATUS=$?
    if [ $STATUS -ne 0 ]; then
      echo -e "${RED}FAILED${NC}: $1 does not match expected output"
      fails=fails+1
    else
      echo -e "${GREEN}PASSED${NC}: $1 matches expected output"
      passes=passes+1
    fi
}

run_test() {
  run_dune exec dsl/meja.exe -- --ml "tests/out/$1.ml" --ocaml-env "tests/out/$FILENAME.ocaml-env" --stderr "tests/out/$1.stderr" "tests/$1.meja"
  if [ $? -ne 0 ]; then
    if [ -e "tests/$1.fail" ]; then
      echo -e "${GREEN}PASSED${NC}: Got expected failure building from $1.mega"
      passes=passes+1
    else
      echo -e "${RED}FAILED${NC}: Building from $1.mega"
      cat "tests/out/$1.stderr"
      fails=fails+1
    fi
  else
    if [ -e "tests/$1.fail" ]; then
      echo -e "${RED}FAILED${NC}: Expected failure building from $1.mega; got success"
      fails=fails+1
    else
      echo -e "${GREEN}PASSED${NC}: Building from $1.mega"
      passes=passes+1
    fi
    ocamlformat tests/out/$1.ml > tests/out/$1.ml.reformat
    mv tests/out/$1.ml.reformat tests/out/$1.ml
    check_diff $1.ml
    check_diff $1.ocaml-env
  fi
    check_diff $1.stderr
}

run_tests() {
  for test in tests/*.meja; do
    local FILENAME=$(basename -- "$test")
    local FILENAME="${FILENAME%.*}"
    run_test "$FILENAME"
  done
  declare -i total
  total=passes+fails
  if [[ "$fails" -ne 0 ]]; then
    echo -e "${RED}FAILED${NC} $fails/$total"
    return 1
  else
    echo -e "${GREEN}PASSED${NC} $passes/$total"
    return 0
  fi
}
