#!/bin/bash

# Capture the interrupt signal (Ctrl-C) and exit
trap "exit" SIGINT

run_dune() {
  dune $1 --display quiet --root=.. ${@:2}
}

declare -i passes
passes=0
declare -i fails
fails=0
declare -i update_output
update_output=0

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

check_diff() {
    diff -Nq "tests/$1" "tests/out/$1" > /dev/null
    local STATUS=$?
    if [ $STATUS -ne 0 ]; then
      fails=fails+1
      if [[ "$update_output" -eq 0 ]]; then
        echo -e "${RED}FAILED${NC}: $1 does not match expected output"
        diff -N "tests/$1" "tests/out/$1"
      else
        echo "Updating tests/$1"
        cp "tests/out/$1" "tests/$1"
      fi
    else
      if [[ "$update_output" -eq 0 ]]; then
        echo -e "${GREEN}PASSED${NC}: $1 matches expected output"
        passes=passes+1
      fi
    fi
}

run_test() {
  if [ -z "$MEJA_BACKTRACE" ]; then
    BACKTRACE_FLAG=""
  else
    BACKTRACE_FLAG="--compiler-backtraces"
  fi
  run_dune exec meja/meja.exe -- $BACKTRACE_FLAG --ml "tests/out/$1.ml" --meji-out "tests/out/$1.meji" --stderr "tests/out/$1.stderr" "tests/$1.meja" 2> /dev/null
  if [ $? -ne 0 ]; then
    if [ -e "tests/$1.fail" ]; then
      if [[ "$update_output" -eq 0 ]]; then
        echo -e "${GREEN}PASSED${NC}: Got expected failure building from $1.meja"
        passes=passes+1
      fi
    else
      fails=fails+1
      if [[ "$update_output" -eq 0 ]]; then
        echo -e "${RED}FAILED${NC}: Building from $1.meja"
        cat "tests/out/$1.stderr"
      else
        echo "Adding tests/$1.fail: Failed to build $1.meja"
        touch "tests/$1.fail"
      fi
    fi
  else
    if [ -e "tests/$1.fail" ]; then
      fails=fails+1
      if [[ "$update_output" -eq 0 ]]; then
        echo -e "${RED}FAILED${NC}: Expected failure building from $1.meja; got success"
      else
        echo "Removing tests/$1.fail: Successfully built $1.meja"
        rm "tests/$1.fail"
      fi
    else
      if [[ "$update_output" -eq 0 ]]; then
        echo -e "${GREEN}PASSED${NC}: Building from $1.meja"
        passes=passes+1
      fi
    fi
    ocamlformat tests/out/$1.ml > tests/out/$1.ml.reformat &&
    mv tests/out/$1.ml.reformat tests/out/$1.ml
    check_diff $1.ml
    check_diff $1.meji
  fi
    check_diff $1.stderr
}

run_tests() {
  run_dune build meja/meja.exe
  if [ $? -ne 0 ]; then
    echo -e "${RED}BUILD FAILED${NC}"
    return 1
  else
    mkdir -p tests/out
    for test in tests/*.meja; do
      local FILENAME=$(basename -- "$test")
      local FILENAME="${FILENAME%.*}"
      run_test "$FILENAME"
    done
    declare -i total
    total=passes+fails
    if [[ "$update_output" -ne 0 ]]; then
      echo -e "${GREEN}UP-TO-DATE${NC} $fails files changed"
      return 0
    elif [[ "$fails" -ne 0 ]]; then
      echo -e "${RED}FAILED${NC} $fails/$total"
      return 1
    else
      echo -e "${GREEN}PASSED${NC} $passes/$total"
      return 0
    fi
  fi
}

update_test_output() {
  update_output=1
  run_tests
}


run_one() {
  if [ -z "$FILENAME" ]; then
    echo "Please specify the filename of the test to run in FILENAME"
    return 1
  fi
  run_dune build meja/meja.exe
  if [ $? -ne 0 ]; then
    echo -e "${RED}BUILD FAILED${NC}"
    return 1
  else
    mkdir -p tests/out
    run_test "$FILENAME"
    declare -i total
    total=passes+fails
    if [[ "$update_output" -ne 0 ]]; then
      echo -e "${GREEN}UP-TO-DATE${NC} $fails files changed"
      return 0
    elif [[ "$fails" -ne 0 ]]; then
      echo -e "${RED}FAILED${NC} $fails/$total"
      return 1
    else
      echo -e "${GREEN}PASSED${NC} $passes/$total"
      return 0
    fi
  fi
}
