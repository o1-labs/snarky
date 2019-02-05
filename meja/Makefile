build:
	dune build --root=.. meja/meja.exe 2>&1 | sed -e 's#"meja/#"#'

reformat:
	dune exec --root=.. app/reformat-snarky/reformat.exe -- -path .

test: SHELL := /bin/bash
test:
	source scripts/run-tests.sh; run_tests

test-output: SHELL := /bin/bash
test-output:
	source scripts/run-tests.sh; update_test_output

.PHONY: build test
