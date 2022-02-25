SHELL=/bin/bash -o pipefail

build:
	dune build --root=.. meja/meja.exe 2>&1 | sed -e 's#"meja/#"#'

cmi-to-meja:
	dune build --root=.. meja/cmi_to_meja.exe 2>&1 | sed -e 's#"meja/#"#'

reformat:
	dune build @./fmt; dune promote

test: build
	source scripts/run-tests.sh; run_tests

test-output: build
	source scripts/run-tests.sh; update_test_output

test-one: build
	source scripts/run-tests.sh; run_one

.PHONY: build test
