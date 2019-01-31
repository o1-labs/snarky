DUNE = dune exec --root=..

build:
	dune build --root=.. dsl/meja.exe 2>&1 | sed -e 's#"dsl/#"#'

reformat:
	$(DUNE) app/reformat-snarky/reformat.exe -- -path .

tests/%.ml: tests/%.meja ../_build/default/dsl/meja.exe
	$(DUNE) dsl/meja.exe -- --ml $@ $<

tests/%.ast: tests/%.meja ../_build/default/dsl/meja.exe
	$(DUNE) dsl/meja.exe -- --ast $@ $<

tests/%.struct: tests/%.meja ../_build/default/dsl/meja.exe
	$(DUNE) dsl/meja.exe -- --struct $@ $<

tests/out/%.meja: tests/%.meja ../_build/default/dsl/meja.exe
	mkdir tests/out || true
	cp $< $@

test: SHELL := /bin/bash
test:
	source scripts/run-tests.sh; run_tests

test-verbose: SHELL := /bin/bash
test-verbose:
	source scripts/run-tests.sh; run_tests_verbose

.PHONY: build reformat run test
