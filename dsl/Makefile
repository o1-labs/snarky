build:
	dune build --root=.. dsl/meja.exe 2>&1 | sed -e 's#"dsl/#"#'

reformat:
	dune exec --root=. app/reformat-snarky/reformat.exe -- -path .

run: build
	dune exec --root=.. dsl/meja.exe -- test.meja

.PHONY: build reformat run
