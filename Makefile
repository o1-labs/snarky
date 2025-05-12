.PHONY: default
default: build

.PHONY: build
build:
	@dune build @install

.PHONY: tests
tests:
	@dune runtest

.PHONY: ml-docs
ml-docs:
	@dune build @doc

.PHONY: reformat
reformat:
	@dune fmt --auto-promote

.PHONY: check-format
check-format:
	@dune fmt

.PHONY: docker
docker:
	./scripts/rebuild-docker.sh snarky
