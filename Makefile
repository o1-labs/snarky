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

.PHONY: website
website: ml-docs
	./scripts/build-website.sh

.PHONY: test-website-build
test-website-build:
	./scripts/test-website-build.sh

.PHONY: publish-website
publish-website: website
	./scripts/publish-website.sh

.PHONY: reformat
reformat:
	@dune fmt --auto-promote

.PHONY: check-format
check-format:
	@dune fmt

.PHONY: docker
docker:
	./rebuild-docker.sh snarky
