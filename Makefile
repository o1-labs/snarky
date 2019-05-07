
default : build

build :
	dune build --root=.

tests :
	dune runtest --root=.

ml-docs :
	dune build --root=. @doc

website : ml-docs
	./scripts/build-website.sh

test-website-build :
	./scripts/test-website-build.sh

publish-website : website
	./scripts/publish-website.sh

examples :
	dune exec --root=. ./examples/election/election_main.exe
	dune exec --root=. ./examples/merkle_update/merkle_update.exe
	# tutorial.exe intentionally is unimplemented, but it should still compile
	dune build --root=. ./examples/tutorial/tutorial.exe
	dune exec --root=. ./examples/toplevel_test/toplevel_test.exe

reformat:
	dune exec --root=. app/reformat-snarky/reformat.exe -- -path .

check-format:
	dune exec --root=. app/reformat-snarky/reformat.exe -- -path . -check

docker :
	./rebuild-docker.sh snarky

minikube :
	./rebuild-minikube.sh snarky

googlecloud :
	./rebuild-googlecloud.sh snarky

.PHONY : default build examples docker minikube googlecloud

