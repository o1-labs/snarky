
default : build

build :
	dune build @./install

tests :
	dune runtest .

ml-docs :
	dune build @./doc

website : ml-docs
	./scripts/build-website.sh

test-website-build :
	./scripts/test-website-build.sh

publish-website : website
	./scripts/publish-website.sh

examples :
	dune exec ./examples/election/election_main.exe
	dune exec ./examples/merkle_update/merkle_update.exe
	# tutorial.exe intentionally is unimplemented, but it should still compile
	dune build ./examples/tutorial/tutorial.exe

examples-gpu :
	dune exec ./examples/election_gpu/election_main.exe

reformat:
	dune build @./fmt; dune promote

check-format:
	dune build @./fmt

docker :
	./rebuild-docker.sh snarky

minikube :
	./rebuild-minikube.sh snarky

googlecloud :
	./rebuild-googlecloud.sh snarky

.PHONY : default build examples docker minikube googlecloud

