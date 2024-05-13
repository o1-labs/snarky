
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

