
all : docker googlecloud minikube

build :
	dune build --root=.

examples :
	dune exec --root=. ./examples/election/election_main.exe
	# TODO: Re-enable when fixed, see #41
	# dune exec --root=. ./examples/merkle_update/merkle_update.exe
	# tutorial.exe intentionally is unimplemented, but it should still compile
	dune build --root=. ./examples/tutorial/tutorial.exe

docker :
	./rebuild-docker.sh ocaml-camlsnark

minikube :
	./rebuild-minikube.sh ocaml-camlsnark

googlecloud :
	./rebuild-googlecloud.sh ocaml-camlsnark

.PHONY : all build examples docker minikube googlecloud

