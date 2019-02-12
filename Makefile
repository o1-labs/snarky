
default : build

build :
	dune build --root=.

tests :
	dune runtest --root=.

ml-docs :
	dune build --root=. @doc

examples :
	dune exec --root=. ./examples/election/election_main.exe
	# TODO: Re-enable when fixed, see #41
	# dune exec --root=. ./examples/merkle_update/merkle_update.exe
	# tutorial.exe intentionally is unimplemented, but it should still compile
	dune build --root=. ./examples/tutorial/tutorial.exe

reformat:
	dune exec --root=. app/reformat-snarky/reformat.exe -- -path .

check-format:
	dune exec --root=. app/reformat-snarky/reformat.exe -- -path . -check

docker :
	./rebuild-docker.sh ocaml-camlsnark

minikube :
	./rebuild-minikube.sh ocaml-camlsnark

googlecloud :
	./rebuild-googlecloud.sh ocaml-camlsnark

.PHONY : default build examples docker minikube googlecloud

