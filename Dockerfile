FROM ocaml/opam:ubuntu

RUN sudo apt-get -y update && \
  sudo apt-get -y upgrade && \
  sudo apt-get -y install build-essential cmake git libgmp3-dev libprocps4-dev python-markdown libboost-all-dev libssl-dev libffi-dev

ADD . /home/opam/snarky
RUN sudo chown -R opam:opam /home/opam/snarky
RUN opam update -y && \
  opam pin add -y snarky /home/opam/snarky

