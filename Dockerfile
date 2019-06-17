# Creates an environment for building both snarky 
# and things that use snarky.

FROM ocaml/opam2:debian-9-ocaml-4.07

# Install the libsnark dependencies and a bootstrap OCaml environment.
RUN sudo apt-get -q update && \
    sudo apt-get --no-install-recommends -q -y install \
        build-essential=12.3 \
        cmake=3.7.2-1 \
        git=1:2.11.0-3+deb9u4 \
        libboost-dev=1.62.0.1 \
        libboost-program-options-dev=1.62.0.1 \
        libffi-dev=3.2.1-6 \
        libgmp-dev=2:6.1.2+dfsg-1 \
        libgmp3-dev=2:6.1.2+dfsg-1 \
        libprocps-dev=2:3.3.12-3+deb9u1 \
        libssl-dev=1.1.0j-1~deb9u1 \
        m4=1.4.18-1 \
        nano=2.7.4-1 \
        pkg-config=0.29-4+b1 \
        python-markdown=2.6.8-1 && \
    sudo apt-get clean && \
    sudo rm -rf /var/lib/apt/lists/*

# Create a volume we can work in. For initial build, 
# we'll copy the local context. To update the snarky 
# library itself later, bind mount your updated source 
# over this and run the build again.
COPY . /source
RUN sudo chown -R opam:opam /source
VOLUME ["/source"]

# Move to a newer version of OCaml and install dune/jbuilder.
RUN git -C /home/opam/opam-repository pull
RUN opam update -y && opam upgrade -y

WORKDIR /source

# Pin and install the dependencies.
RUN eval "$(opam config env)" && \
    (rm -r .git || true) && \
    opam install menhir && \
    opam pin add -y "/source"

# Docker inception
COPY Dockerfile /
