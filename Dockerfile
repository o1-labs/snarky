FROM ocaml/opam2:debian-9-ocaml-4.07

# dependencies
RUN sudo apt update
RUN sudo apt install -y \
    build-essential=12.3 \
    pkg-config=0.29-4+b1 \
    git=1:2.11.0-3+deb9u4 \
    libboost-dev=1.62.0.1 \
    libboost-program-options-dev=1.62.0.1 \
    libffi-dev=3.2.1-6 \
    libgmp-dev=2:6.1.2+dfsg-1+deb9u1 \
    libgmp3-dev=2:6.1.2+dfsg-1+deb9u1 \
    libprocps-dev=2:3.3.12-3+deb9u1 \
    libssl-dev \
    m4=1.4.18-1 


# Create a volume we can work in. For initial build, 
# we'll copy the local context. To update the snarky 
# library itself later, bind mount your updated source 
# over this and run the build again.
COPY . /source
RUN sudo chown -R opam:opam /source
VOLUME /source

# Move to a newer version of OCaml and install dune.
RUN git -C /home/opam/opam-repository pull
RUN eval "$(opam config env)" 
RUN opam update -y
RUN opam upgrade -y
RUN opam install dune

WORKDIR /source

# Pin and install the dependencies.
RUN opam pin add -y --kind path .

CMD ["bash" "opam list"]
