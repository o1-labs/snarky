if [[ "$OSTYPE" == "linux-gnu" ]]; then
  echo "Installing opam..."
  sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

  sudo apt-get install build-essential cmake git libgmp3-dev libprocps4-dev python-markdown libboost-all-dev libssl-dev
elif [[ "$OSTYPE" == "darwin"* ]]; then
  PACKAGES="gpatch cmake gmp pkg-config openssl libffi libsodium boost zlib"

  echo "Installing opam..."
  sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

  # removing already installed packages from the list
  for p in $(env HOMEBREW_NO_AUTO_UPDATE=1 brew list); do
    PACKAGES=${PACKAGES//$p/}
  done;

  # only run if there's work to do
  if [[ $PACKAGES = *[![:space:]]* ]];
    then
    yes | env HOMEBREW_NO_AUTO_UPDATE=1 brew install $PACKAGES
  else
    echo 'All brew packages have already been installed.'
  fi
elif [[ "$OSTYPE" == "cygwin" ]]; then
  echo "Cygwin is not yet supported. Please file an issue so we know to work on it!"
  exit 1
elif [[ "$OSTYPE" == "msys" ]]; then
  echo "Windows is not yet supported. Please file an issue so we know to work on it!"
  exit 1
elif [[ "$OSTYPE" == "win32" ]]; then
  echo "Windows is not yet supported. Please file an issue so we know to work on it!"
  exit 1
elif [[ "$OSTYPE" == "freebsd"* ]]; then
  echo "BSD is not yet supported. Please file an issue so we know to work on it!"
  exit 1
else
  echo "Unknown OS, please file an issue."
  exit 1
fi

# ocaml downloading
OPAMYES=1 opam init
eval $(opam config env)

OPAMYES=1 opam switch create 4.07.1
eval `opam env`

OPAMYES=1 opam install reason rtop
OPAMYES=1 opam pin add git@github.com:o1-labs/snarky.git
