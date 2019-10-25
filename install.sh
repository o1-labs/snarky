#!/bin/bash
set -e # Exit on error
set -u # Fail if an undefined variable is used

which opam
if [ $0 ]; then
  echo "Found opam."
else
  echo "Installing opam..."
  sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
  echo "Installed opam."
fi

OS_NAME=$(uname)
if [ "$OS_NAME" = 'Linux' ]; then

  echo "Found a linux distribution." ;

  # Find the Linux distro
  if [ -n "$(command -v lsb_release)" ]; then
	DISTRO=$(lsb_release --short --id)
  elif [ -f "/etc/os-release" ]; then
	DISTRO=$(grep NAME /etc/os-release | sed 's/NAME=//g' | tr -d '="')
  elif [ -f "/etc/debian_version" ]; then
	DISTRO="Debian"
  elif [ -f "/etc/redhat-release" ]; then
	DISTRO="Fedora"
  else
    DISTRO="Unknown"
  fi

  if [ "$DISTRO" = 'Ubuntu' ]; then
    echo "Installing libsnark dependencies for Ubuntu..."
    sudo apt-get install build-essential cmake git libgmp3-dev libboost-all-dev libssl-dev
    # Install the right version of libprocps-dev for the release
    RELEASE=$(lsb_release -s --release)
    if [ "$RELEASE" = '18.04' ]; then
      sudo apt-get install libprocps-dev
    elif [ "$RELEASE" = '16.04' ]; then
      sudo apt-get install libprocps4-dev
    elif [ "$RELEASE" = '14.04' ]; then
      sudo apt-get install libprocps3-dev
    else
      # Try all of the different packages for libprocps-dev, in order of package recency
      sudo apt-get install libprocps-dev || sudo apt-get install libprocps4-dev || sudo apt-get install libprocps3-dev
    fi
    echo "Installed libsnark dependencies."

  elif [ "$DISTRO" = 'Fedora' ]; then
    echo "Installing libsnark dependencies for Fedora..."
    sudo yum install gcc-c++ cmake make git gmp-devel procps-ng-devel
    echo "Installed libsnark dependencies."

  elif [ "$DISTRO" = 'Debian' ]; then
    echo "Installing libsnark dependencies for Debian..."
    sudo apt-get install build-essential cmake git libgmp3-dev libboost-all-dev libssl-dev libprocps-dev
    echo "Installed libsnark dependencies."

  else
    echo "Unrecognised Linux distribution: $DISTRO"
  fi

elif [ "$OS_NAME" = 'Darwin' ]; then
  echo "Found OS X."

  which brew
  if [ $0 ]; then
    echo "Found homebrew."
  else
    echo "Installing homebrew..."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    echo "Installed homebrew."
  fi

  PACKAGES="gpatch opam cmake gmp pkg-config openssl libffi libsodium boost zlib libomp"

  echo -n "Found already installed packages:"

  # removing already installed packages from the list
  for p in $(env HOMEBREW_NO_AUTO_UPDATE=1 brew list); do
    echo -n "$p"
    PACKAGES=${PACKAGES//$p/}
  done;

  # only run if there's work to do
  if [[ $PACKAGES = *[![:space:]]* ]]; then
    echo "Installing packages: $p"
    yes | env HOMEBREW_NO_AUTO_UPDATE=1 brew install $PACKAGES
  else
    echo 'All brew packages have already been installed.'
  fi

else
  echo 'Unrecognised operating system: $OS_NAME'
fi
