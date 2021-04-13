#!/bin/bash

# Do nothing if the C++ build directory is already present
if [ -d libsnark-caml/build ] | [ -f LIBSNARK_STUBS_CMAKE_PID ] ; then
  # Wait if this script is currently running
  while [ -f LIBSNARK_STUBS_CMAKE_PID ] && ps $(cat LIBSNARK_STUBS_CMAKE_PID || true) > /dev/null; do
    sleep 1s
  done
  exit 0
fi

echo $$ > LIBSNARK_STUBS_CMAKE_PID

if [ ! -v SNARKY_PERFORMANCE ] ||
   [ "$SNARKY_PERFORMANCE" = "off" ] ||
   [ "$SNARKY_PERFORMANCE" = "OFF" ] ||
   [ "$SNARKY_PERFORMANCE" = "false" ] ||
   [ "$SNARKY_PERFORMANCE" = "FALSE" ]
then
  SNARKY_PERFORMANCE="OFF"
else
  SNARKY_PERFORMANCE="ON"
fi
if [ $(uname) = 'Linux' ]; then
  pushd libsnark-caml
  mkdir -p build
  pushd build
    cmake -DPERFORMANCE=$SNARKY_PERFORMANCE \
      -DMULTICORE=ON \
      -DUSE_PT_COMPRESSION=OFF \
      -DUSE_ASM=ON \
      -DOPT_FLAGS='-ggdb3 -O2' ..
  popd
  popd
elif [ $(uname) = 'Darwin' ]; then
  pushd libsnark-caml
  mkdir -p build
  pushd build
    BOOST_ROOT=/usr/local/Cellar/boost/1.68.0_1 \
    BOOST_INCLUDEDIR=/usr/local/Cellar/boost/1.68.0_1/include \
    CPPFLAGS='-I/usr/local/opt/openssl/include-I/usr/local/opt/boost/include -I/usr/local/opt/gmp/include -I/usr/local/include -L/usr/local/lib -I/usr/local/include/gmp.h' \
    CFLAGS='-I/usr/local/include' \
    CXXFLAGS='-I/usr/local/include' \
    LDFLAGS='-L/usr/local/opt/openssl/lib -L/usr/local/opt/boost/lib -L/usr/local/opt/gmp/lib' \
    PKG_CONFIG_PATH=/usr/local/opt/openssl/lib/pkgconfig \
    cmake \
      -DPERFORMANCE=OFF \
      -DUSE_ASM=ON \
      -DOPT_FLAGS='-ggdb3 -O2' \
      -DMULTICORE=ON \
      -DUSE_PT_COMPRESSION=OFF \
      -DWITH_SUPERCOP=OFF \
      -DWITH_PROCPS=OFF \
      -DCMAKE_LIBRARY_PATH=/usr/local/opt/gmp/lib \
      -DCMAKE_RULE_MESSAGES:BOOL=OFF \
      -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON ..
  popd
  popd
else
    echo I do not know how to build libsnark on $(uname)
    exit 1
fi

rm LIBSNARK_STUBS_CMAKE_PID 
