cmake_minimum_required(VERSION 2.8)

project (caml-libsnark-bindings)

add_library(
  caml-libsnark-bindings
  STATIC

  common.cpp
  caml_bn128.cpp
  caml_mnt6.cpp
  caml_mnt4.cpp
  caml_mnt4_specific.cpp
  caml_mnt6_specific.cpp
  caml_mnt6753.cpp
  caml_mnt4753.cpp
  caml_mnt4753_specific.cpp
  caml_mnt6753_specific.cpp
)

target_link_libraries(
  caml-libsnark-bindings

  ff
  ${GMP_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${CRYPTO_LIBRARIES}
  ${PROCPS_LIBRARIES}
  ${SNARK_EXTRALIBS}
)
target_include_directories(
  caml-libsnark-bindings

  PUBLIC
  ${OPENSSL_INCLUDE_DIR}
  ${DEPENDS_DIR}/libff
  ${DEPENDS_DIR}/libfqfft
  ../libsnark
)
