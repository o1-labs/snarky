add_library(
  snark_caml_common
  STATIC

  common.cpp
)
target_link_libraries(
  snark_caml_common
  
  ff
  ${GMP_LIBRARIES}
  ${SODIUM_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${CRYPTO_LIBRARIES}
  ${PROCPS_LIBRARIES}
  ${SNARK_EXTRALIBS}
)
target_include_directories(
  snark_caml_common

  PUBLIC
  ${OPENSSL_INCLUDE_DIR}
  ${DEPENDS_DIR}/libff
  ${DEPENDS_DIR}/libfqfft
)

add_library(
  snark_caml_bn128
  STATIC

  caml_bn128.cpp
)
target_link_libraries(
  snark_caml_bn128
  
  ff
  snark_caml_common
  ${GMP_LIBRARIES}
  ${SODIUM_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${CRYPTO_LIBRARIES}
  ${PROCPS_LIBRARIES}
  ${SNARK_EXTRALIBS}
)
target_include_directories(
  snark_caml_bn128

  PUBLIC
  ${OPENSSL_INCLUDE_DIR}
  ${DEPENDS_DIR}/libff
  ${DEPENDS_DIR}/libfqfft
)

add_library(
  snark_caml_mnt298
  STATIC

  caml_mnt6.cpp
  caml_mnt4.cpp
  caml_mnt4_specific.cpp
  caml_mnt6_specific.cpp
)
target_link_libraries(
  snark_caml_mnt298
  
  ff
  snark_caml_common
  ${GMP_LIBRARIES}
  ${SODIUM_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${CRYPTO_LIBRARIES}
  ${SNARK_EXTRALIBS}
)
target_include_directories(
  snark_caml_mnt298

  PUBLIC
  ${OPENSSL_INCLUDE_DIR}
  ${DEPENDS_DIR}/libff
  ${DEPENDS_DIR}/libfqfft
)

add_library(
  snark_caml_mnt753
  STATIC

  caml_mnt6753.cpp
  caml_mnt4753.cpp
  caml_mnt4753_specific.cpp
  caml_mnt6753_specific.cpp
)
target_link_libraries(
  snark_caml_mnt753
  
  ff
  snark_caml_common
  ${GMP_LIBRARIES}
  ${SODIUM_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${CRYPTO_LIBRARIES}
  ${SNARK_EXTRALIBS}
)
target_include_directories(
  snark_caml_mnt753

  PUBLIC
  ${OPENSSL_INCLUDE_DIR}
  ${DEPENDS_DIR}/libff
  ${DEPENDS_DIR}/libfqfft
)
