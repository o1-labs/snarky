add_library(
  snark_caml_common
  STATIC

  ../depends/libff/libff/common/double.cpp
  ../depends/libff/libff/common/profiling.cpp
  ../depends/libff/libff/common/utils.cpp
  common.cpp
)
target_link_libraries(
  snark_caml_common

  ${GMP_LIBRARIES}
  ${SODIUM_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${OPENSSL_CRYPTO_LIBRARY}
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

  ../depends/libff/libff/algebra/curves/bn128/bn128_g1.cpp
  ../depends/libff/libff/algebra/curves/bn128/bn128_g2.cpp
  ../depends/libff/libff/algebra/curves/bn128/bn128_gt.cpp
  ../depends/libff/libff/algebra/curves/bn128/bn128_init.cpp
  ../depends/libff/libff/algebra/curves/bn128/bn128_pairing.cpp
  ../depends/libff/libff/algebra/curves/bn128/bn128_pp.cpp
  caml_bn128.cpp
)
target_link_libraries(
  snark_caml_bn128

  snark_caml_common
  ${GMP_LIBRARIES}
  ${SODIUM_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${OPENSSL_CRYPTO_LIBRARY}
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

  ../depends/libff/libff/algebra/curves/mnt/mnt4/mnt4_g1.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt4/mnt4_g2.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt4/mnt4_init.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt4/mnt4_pairing.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt4/mnt4_pp.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt46_common.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt6/mnt6_g1.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt6/mnt6_g2.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt6/mnt6_init.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt6/mnt6_pairing.cpp
  ../depends/libff/libff/algebra/curves/mnt/mnt6/mnt6_pp.cpp
  caml_mnt6.cpp
  caml_mnt4.cpp
  caml_mnt4_specific.cpp
  caml_mnt6_specific.cpp
)
target_link_libraries(
  snark_caml_mnt298

  snark_caml_common
  ${GMP_LIBRARIES}
  ${SODIUM_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${OPENSSL_CRYPTO_LIBRARY}
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

  ../depends/libff/libff/algebra/curves/mnt753/mnt4753/mnt4753_g1.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt4753/mnt4753_g2.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt4753/mnt4753_init.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt4753/mnt4753_pairing.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt4753/mnt4753_pp.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt46753_common.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt6753/mnt6753_g1.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt6753/mnt6753_g2.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt6753/mnt6753_init.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt6753/mnt6753_pairing.cpp
  ../depends/libff/libff/algebra/curves/mnt753/mnt6753/mnt6753_pp.cpp
  caml_mnt6753.cpp
  caml_mnt4753.cpp
  caml_mnt4753_specific.cpp
  caml_mnt6753_specific.cpp
)
target_link_libraries(
  snark_caml_mnt753

  snark_caml_common
  ${GMP_LIBRARIES}
  ${SODIUM_LIBRARIES}
  ${GMPXX_LIBRARIES}
  ${OPENSSL_CRYPTO_LIBRARY}
  ${SNARK_EXTRALIBS}
)
target_include_directories(
  snark_caml_mnt753

  PUBLIC
  ${OPENSSL_INCLUDE_DIR}
  ${DEPENDS_DIR}/libff
  ${DEPENDS_DIR}/libfqfft
)
