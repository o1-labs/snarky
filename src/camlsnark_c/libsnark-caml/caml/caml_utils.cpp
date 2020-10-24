#include <libff/algebra/fields/bigint.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_init.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_init.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_g1.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_g1.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_pp.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_pp.hpp>
#include <libff/common/profiling.hpp>
#include <libff/common/logging.hpp>

#include <stdarg.h>
#include <time.h>

void camlsnark_set_profiling(bool b) {
  libff::inhibit_profiling_counters = b;
  libff::inhibit_profiling_info = b;
}

const libff::bigint<libff::mnt6_q_limbs>* camlsnark_mnt6_final_exponent_last_chunk_abs_of_w0 =
  &libff::mnt6_final_exponent_last_chunk_abs_of_w0;

const libff::bigint<libff::mnt6_q_limbs>* camlsnark_mnt6_final_exponent_last_chunk_w1 =
  &libff::mnt6_final_exponent_last_chunk_w1;
