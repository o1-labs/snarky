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

#ifdef __cplusplus
extern "C" {
#endif

void camlsnark_set_printing_off() {
    libff_set_printing_off();
}

void camlsnark_set_printing_stdout() {
    libff_set_printing_stdout();
}

void camlsnark_set_printing_stderr() {
    libff_set_printing_stderr();
}

void camlsnark_set_printing_file(char *file) {
    libff_set_printing_file(file);
}

void camlsnark_set_printing_normal() {
    libff_set_printing_normal();
}

void camlsnark_set_printing_json() {
    libff_set_printing_json();
}

#ifdef __cplusplus
}
#endif

