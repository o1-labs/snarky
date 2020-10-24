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

const char* camlsnark_string_to_char_pointer(std::string* s) {
  return s->c_str();
}

std::string* camlsnark_string_of_char_pointer(char* p, int length) {
  return new std::string(p, length);
}

void camlsnark_string_delete(std::string* s) {
  delete s;
}

int camlsnark_string_length(std::string* s) {
  return s->size();
}
