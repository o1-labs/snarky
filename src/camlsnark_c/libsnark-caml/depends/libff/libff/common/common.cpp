#include <libff/algebra/fields/bigint.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_init.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_init.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_g1.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_g1.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_pp.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_pp.hpp>
#include <libff/common/logging.hpp>
#include <libff/common/profiling.hpp>
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

void camlsnark_set_profiling(bool b) {
  libff::inhibit_profiling_counters = b;
  libff::inhibit_profiling_info = b;
}

const libff::bigint<libff::mnt6_q_limbs>* camlsnark_mnt6_final_exponent_last_chunk_abs_of_w0 =
  &libff::mnt6_final_exponent_last_chunk_abs_of_w0;

const libff::bigint<libff::mnt6_q_limbs>* camlsnark_mnt6_final_exponent_last_chunk_w1 =
  &libff::mnt6_final_exponent_last_chunk_w1;

// long vector
std::vector<long>* camlsnark_long_vector_create() {
  return new std::vector<long>();
}

int camlsnark_long_vector_length(std::vector<long> *v) {
  return v->size();
}

void camlsnark_long_vector_emplace_back(std::vector<long>* v, long x) {
  v->emplace_back(x);
}

long camlsnark_long_vector_get(std::vector<long>* v, int i) {
  return (*v)[i];
}

void camlsnark_long_vector_delete(std::vector<long>* v) {
  delete v;
}

// bool vector
std::vector<bool>* camlsnark_bool_vector_create() {
  return new std::vector<bool>();
}

int camlsnark_bool_vector_length(std::vector<bool> *v) {
  return v->size();
}

void camlsnark_bool_vector_emplace_back(std::vector<bool>* v, bool x) {
  v->emplace_back(x);
}

bool camlsnark_bool_vector_get(std::vector<bool>* v, int i) {
  return (*v)[i];
}

void camlsnark_bool_vector_delete(std::vector<bool>* v) {
  delete v;
}

// int vector
std::vector<int>* camlsnark_int_vector_create() {
  return new std::vector<int>();
}

int camlsnark_int_vector_length(std::vector<int> *v) {
  return v->size();
}

void camlsnark_int_vector_emplace_back(std::vector<int>* v, int x) {
  v->emplace_back(x);
}

int camlsnark_int_vector_get(std::vector<int>* v, int i) {
  return (*v)[i];
}

void camlsnark_int_vector_delete(std::vector<int>* v) {
  delete v;
}

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

#ifdef __cplusplus
}
#endif
