#include <libff/algebra/fields/bigint.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_init.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_init.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_g1.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_g1.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_pp.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_pp.hpp>
#include <libff/common/profiling.hpp>
#include <stdarg.h>

int (*snarky_printf_deferred)(const char* format, va_list args) = &vprintf;

int snarky_printf(const char* format, ...)
{
    va_list args;
    va_start(args, format);
    int ret = (*snarky_printf_deferred)(format, args);
    va_end(args);
    return ret;
}

int no_print(const char* format, va_list args) {
    return 0;
}

FILE *snarky_print_dest = NULL;

int close_snarky_print_dest() {
    if (snarky_print_dest) {
        return fclose(snarky_print_dest);
    } else {
        return 0;
    }
}

int file_print(const char* format, va_list args) {
    return vfprintf(snarky_print_dest, format, args);
}

int (*snarky_print_func)(char*) = NULL;

int snarky_print_to_func(const char* format, va_list args) {
    char output[1024];
    int true_size = vsnprintf(output, 1024, format, args);
    if (true_size >= 0) {
        return (*snarky_print_func)(output);
    } else {
        return (-true_size);
    }
}

extern "C" {

void camlsnark_set_printing_off() {
    snarky_printf_deferred = &no_print;
    close_snarky_print_dest();
}

void camlsnark_set_printing_stdout() {
    snarky_printf_deferred = &vprintf;
    close_snarky_print_dest();
}

void camlsnark_set_printing_file(char *file) {
    snarky_printf_deferred = &no_print;
    close_snarky_print_dest();
    snarky_print_dest = fopen(file, "a");
    if (snarky_print_dest) {
        snarky_printf_deferred = &file_print;
    } else {
        // Fail silently..
    }
}

void camlsnark_set_printing_fun(int (*pf)(char*)) {
    snarky_print_func = pf;
    snarky_printf_deferred = &snarky_print_to_func;
    close_snarky_print_dest();
}

void camlsnark_puts(const char* str) {
    puts(str);
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

}

