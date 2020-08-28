#include <libff/algebra/fields/bigint.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_init.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_init.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_g1.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_g1.hpp>
#include <libff/algebra/curves/mnt/mnt4/mnt4_pp.hpp>
#include <libff/algebra/curves/mnt/mnt6/mnt6_pp.hpp>
#include <libff/common/profiling.hpp>
#include <stdarg.h>
#include <time.h>

// int normal_printf(const char* filename,
//                   int line,
//                   FILE* file,
//                   const char* format,
//                   va_list args) {
//     return vfprintf(file, format, args);
// }

// int json_printf(const char* filename,
//                 int line,
//                 FILE* file,
//                 const char* format,
//                 va_list args) {
//     time_t now;
//     struct tm* now_utc;

//     time(&now);
//     now_utc = gmtime(&now);
//     fprintf(file,
//             "{\"timestamp\":\"%d-%02d-%02d %02d:%02d:%02d.%06dZ",
//             now_utc->tm_year + 1900,
//             now_utc->tm_mon,
//             now_utc->tm_mday,
//             now_utc->tm_hour,
//             now_utc->tm_min,
//             now_utc->tm_sec,
//             0);
//     fprintf (file, "\",\"level\":\"%s\"", "DEBUG");
//     fprintf (file,
//              ",\"source\":{\"module\":\"%s\",\"location\":\"File \\\"%s\\\", line %d, characters 0-1\"},\"message\":\"",
//              "Snarky__Libsnark",
//              filename,
//              line);
//     const int size = 1024;
//     char inbuf[size];
//     char outbuf[size];
//     int ret = vsnprintf(inbuf, 1024, format, args);
//     int i;
//     int j;
//     for (i = j = 0; i < size - 1; i++, j++) {
//         if (inbuf[j] == '\0') {
//             outbuf[i] = '\0';
//             break;
//         } else if (inbuf[j] == '\n') {
//             outbuf[i] = '\\';
//             i++;
//             outbuf[i] = 'n';
//         } else if (inbuf[j] == '\\') {
//             outbuf[i] = '\\';
//             i++;
//             outbuf[i] = '\\';
//         } else {
//             outbuf[i] = inbuf[j];
//         }
//     }
//     if (outbuf[i] != '\0') {
//         outbuf[i+1] = '\0';
//     }
//     fputs(outbuf, file);
//     fputs("\",\"metadata\":{}}\n", file);
//     return ret;
// }

// int (*snarky_printf_deferred)(const char* filename,
//                               int line,
//                               FILE* file,
//                               const char* format,
//                               va_list args) = &normal_printf;

// FILE *snarky_print_dest = stdout;

// int may_close = false;

// int close_snarky_print_dest() {
//     if (snarky_print_dest) {
//         int ret = 0;
//         if (may_close) {
//             ret = fclose(snarky_print_dest);
//             may_close = false;
//         };
//         snarky_print_dest = NULL;
//         return ret;
//     } else {
//         return 0;
//     }
// }

// int snarky_printf(const char* filename, int line, const char* format, ...) {
//     if (snarky_print_dest) {
//         va_list args;
//         va_start(args, format);
//         int ret = (*snarky_printf_deferred)(filename, line, snarky_print_dest, format, args);
//         va_end(args);
//         return ret;
//     }
//     else {
//         return 0;
//     }
// }

extern "C" {

// void camlsnark_set_printing_off() {
//     close_snarky_print_dest();
// }

// void camlsnark_set_printing_stdout() {
//     close_snarky_print_dest();
//     snarky_print_dest = stdout;
// }

// void camlsnark_set_printing_stderr() {
//     close_snarky_print_dest();
//     snarky_print_dest = stderr;
// }

// void camlsnark_set_printing_file(char *file) {
//     close_snarky_print_dest();
//     may_close = true;
//     snarky_print_dest = fopen(file, "a");
// }

// void camlsnark_set_printing_normal() {
//     snarky_printf_deferred = &normal_printf;
// }

// void camlsnark_set_printing_json() {
//     snarky_printf_deferred = &json_printf;
// }

// void camlsnark_set_profiling(bool b) {
//   libff::inhibit_profiling_counters = b;
//   libff::inhibit_profiling_info = b;
// }

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

