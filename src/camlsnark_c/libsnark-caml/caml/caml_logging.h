// Defines bool
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

    void camlsnark_set_printing_off();

    void camlsnark_set_printing_stdout();

    void camlsnark_set_printing_file(char *file);

    void camlsnark_set_printing_fun(int (*pf)(char*));

    void camlsnark_puts(const char* str);

    /* void camlsnark_set_profiling(bool b); */

    /* // long vector */
    /* void* camlsnark_long_vector_create(); */

    /* int camlsnark_long_vector_length(void *v); */

    /* void camlsnark_long_vector_emplace_back(void *v, long x); */

    /* long camlsnark_long_vector_get(void *v, int i); */

    /* void camlsnark_long_vector_delete(void* v); */

    /* // bool vector */
    /* void* camlsnark_bool_vector_create(); */

    /* int camlsnark_bool_vector_length(void*v); */

    /* void camlsnark_bool_vector_emplace_back(void* v, bool x); */

    /* bool camlsnark_bool_vector_get(void* v, int i); */

    /* void camlsnark_bool_vector_delete(void* v); */

    /* // int vector */
    /* void* camlsnark_int_vector_create(); */

    /* int camlsnark_int_vector_length(void*v); */

    /* void camlsnark_int_vector_emplace_back(void* v, int x); */

    /* int camlsnark_int_vector_get(void* v, int i); */

    /* void camlsnark_int_vector_delete(void* v); */

    /* const char* camlsnark_string_to_char_pointer(void* s); */

    /* void* camlsnark_string_of_char_pointer(char* p, int length); */

    /* void camlsnark_string_delete(void* s); */

    /* int camlsnark_string_length(void* s); */

#ifdef __cplusplus
}
#endif

