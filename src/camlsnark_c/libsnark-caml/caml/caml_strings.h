// Defines bool
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

    const char* camlsnark_string_to_char_pointer(void* s);

    void* camlsnark_string_of_char_pointer(char* p, int length);

    void camlsnark_string_delete(void* s);

    int camlsnark_string_length(void* s);

#ifdef __cplusplus
}
#endif

