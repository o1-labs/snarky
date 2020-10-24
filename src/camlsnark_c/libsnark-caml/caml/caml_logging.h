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

#ifdef __cplusplus
}
#endif

