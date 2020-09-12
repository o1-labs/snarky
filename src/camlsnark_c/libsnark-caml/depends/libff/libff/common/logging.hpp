/** @file
 *****************************************************************************
 *****************************************************************************
 * @author     This file is part of libff, developed by o1-labs
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef LOGGING_HPP_
#define LOGGING_HPP_

// #include <cassert>
// #include <iostream>
// #include <sstream>
// #include <string>
// #include <vector>

// #include <stdarg.h>

int libff_printf (const char* filename, int line, const char* format, ...);

#define printf(...) libff_printf(__FILE__, __LINE__, __VA_ARGS__)

#ifdef __cplusplus
extern "C" {
#endif

    void libff_set_printing_off();
    void libff_set_printing_stdout();
    void libff_set_printing_stderr();
    void libff_set_printing_file(char *file);
    void libff_set_printing_normal();
    void libff_set_printing_json();

#ifdef __cplusplus
}
#endif

#endif // LOGGING_HPP_
