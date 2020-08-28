/** @file
 *****************************************************************************
 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef DEBUG_PRINT_HPP_
#define DEBUG_PRINT_HPP_

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

int snarky_printf (const char* filename, int line, const char* format, ...);

#define printf(...) snarky_printf(__FILE__, __LINE__, __VA_ARGS__)

#endif // DEBUG_PRINT_HPP_
