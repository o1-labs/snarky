/** @file
 *****************************************************************************

 Declaration of functions for profiling code blocks.

 Reports time, operation counts, memory usage, and others.

 *****************************************************************************
 * @author     This file is adapted from libsnark, developed by SCIPR Lab
 *             and contributors
 *             (see AUTHORS for libsnark and here for libiop).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#ifndef LIBIOP_COMMON_PROFILING_HPP_
#define LIBIOP_COMMON_PROFILING_HPP_

#include <cstddef>
#include <map>
#include <string>
#include <vector>

namespace libiop {

void start_profiling();
long long get_nsec_time();
void print_time(const char* msg);
void print_header(const char* msg);
void print_separator();

void print_indent();

extern bool inhibit_profiling_info;
extern bool inhibit_profiling_counters;
extern std::map<std::string, size_t> invocation_counts;
extern std::map<std::string, long long> last_times;
extern std::map<std::string, long long> cumulative_times;

void clear_profiling_counters();

void print_cumulative_time_entry(const std::string &key, const long long factor=1);
void print_cumulative_times(const long long factor=1);

void enter_block(const std::string &msg, const bool indent=true);
void leave_block(const std::string &msg, const bool indent=true);

} // libiop

#endif // LIBIOP_COMMON_PROFILING_HPP_
