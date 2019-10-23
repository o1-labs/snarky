/** @file
 *****************************************************************************

 Implementation of functions for profiling code blocks.

 See profiling.hpp .

 *****************************************************************************
 * @author     This file is part of libff, developed by SCIPR Lab
 *             and contributors (see AUTHORS).
 * @copyright  MIT license (see LICENSE file)
 *****************************************************************************/

#include <cassert>
#include <chrono>
#include <cstdio>
#include <ctime>
#include <list>
#include <map>
#include <stdexcept>
#include <vector>

#include <string>
#include <sstream>
#include <iostream>

#include "libiop/common/common.hpp"

namespace libiop {

long long get_nsec_time()
{
    auto timepoint = std::chrono::high_resolution_clock::now();
    return std::chrono::duration_cast<std::chrono::nanoseconds>(timepoint.time_since_epoch()).count();
}

/* Return total CPU time consumsed by all threads of the process, in nanoseconds. */
long long get_nsec_cpu_time()
{
#if _MSC_VER
    return 0;
#else
    ::timespec ts;
    if ( ::clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts) )
        throw ::std::runtime_error("clock_gettime(CLOCK_PROCESS_CPUTIME_ID) failed");
        // If we expected this to work, don't silently ignore failures, because that would hide the problem and incur an unnecessarily system-call overhead. So if we ever observe this exception, we should probably add a suitable #ifdef .
        //TODO: clock_gettime(CLOCK_PROCESS_CPUTIME_ID) is not supported by native Windows. What about Cygwin? Should we #ifdef on CLOCK_PROCESS_CPUTIME_ID or on __linux__?
    return ts.tv_sec * 1000000000ll + ts.tv_nsec;
#endif
}

long long start_time, last_time;
long long start_cpu_time, last_cpu_time;

void start_profiling()
{
    printf("Reset time counters for profiling\n");

    last_time = start_time = get_nsec_time();
    last_cpu_time = start_cpu_time = get_nsec_cpu_time();
}

std::map<std::string, size_t> invocation_counts;
std::map<std::string, long long> enter_times;
std::map<std::string, long long> last_times;
std::map<std::string, long long> cumulative_times;
//TODO: Instead of analogous maps for time and cpu_time, use a single struct-valued map
std::map<std::string, long long> enter_cpu_times;
std::map<std::string, long long> last_cpu_times;
std::vector<std::string> block_names;

size_t indentation = 0;

bool inhibit_profiling_info = false;
bool inhibit_profiling_counters = false;

void clear_profiling_counters()
{
    invocation_counts.clear();
    last_times.clear();
    last_cpu_times.clear();
    cumulative_times.clear();
}

void print_cumulative_time_entry(const std::string &key, const long long factor)
{
    const double total_ms = (cumulative_times.at(key) * 1e-6);
    const size_t cnt = invocation_counts.at(key);
    const double avg_ms = total_ms / cnt;
    printf("   %-45s: %12.5fms = %lld * %0.5fms (%zu invocations, %0.5fms = %lld * %0.5fms per invocation)\n", key.c_str(), total_ms, factor, total_ms/factor, cnt, avg_ms, factor, avg_ms/factor);
}

void print_cumulative_times(const long long factor)
{
    printf("Dumping times:\n");
    for (auto& kv : cumulative_times)
    {
        print_cumulative_time_entry(kv.first, factor);
    }
}

static void print_times_from_last_and_start(long long     now, long long     last,
                                            long long cpu_now, long long cpu_last)
{
    long long time_from_start = now - start_time;
    long long time_from_last = now - last;

    long long cpu_time_from_start = cpu_now - start_cpu_time;
    long long cpu_time_from_last = cpu_now - cpu_last;

    if (time_from_last != 0) {
        double parallelism_from_last = 1.0 * cpu_time_from_last / time_from_last;
        printf("[%0.4fs x%0.2f]", time_from_last * 1e-9, parallelism_from_last);
    } else {
        printf("[             ]");
    }
    if (time_from_start != 0) {
        double parallelism_from_start = 1.0 * cpu_time_from_start / time_from_start;
        printf("\t(%0.4fs x%0.2f from start)", time_from_start * 1e-9, parallelism_from_start);
    }
}

void print_time(const char* msg)
{
    if (inhibit_profiling_info)
    {
        return;
    }

    long long now = get_nsec_time();
    long long cpu_now = get_nsec_cpu_time();

    printf("%-35s\t", msg);
    print_times_from_last_and_start(now, last_time, cpu_now, last_cpu_time);
    printf("\n");

    fflush(stdout);
    last_time = now;
    last_cpu_time = cpu_now;
}

void print_header(const char *msg)
{
    printf("\n================================================================================\n");
    printf("%s\n", msg);
    printf("================================================================================\n\n");
}

void print_separator()
{
    printf("\n================================================================================\n\n");
}

void print_indent()
{
    for (size_t i = 0; i < indentation; ++i)
    {
        printf("  ");
    }
}

void enter_block(const std::string &msg, const bool indent)
{
    if (inhibit_profiling_counters)
    {
        return;
    }

    block_names.emplace_back(msg);
    long long t = get_nsec_time();
    enter_times[msg] = t;
    long long cpu_t = get_nsec_cpu_time();
    enter_cpu_times[msg] = cpu_t;

    if (inhibit_profiling_info)
    {
        return;
    }

#ifdef MULTICORE
#pragma omp critical
#endif
    {
        print_indent();
        printf("(enter) %-35s\t", msg.c_str());
        print_times_from_last_and_start(t, t, cpu_t, cpu_t);
        printf("\n");
        fflush(stdout);

        if (indent)
        {
            ++indentation;
        }
    }
}

void leave_block(const std::string &msg, const bool indent)
{
    if (inhibit_profiling_counters)
    {
        return;
    }

#ifndef MULTICORE
    // printf("expected: %s\n", (*(--block_names.end())).c_str());
    // printf("got: %s\n", msg.c_str());
    assert(*(--block_names.end()) == msg);
#endif
    block_names.pop_back();

    ++invocation_counts[msg];

    long long t = get_nsec_time();
    last_times[msg] = (t - enter_times[msg]);
    cumulative_times[msg] += (t - enter_times[msg]);

    long long cpu_t = get_nsec_cpu_time();
    last_cpu_times[msg] = (cpu_t - enter_cpu_times[msg]);

    if (inhibit_profiling_info)
    {
        return;
    }

#ifdef MULTICORE
#pragma omp critical
#endif
    {
        if (indent)
        {
            --indentation;
        }

        print_indent();
        printf("(leave) %-35s\t", msg.c_str());
        print_times_from_last_and_start(t, enter_times[msg], cpu_t, enter_cpu_times[msg]);
        printf("\n");
        fflush(stdout);
    }
}

} // libiop
