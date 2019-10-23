#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <stdexcept>
#include <string>


#ifndef CPPDEBUG /* Ubuntu's Boost does not provide binaries compatible with libstdc++'s debug mode so we just reduce functionality here */
#include <boost/program_options.hpp>
#endif

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/fields/gf128.hpp"
#include "libiop/algebra/fields/gf192.hpp"
#include "libiop/algebra/fields/gf256.hpp"
#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/profiling.hpp"

#ifndef CPPDEBUG
bool process_prover_command_line(const int argc, const char** argv,
                                 std::size_t &log_n_min,
                                 std::size_t &log_n_max,
                                 std::size_t &field_size)
{
    namespace po = boost::program_options;

    try
    {
        po::options_description desc("Usage");
        desc.add_options()
        ("help", "print this help message")
        ("log_n_min", po::value<std::size_t>(&log_n_min)->default_value(8))
        ("log_n_max", po::value<std::size_t>(&log_n_max)->default_value(20))
        ("field_size", po::value<std::size_t>(&field_size)->default_value(64));

        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);

        if (vm.count("help"))
        {
            std::cout << desc << "\n";
            return false;
        }

        po::notify(vm);
    }
    catch(std::exception& e)
    {
        std::cerr << "Error: " << e.what() << "\n";
        return false;
    }

    return true;
}
#endif

using namespace libiop;

template<typename FieldT>
void instrument_algebra(std::size_t log_n_min,
                        std::size_t log_n_max)
{
    if (log_n_min % 2 != 0)
    {
        log_n_min += 1;
    }
    if (log_n_max % 2 != 0)
    {
        log_n_max += 1;
    }

    for (std::size_t log_n = log_n_min; log_n <= log_n_max; log_n += 2)
    {
        print_separator();
        const std::size_t n = 1ul << log_n;
        print_indent(); printf("* size of n: %zu\n", n);
        const std::size_t sqrt_n = 1ul << (log_n / 2);
        print_indent(); printf("* size of sqrt(n): %zu\n", sqrt_n);

        /* FFT(n) */
        std::vector<FieldT> n_vec;
        enter_block("n");
        for (size_t i = 0; i < n; ++i)
        {
            n_vec.push_back(FieldT::random_element());
        }
        leave_block("n");
        affine_subspace<FieldT> n_subspace = linear_subspace<FieldT>::standard_basis(libiop::log2(n));
        enter_block("FFT(n)");
        std::vector<FieldT> fft_results = additive_FFT<FieldT>(n_vec, n_subspace);
        leave_block("FFT(n)");

        /* sqrt(n) * FFT(sqrt(n)) */
        std::vector<FieldT> sqrt_n_vec;
        enter_block("sqrt(n)");
        for (size_t i = 0; i < sqrt_n; ++i)
        {
            sqrt_n_vec.push_back(FieldT::random_element());
        }
        leave_block("sqrt(n)");
        affine_subspace<FieldT> sqrt_n_subspace = linear_subspace<FieldT>::standard_basis(libiop::log2(sqrt_n));
        enter_block("sqrt(n) * FFT(sqrt(n))");
        for (size_t i = 0; i < sqrt_n; ++i)
        {
            std::vector<FieldT> sqrt_results = additive_FFT<FieldT>(sqrt_n_vec, sqrt_n_subspace);
        }
        leave_block("sqrt(n) * FFT(sqrt(n))");
    }
}

int main(int argc, const char * argv[])
{
    std::size_t log_n_min;
    std::size_t log_n_max;
    std::size_t field_size;

#ifdef CPPDEBUG
    /* set reasonable defaults */
    if (argc > 1)
    {
        printf("There is no argument parsing in CPPDEBUG mode.");
        exit(1);
    }
    libiop::UNUSED(argv);

    log_n_min = 8;
    log_n_max = 20;
    field_size = 64;
#else
    if (!process_prover_command_line(argc, argv, log_n_min, log_n_max, field_size))
    {
        return 1;
    }
#endif

    printf("Selected parameters:\n");
    printf("* log_n_min = %zu\n", log_n_min);
    printf("* log_n_max = %zu\n", log_n_max);

    switch (field_size)
    {
        case 64:
            instrument_algebra<gf64>(log_n_min, log_n_max);
            break;
        case 128:
            instrument_algebra<gf128>(log_n_min, log_n_max);
            break;
        case 192:
            instrument_algebra<gf192>(log_n_min, log_n_max);
            break;
        case 256:
            instrument_algebra<gf256>(log_n_min, log_n_max);
            break;
        default:
            throw std::invalid_argument("Field size not supported.");
    }
}
