#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <stdexcept>
#include <string>

#ifndef CPPDEBUG /* Ubuntu's Boost does not provide binaries compatible with libstdc++'s debug mode so we just reduce functionality here */
#include <boost/program_options.hpp>
#endif

#include "libff/algebra/curves/edwards/edwards_pp.hpp"
#include "libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp"

#include "libiop/algebra/fields/utils.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/fields/gf128.hpp"
#include "libiop/algebra/fields/gf192.hpp"
#include "libiop/algebra/fields/gf256.hpp"
#include "libiop/algebra/field_subset/subgroup.hpp"

#include "libiop/common/common.hpp"
#include "libiop/iop/iop.hpp"
#include "libiop/protocols/ldt/fri/fri_ldt.hpp"
#include "libiop/snark/fri_snark.hpp"

#ifndef CPPDEBUG
bool process_prover_command_line(const int argc, const char** argv,
                                 std::size_t &log_n_min,
                                 std::size_t &log_n_max,
                                 std::size_t &security_level,
                                 std::size_t &field_size,
                                 bool &is_multiplicative,
                                 std::size_t &localization_parameter,
                                 std::size_t &num_localization_steps,
                                 std::size_t &num_oracles,
                                 std::size_t &num_interactive_repetitions,
                                 std::size_t &num_query_repetitions)
{
    namespace po = boost::program_options;

    try
    {
        po::options_description desc("Usage");
        desc.add_options()
        ("help", "print this help message")
        ("log_n_min", po::value<std::size_t>(&log_n_min)->default_value(8))
        ("log_n_max", po::value<std::size_t>(&log_n_max)->default_value(20))
        ("security_level", po::value<std::size_t>(&security_level)->default_value(128))
        ("field_size", po::value<std::size_t>(&field_size)->default_value(64))
        ("is_multiplicative", po::value<bool>(&is_multiplicative)->default_value(false))
        ("localization_parameter", po::value<std::size_t>(&localization_parameter)->default_value(2), "Only used when num_localization_steps is 0")
        ("num_localization_steps", po::value<std::size_t>(&num_localization_steps)->default_value(0))
        ("num_oracles", po::value<std::size_t>(&num_oracles)->default_value(1))
        ("num_interactive_repetitions", po::value<std::size_t>(&num_interactive_repetitions)->default_value(1))
        ("num_query_repetitions", po::value<std::size_t>(&num_query_repetitions)->default_value(64));

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
void instrument_FRI(std::size_t log_n_min,
                    std::size_t log_n_max,
                    std::size_t security_level,
                    bool is_multiplicative,
                    std::size_t localization_parameter,
                    std::size_t num_localization_steps,
                    std::size_t num_oracles,
                    std::size_t num_interactive_repetitions,
                    std::size_t num_query_repetitions)
{
    for (std::size_t log_n = log_n_min; log_n <= log_n_max; ++log_n)
    {
        print_separator();
        const std::size_t poly_degree_bound = 1ull << log_n;
        const std::size_t RS_extra_dimensions = 2; /* \rho = 2^{-RS_extra_dimensions} */
        const std::size_t codeword_domain_dim = log_n + RS_extra_dimensions;


        std::vector<std::size_t> localization_parameter_array;
        if (num_localization_steps != 0)
        {
            std::size_t remaining = codeword_domain_dim - RS_extra_dimensions - 1;
            std::size_t vals = remaining / num_localization_steps;
            localization_parameter_array = std::vector<std::size_t>(num_localization_steps, vals);
            localization_parameter_array.insert(localization_parameter_array.begin(), 1);
        }

        std::cout << "Codeword domain dimension: " << codeword_domain_dim << "\n"
                << "RS_extra_dimensions: " << RS_extra_dimensions << "\n"
                << "poly_degree_bound: " << poly_degree_bound << "\n"
                << "\n";

        const polynomial<FieldT> poly = polynomial<FieldT>::random_polynomial(poly_degree_bound);

        /* Set up the protocol blueprint */
        iop_protocol<FieldT> IOP;

        const std::size_t codeword_domain_size = 1ull << codeword_domain_dim;

        FRI_snark_parameters<FieldT> params;
        params.codeword_domain_dim_ = codeword_domain_dim;
        params.security_level_ = security_level;
        params.RS_extra_dimensions_ = RS_extra_dimensions;
        params.localization_parameter_array_ = localization_parameter_array;
        params.localization_parameter_ = localization_parameter;
        params.num_interactive_repetitions_ = num_interactive_repetitions;
        params.num_query_repetitions_ = num_query_repetitions;
        params.field_type_ = get_field_type<FieldT>(FieldT::zero());
        params.num_oracles_ = num_oracles;

        const FRI_snark_proof<FieldT> proof = FRI_snark_prover<FieldT>(params);
        printf("\n");
        print_indent(); printf("* Argument size in bytes (IOP): %zu\n", proof.IOP_size_in_bytes());
        print_indent(); printf("* Argument size in bytes (BCS): %zu\n", proof.BCS_size_in_bytes());
        print_indent(); printf("* Argument size in bytes (total): %zu\n", proof.size_in_bytes());

        printf("\nIf we were to remove the pruning of BCS merkle tree paths feature,\n"
               "the argument would have the following sizes:\n");
        print_indent(); printf("* Argument size in bytes (BCS, no pruning): %zu\n", proof.BCS_size_in_bytes_without_pruning());
        print_indent(); printf("* Argument size in bytes (total, no pruning): %zu\n", proof.size_in_bytes_without_pruning());
        printf("\n");

        const bool bit = FRI_snark_verifier<FieldT>(proof, params);

        libiop::print_indent(); printf("* Verifier satisfied: %s\n", bit ? "true" : "false");
    }
}

int main(int argc, const char * argv[])
{
    std::size_t log_n_min;
    std::size_t log_n_max;
    std::size_t security_level;
    std::size_t field_size;
    bool is_multiplicative;
    std::size_t localization_parameter;
    std::size_t num_localization_steps;
    std::size_t num_oracles;
    std::size_t num_interactive_repetitions;
    std::size_t num_query_repetitions;

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
    security_level = 128;
    field_size = 64;
    is_multiplicative = false;
    localization_parameter = 2;
    num_localization_steps = 0;
    num_oracles = 1;
    num_interactive_repetitions = 1;
    num_query_repetitions = 10;
#else
    if (!process_prover_command_line(argc, argv, log_n_min, log_n_max, security_level,
                                     field_size, is_multiplicative, localization_parameter, num_localization_steps,
                                     num_oracles, num_interactive_repetitions, num_query_repetitions))
    {
        return 1;
    }
#endif

    start_profiling();

    printf("Selected parameters:\n");
    printf("* log_n_min = %zu\n", log_n_min);
    printf("* log_n_max = %zu\n", log_n_max);
    printf("* security_level = %zu\n", security_level);

    if (is_multiplicative) {
        switch (field_size) {
            case 181:
                edwards_pp::init_public_params();
                instrument_FRI<edwards_Fr>(log_n_min, log_n_max, security_level, true,
                                   localization_parameter, num_localization_steps, num_oracles,
                                   num_interactive_repetitions, num_query_repetitions);
                break;
            case 256:
                libff::alt_bn128_pp::init_public_params();
                instrument_FRI<alt_bn128_Fr>(log_n_min, log_n_max, security_level, true,
                                   localization_parameter, num_localization_steps, num_oracles,
                                   num_interactive_repetitions, num_query_repetitions);
                break;
            default:
                throw std::invalid_argument("Field size not supported.");
        }
    } else {
        switch (field_size)
        {
            case 64:
                instrument_FRI<gf64>(log_n_min, log_n_max, security_level, false,
                                   localization_parameter, num_localization_steps, num_oracles,
                                   num_interactive_repetitions, num_query_repetitions);
                break;
            case 128:
                instrument_FRI<gf128>(log_n_min, log_n_max, security_level, false,
                                   localization_parameter, num_localization_steps, num_oracles,
                                   num_interactive_repetitions, num_query_repetitions);
                break;
            case 192:
                instrument_FRI<gf192>(log_n_min, log_n_max, security_level, false,
                                   localization_parameter, num_localization_steps, num_oracles,
                                   num_interactive_repetitions, num_query_repetitions);
                break;
            case 256:
                instrument_FRI<gf256>(log_n_min, log_n_max, security_level, false,
                                   localization_parameter, num_localization_steps, num_oracles,
                                   num_interactive_repetitions, num_query_repetitions);
                break;
            default:
                throw std::invalid_argument("Field size not supported.");
        }
    }
}
