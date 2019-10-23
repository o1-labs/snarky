#include <algorithm>
#include <cstdint>
#include <iostream>
#include <stdexcept>
#include <string>


#ifndef CPPDEBUG /* Ubuntu's Boost does not provide binaries compatible with libstdc++'s debug mode so we just reduce functionality here */
#include <boost/program_options.hpp>
#endif

#include "libiop/algebra/fields/gf64.hpp"
#include "libiop/algebra/fields/gf128.hpp"
#include "libiop/algebra/fields/gf192.hpp"
#include "libiop/algebra/fields/gf256.hpp"
#include <libff/algebra/curves/edwards/edwards_pp.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pp.hpp>
#include "libiop/algebra/fields/utils.hpp"

#include "libiop/snark/fractal_snark.hpp"
#include "libiop/snark/common/bcs_common.hpp"
#include "libiop/protocols/fractal_hiop.hpp"
#include "libiop/protocols/ldt/fri/argument_size_optimizer.hpp"
#include "libiop/relations/examples/r1cs_examples.hpp"

#ifndef CPPDEBUG
bool process_prover_command_line(const int argc, const char** argv,
                                 std::size_t &log_n_min,
                                 std::size_t &log_n_max,
                                 std::size_t &security_level,
                                 std::size_t &field_size,
                                 bool &heuristic_ldt_reducer_soundness,
                                 bool &heuristic_fri_soundness,
                                 bool &make_zk,
                                 bool &is_multiplicative,
                                 bool &optimize_localization)
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
            ("field_size", po::value<std::size_t>(&field_size)->default_value(192))
            ("heuristic_ldt_reducer_soundness", po::value<bool>(&heuristic_ldt_reducer_soundness)->default_value(true))
            ("heuristic_fri_soundness", po::value<bool>(&heuristic_fri_soundness)->default_value(true))
            ("make_zk", po::value<bool>(&make_zk)->default_value(false))
            ("is_multiplicative", po::value<bool>(&is_multiplicative)->default_value(false))
            ("optimize_localization", po::value<bool>(&optimize_localization)->default_value(false));

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
void print_argument_size(
    fractal_snark_parameters<FieldT> params,
    bcs_verifier_index<FieldT> index,
    fractal_snark_argument<FieldT> argument)
{
    /* We go through registration on the verifier to know what the domains look like */
    bcs_verifier<FieldT> verifier(params.bcs_params_, argument, index);
    fractal_iop<FieldT> full_protocol(verifier, params.iop_params_);
    full_protocol.register_interactions();
    verifier.seal_interaction_registrations();
    full_protocol.register_queries();
    verifier.seal_query_registrations();
    const bool holographic = true;
    print_detailed_transcript_data<FieldT>(
        holographic,
        argument,
        params.bcs_params_,
        verifier.get_MT_depths(),
        verifier.get_MT_zk_flags(),
        verifier.get_all_round_params());
}

template<typename FieldT>
void instrument_fractal_snark(
    const std::size_t log_n_min,
    const std::size_t log_n_max,
    std::size_t security_level,
    LDT_reducer_soundness_type ldt_reducer_soundness_type,
    FRI_soundness_type fri_soundness_type,
    const bool make_zk,
    const bool is_multiplicative,
    bool optimize_localization)
{
    // TODO: Unhard code this
    const size_t RS_extra_dimensions = 3;
    const size_t fri_localization_parameter = 2;
    field_subset_type domain_type = affine_subspace_type;
    if (is_multiplicative) {
        domain_type = multiplicative_coset_type;
    }

    for (std::size_t log_n = log_n_min; log_n <= log_n_max; ++log_n)
    {
        print_separator();

        const std::size_t n = 1ul << log_n;
        /* k+1 needs to be a power of 2 (proof system artifact) so we just fix it to 15 here */
        size_t k = 15;
        if (domain_type == multiplicative_coset_type)
        {
            k = 0;
        }
        const std::size_t m = n - 1;
        r1cs_example<FieldT> example = generate_r1cs_example<FieldT>(n, k, m);

        fractal_snark_parameters<FieldT> parameters(
            security_level,
            ldt_reducer_soundness_type,
            fri_soundness_type,
            fri_localization_parameter,
            RS_extra_dimensions,
            make_zk,
            domain_type,
            std::make_shared<r1cs_constraint_system<FieldT>>(example.constraint_system_));

        std::vector<std::size_t> localization_parameter_array;
        if (optimize_localization)
        {
            const size_t codeword_dim = parameters.iop_params_.codeword_domain().dimension();
            std::size_t num_query_sets = parameters.iop_params_.FRI_params_.query_repetitions();
            const size_t hash_size = (parameters.bcs_params_.security_parameter + 3) / 4;
            std::vector<size_t> oracle_locality_vector = parameters.iop_params_.locality_vector();
            // if (parameters.iop_params_.make_zk())
            // {
            //     /* Handle the zk leaves for the SNARK. TODO: Where should this go? */
            //     oracle_locality_vector[1] += 1;
            // }

            /* TODO: Get exact max tested degree */
            const size_t max_tested_degree = 6 * parameters.iop_params_.index_domain().num_elements();
            localization_parameter_array =
                compute_argument_size_optimal_localization_parameters<FieldT>(
                    oracle_locality_vector, codeword_dim, num_query_sets,
                    max_tested_degree, hash_size);

            parameters.reset_fri_localization_parameters(localization_parameter_array);
        }

        enter_block("Check satisfiability of R1CS example");
        const bool is_satisfied = example.constraint_system_.is_satisfied(
            example.primary_input_, example.auxiliary_input_);
        assert(is_satisfied);
        leave_block("Check satisfiability of R1CS example");
        printf("\n");
        print_indent(); printf("* R1CS number of constraints: %zu\n", example.constraint_system_.num_constraints());
        print_indent(); printf("* R1CS number of variables: %zu\n", example.constraint_system_.num_variables());
        print_indent(); printf("* R1CS number of variables for primary input: %zu\n", example.primary_input_.size());
        print_indent(); printf("* R1CS number of variables for auxiliary input: %zu\n", example.auxiliary_input_.size());
        print_indent(); printf("* R1CS size of constraint system (bytes): %zu\n", example.constraint_system_.size_in_bytes());
        print_indent(); printf("* R1CS size of primary input (bytes): %zu\n", example.primary_input_.size() * sizeof(FieldT));
        print_indent(); printf("* R1CS size of auxiliary input (bytes): %zu\n", example.auxiliary_input_.size() * sizeof(FieldT));
        printf("\n");

        std::pair<bcs_prover_index<FieldT>, bcs_verifier_index<FieldT>> index =
            fractal_snark_indexer(parameters);

        /** TODO: Print some useful data about the indexed data */

        const fractal_snark_argument<FieldT> argument =
            fractal_snark_prover(
                index.first,
                example.primary_input_,
                example.auxiliary_input_,
                parameters);

        parameters = fractal_snark_parameters<FieldT>(
            security_level,
            ldt_reducer_soundness_type,
            fri_soundness_type,
            fri_localization_parameter,
            RS_extra_dimensions,
            make_zk,
            domain_type,
            std::make_shared<r1cs_constraint_system<FieldT>>(example.constraint_system_));
        if (optimize_localization)
        {
            parameters.reset_fri_localization_parameters(localization_parameter_array);
        }

        print_argument_size(parameters, index.second, argument);

        const bool bit = fractal_snark_verifier<FieldT>(
            index.second,
            example.primary_input_,
            argument,
            parameters);

        printf("\n\n");

        print_indent(); printf("* Verifier satisfied: %s\n", bit ? "true" : "false");
    }
}

int main(int argc, const char * argv[])
{
    /* Set up R1CS */
    std::size_t log_n_min;
    std::size_t log_n_max;
    std::size_t security_level;
    std::size_t field_size;
    bool heuristic_ldt_reducer_soundness;
    bool heuristic_fri_soundness;
    bool make_zk;
    bool is_multiplicative;
    bool optimize_localization;

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
    heuristic_ldt_reducer_soundness = true;
    heuristic_fri_soundness = true;
    make_zk = false;
    is_multiplicative = false;
    optimize_localization = false;
#else
    if (!process_prover_command_line(argc, argv, log_n_min, log_n_max, security_level, field_size,
        heuristic_ldt_reducer_soundness, heuristic_fri_soundness, make_zk, is_multiplicative, optimize_localization))
    {
        return 1;
    }
#endif
    /** TODO: eventually get a string from program options, and then have a from string methods in protocols */
    LDT_reducer_soundness_type ldt_reducer_soundness_type = LDT_reducer_soundness_type::proven;
    if (heuristic_ldt_reducer_soundness)
    {
        ldt_reducer_soundness_type = LDT_reducer_soundness_type::optimistic_heuristic;
    }
    FRI_soundness_type fri_soundness_type = FRI_soundness_type::proven;
    if (heuristic_fri_soundness) {
        fri_soundness_type = FRI_soundness_type::heuristic;
    }
    start_profiling();

    printf("Selected parameters:\n");
    printf("- log_n_min = %zu\n", log_n_min);
    printf("- log_n_max = %zu\n", log_n_max);
    printf("- security_level = %zu\n", security_level);
    printf("- LDT_reducer_soundness_type = %s\n", LDT_reducer_soundness_type_to_string(ldt_reducer_soundness_type));
    printf("- FRI_soundness_type = %s\n", FRI_soundness_type_to_string(fri_soundness_type));
    printf("- is_multiplicative = %s\n", is_multiplicative ? "true" : "false");
    printf("- field_size = %zu\n", field_size);
    printf("- make_zk = %s\n", make_zk ? "true" : "false");

    if (is_multiplicative) {
        switch (field_size) {
            case 181:
                edwards_pp::init_public_params();
                instrument_fractal_snark<edwards_Fr>(
                    log_n_min, log_n_max, security_level,
                    ldt_reducer_soundness_type, fri_soundness_type,
                    make_zk, is_multiplicative, optimize_localization);
                break;
            case 256:
                libff::alt_bn128_pp::init_public_params();
                instrument_fractal_snark<libff::alt_bn128_Fr>(
                    log_n_min, log_n_max, security_level,
                    ldt_reducer_soundness_type, fri_soundness_type,
                    make_zk, is_multiplicative, optimize_localization);
                break;
            default:
                throw std::invalid_argument("Field size not supported.");
        }

    } else {
        switch (field_size)
        {
            case 64:
                instrument_fractal_snark<gf64>(
                    log_n_min, log_n_max, security_level,
                    ldt_reducer_soundness_type, fri_soundness_type,
                    make_zk, is_multiplicative, optimize_localization);
                break;
            case 128:
                instrument_fractal_snark<gf128>(
                    log_n_min, log_n_max, security_level,
                    ldt_reducer_soundness_type, fri_soundness_type,
                    make_zk, is_multiplicative, optimize_localization);
                break;
            case 192:
                instrument_fractal_snark<gf192>(
                    log_n_min, log_n_max, security_level,
                    ldt_reducer_soundness_type, fri_soundness_type,
                    make_zk, is_multiplicative, optimize_localization);
                break;
            case 256:
                instrument_fractal_snark<gf256>(
                    log_n_min, log_n_max, security_level,
                    ldt_reducer_soundness_type, fri_soundness_type,
                    make_zk, is_multiplicative, optimize_localization);
                break;
            default:
                throw std::invalid_argument("Field size not supported.");
        }
    }
}
