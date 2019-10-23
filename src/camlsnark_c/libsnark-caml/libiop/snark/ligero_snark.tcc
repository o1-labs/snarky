#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include "libiop/common/profiling.hpp"
#include "libiop/snark/common/bcs_common.hpp"

namespace libiop {

template<typename FieldT>
void ligero_snark_parameters<FieldT>::describe()
{
    print_indent(); printf("Interleaved R1CS SNARK parameters:\n");
    print_indent(); printf("Security level: %zu\n", security_level_);
    print_indent(); printf("Height/width ratio: %f\n", height_width_ratio_);
    print_indent(); printf("RS extra dimensions: %zu\n", RS_extra_dimensions_);
    print_indent(); printf("Zero-knowledge: %d\n", make_zk_);
    print_indent(); printf("Domain type = %s\n", field_subset_type_names[this->domain_type_]);
}

template<typename FieldT>
bcs_transformation_parameters<FieldT> obtain_bcs_parameters_from_ligero_snark_params(
    const ligero_snark_parameters<FieldT> &parameters)
{
    bcs_transformation_parameters<FieldT> bcs_parameters;
    bcs_parameters.security_parameter = parameters.security_level_;
    bcs_parameters.field_hasher = blake2b_field_element_hash<FieldT>;
    bcs_parameters.zk_hasher = blake2b_zk_element_hash;
    bcs_parameters.compression_hasher = blake2b_two_to_one_hash;
    bcs_parameters.FieldT_randomness_extractor = blake2b_FieldT_randomness_extractor<FieldT>;
    bcs_parameters.integer_randomness_extractor = blake2b_integer_randomness_extractor;
    return bcs_parameters;
}

template<typename FieldT>
ligero_iop_parameters<FieldT> obtain_iop_parameters_from_ligero_snark_params(
    const ligero_snark_parameters<FieldT> &parameters,
    const std::size_t num_constraints,
    const std::size_t num_variables)
{
    ligero_iop_parameters<FieldT> iop_parameters(
        parameters.security_level_,
        parameters.LDT_reducer_soundness_type_,
        parameters.RS_extra_dimensions_,
        parameters.height_width_ratio_,
        parameters.make_zk_,
        parameters.domain_type_,
        num_constraints,
        num_variables);
    iop_parameters.print();
    return iop_parameters;
}

template<typename FieldT>
ligero_snark_argument<FieldT> ligero_snark_prover(const r1cs_constraint_system<FieldT> &constraint_system,
                                                  const r1cs_primary_input<FieldT> &primary_input,
                                                  const r1cs_auxiliary_input<FieldT> &auxiliary_input,
                                                  const ligero_snark_parameters<FieldT> &parameters)
{
    enter_block("Ligero SNARK prover");
    const bcs_transformation_parameters<FieldT> bcs_params =
        obtain_bcs_parameters_from_ligero_snark_params(parameters);
    const ligero_iop_parameters<FieldT> iop_params =
        obtain_iop_parameters_from_ligero_snark_params<FieldT>(
            parameters,
            constraint_system.num_constraints(),
            constraint_system.num_variables());

    bcs_prover<FieldT> IOP(bcs_params);
    ligero_iop<FieldT> full_protocol(IOP,
                                     constraint_system,
                                     iop_params);
    full_protocol.register_interactions();
    IOP.seal_interaction_registrations();
    full_protocol.register_queries();
    IOP.seal_query_registrations();

    full_protocol.produce_proof(primary_input, auxiliary_input);

    enter_block("Obtain transcript");
    const ligero_snark_argument<FieldT> transcript = IOP.get_transcript();
    leave_block("Obtain transcript");

    IOP.describe_sizes();

    leave_block("Ligero SNARK prover");
    return transcript;
}

template<typename FieldT>
bool ligero_snark_verifier(const r1cs_constraint_system<FieldT> &constraint_system,
                                     const r1cs_primary_input<FieldT> &primary_input,
                                     const ligero_snark_argument<FieldT> &proof,
                                     const ligero_snark_parameters<FieldT> &parameters)
{
    enter_block("Ligero SNARK verifier");
    const bcs_transformation_parameters<FieldT> bcs_params =
        obtain_bcs_parameters_from_ligero_snark_params(parameters);
    const ligero_iop_parameters<FieldT> iop_params =
        obtain_iop_parameters_from_ligero_snark_params<FieldT>(
            parameters,
            constraint_system.num_constraints(),
            constraint_system.num_variables());

    bcs_verifier<FieldT> IOP(bcs_params, proof);

    ligero_iop<FieldT> full_protocol(IOP,
                                     constraint_system,
                                     iop_params);
    full_protocol.register_interactions();
    IOP.seal_interaction_registrations();
    full_protocol.register_queries();
    IOP.seal_query_registrations();

    enter_block("Check semantic validity of IOP transcript");
    const bool IOP_transcript_valid = IOP.transcript_is_valid();
    leave_block("Check semantic validity of IOP transcript");

    enter_block("Check verifier predicate");
    const bool full_protocol_accepts = full_protocol.verifier_predicate(primary_input);
    leave_block("Check verifier predicate");

    print_indent(); printf("* IOP transcript valid: %s\n", IOP_transcript_valid ? "true" : "false");
    print_indent(); printf("* Full protocol decision predicate satisfied: %s\n", full_protocol_accepts ? "true" : "false");
    const bool decision = IOP_transcript_valid && full_protocol_accepts;
    leave_block("Ligero SNARK verifier");

    return decision;
}

} // namespace libiop
