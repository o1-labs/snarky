#include "libiop/algebra/field_subset/subspace.hpp"
#include "libiop/common/common.hpp"
#include "libiop/common/profiling.hpp"
#include "libiop/snark/common/bcs_common.hpp"

namespace libiop {

/** Initialize snark with FRI localization parameter array */
template<typename FieldT>
aurora_snark_parameters<FieldT>::aurora_snark_parameters(
    const size_t security_parameter,
    const LDT_reducer_soundness_type ldt_reducer_soundness_type,
    const FRI_soundness_type fri_soundness_type,
    const std::vector<size_t> FRI_localization_parameter_array,
    const size_t RS_extra_dimensions,
    const bool make_zk,
    const field_subset_type domain_type,
    const size_t num_constraints,
    const size_t num_variables) :
    security_parameter_(security_parameter),
    LDT_reducer_soundness_type_(ldt_reducer_soundness_type),
    FRI_soundness_type_(fri_soundness_type),
    RS_extra_dimensions_(RS_extra_dimensions),
    make_zk_(make_zk),
    domain_type_(domain_type),
    num_constraints_(num_constraints),
    num_variables_(num_variables),
    FRI_localization_parameter_array_(FRI_localization_parameter_array)
{
    this->initialize_bcs_params();
    this->initialize_iop_params();
}

/** Initialize snark with FRI localization parameter */
template<typename FieldT>
aurora_snark_parameters<FieldT>::aurora_snark_parameters(
    const size_t security_parameter,
    const LDT_reducer_soundness_type ldt_reducer_soundness_type,
    const FRI_soundness_type fri_soundness_type,
    const size_t FRI_localization_parameter,
    const size_t RS_extra_dimensions,
    const bool make_zk,
    const field_subset_type domain_type,
    const size_t num_constraints,
    const size_t num_variables) :
    security_parameter_(security_parameter),
    LDT_reducer_soundness_type_(ldt_reducer_soundness_type),
    FRI_soundness_type_(fri_soundness_type),
    RS_extra_dimensions_(RS_extra_dimensions),
    make_zk_(make_zk),
    domain_type_(domain_type),
    num_constraints_(num_constraints),
    num_variables_(num_variables),
    FRI_localization_parameter_(FRI_localization_parameter)
{
    this->initialize_bcs_params();
    this->initialize_iop_params();
}

template<typename FieldT>
void aurora_snark_parameters<FieldT>::reset_fri_localization_parameters(
    const std::vector<size_t> FRI_localization_parameter_array)
{
    this->FRI_localization_parameter_array_ = FRI_localization_parameter_array;
    this->initialize_iop_params();
}


template<typename FieldT>
void aurora_snark_parameters<FieldT>::initialize_iop_params()
{
    this->iop_params_ = aurora_iop_parameters<FieldT>(
        this->security_parameter_,
        this->RS_extra_dimensions_,
        this->make_zk_,
        this->domain_type_,
        this->num_constraints_,
        this->num_variables_);
    if (this->FRI_localization_parameter_array_.size() == 0) {
        this->iop_params_.set_ldt_parameters(this->FRI_localization_parameter_,
                                             this->FRI_soundness_type_,
                                             this->LDT_reducer_soundness_type_);
    } else {
        this->iop_params_.set_ldt_parameters(this->FRI_localization_parameter_array_,
                                             this->FRI_soundness_type_,
                                             this->LDT_reducer_soundness_type_);
        this->FRI_localization_parameter_array_ =
            this->iop_params_.FRI_params_.get_localization_parameters();
    }
}

template<typename FieldT>
void aurora_snark_parameters<FieldT>::initialize_bcs_params()
{
    this->bcs_params_.security_parameter = this->security_parameter_;
    this->bcs_params_.field_hasher = blake2b_field_element_hash<FieldT>;
    this->bcs_params_.zk_hasher = blake2b_zk_element_hash;
    this->bcs_params_.compression_hasher = blake2b_two_to_one_hash;
    this->bcs_params_.FieldT_randomness_extractor = blake2b_FieldT_randomness_extractor<FieldT>;
    this->bcs_params_.integer_randomness_extractor = blake2b_integer_randomness_extractor;
}

template<typename FieldT>
void aurora_snark_parameters<FieldT>::print() const
{
    print_indent(); printf("\nAurora SNARK parameters\n");
    print_indent(); printf("* security parameter (bits) = %zu\n", security_parameter_);
    print_indent(); printf("* RS extra dimensions = %zu\n", RS_extra_dimensions_);
    print_indent(); printf("* LDT reducer soundness type = %s\n",
        LDT_reducer_soundness_type_to_string(LDT_reducer_soundness_type_));
    print_indent(); printf("* FRI soundness type = %s\n",
        FRI_soundness_type_to_string(FRI_soundness_type_));
    print_indent(); printf("* zero-knowledge = %s\n", make_zk_ ? "true" : "false");
    print_indent(); printf("* domain type = %s\n", field_subset_type_names[this->domain_type_]);

    this->iop_params_.print();
}

template<typename FieldT>
aurora_snark_argument<FieldT> aurora_snark_prover(const r1cs_constraint_system<FieldT> &constraint_system,
                                                  const r1cs_primary_input<FieldT> &primary_input,
                                                  const r1cs_auxiliary_input<FieldT> &auxiliary_input,
                                                  const aurora_snark_parameters<FieldT> &parameters)
{
    enter_block("Aurora SNARK prover");
    parameters.print();

    bcs_prover<FieldT> IOP(parameters.bcs_params_);
    aurora_iop<FieldT> full_protocol(IOP, constraint_system, parameters.iop_params_);
    full_protocol.register_interactions();
    IOP.seal_interaction_registrations();
    full_protocol.register_queries();
    IOP.seal_query_registrations();

    full_protocol.produce_proof(primary_input, auxiliary_input);

    enter_block("Obtain transcript");
    const aurora_snark_argument<FieldT> transcript = IOP.get_transcript();
    leave_block("Obtain transcript");

    IOP.describe_sizes();

    leave_block("Aurora SNARK prover");
    return transcript;
}

template<typename FieldT>
bool aurora_snark_verifier(const r1cs_constraint_system<FieldT> &constraint_system,
                           const r1cs_primary_input<FieldT> &primary_input,
                           const aurora_snark_argument<FieldT> &proof,
                           const aurora_snark_parameters<FieldT> &parameters)
{
    enter_block("Aurora SNARK verifier");
    parameters.print();

    bcs_verifier<FieldT> IOP(parameters.bcs_params_, proof);

    enter_block("Aurora IOP protocol initialization and registration");
    aurora_iop<FieldT> full_protocol(IOP, constraint_system, parameters.iop_params_);
    full_protocol.register_interactions();
    IOP.seal_interaction_registrations();
    full_protocol.register_queries();
    IOP.seal_query_registrations();
    leave_block("Aurora IOP protocol initialization and registration");

    enter_block("Check semantic validity of IOP transcript");
    const bool IOP_transcript_valid = IOP.transcript_is_valid();
    leave_block("Check semantic validity of IOP transcript");

    const bool full_protocol_accepts = full_protocol.verifier_predicate(primary_input);

    print_indent(); printf("* IOP transcript valid: %s\n", IOP_transcript_valid ? "true" : "false");
    print_indent(); printf("* Full protocol decision predicate satisfied: %s\n", full_protocol_accepts ? "true" : "false");
    const bool decision = IOP_transcript_valid && full_protocol_accepts;
    leave_block("Aurora SNARK verifier");

    return decision;
}

} // namespace libiop
