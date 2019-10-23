#include <cassert>
#include <stdexcept>

namespace libiop {

template<typename FieldT>
LDT_instance_reducer_params<FieldT>::LDT_instance_reducer_params(
    const size_t interactive_soundness_bits,
    const LDT_reducer_soundness_type soundness_type,
    const size_t codeword_domain_dim,
    const size_t max_tested_degree_bound,
    const size_t max_constraint_degree_bound,
    const bool make_zk) :
    interactive_soundness_bits_(interactive_soundness_bits),
    soundness_type_(soundness_type),
    codeword_domain_dim_(codeword_domain_dim),
    max_tested_degree_bound_(max_tested_degree_bound),
    max_constraint_degree_bound_(max_constraint_degree_bound),
    make_zk_(make_zk)
{
    const size_t codeword_domain_size = 1ull << this->codeword_domain_dim_;
    if (this->soundness_type_ == LDT_reducer_soundness_type::proven)
    {
        /** Per the RS-encoded IOP to standard IOP compiler's soundness,
         *  absolute_proximity_parameter < min(codeword_domain_size - max_constraint_degree,
         *                                    (codeword_domain_size - 2*max_tested_degree) / 2,
         *                                    (codeword_domain_size - max_tested_degree) / 3))
         * We subtract 1, in order to force the less than property. */
        this->absolute_proximity_parameter_ =
            std::min({codeword_domain_size - max_constraint_degree_bound,
                    (codeword_domain_size - 2*max_tested_degree_bound) / 2,
                    (codeword_domain_size - max_tested_degree_bound) / 3}) - 1;
    } else if (this->soundness_type_ == LDT_reducer_soundness_type::optimistic_heuristic)
    {
        /** This is essentially a minimal upper bound on how good the LDT reducer's proximity parameter can be.
         *  absolute_proximity_parameter < min(codeword_domain_size - max_constraint_degree,
         *                                     codeword_domain_size - max_tested_degree))
         * We subtract 1, in order to force the less than property. */
        this->absolute_proximity_parameter_ =
            std::min({codeword_domain_size - max_constraint_degree_bound,
                      codeword_domain_size - max_tested_degree_bound}) - 1;
    }
    this->fractional_proximity_parameter_ =
        (long double)(this->absolute_proximity_parameter_) / (long double)(codeword_domain_size);
    /** Interactive soundness error is (codeword_domain_size / F)^(num_ldt_instances)
     *  Consequently the bits of security is:
     *      -Interactive_soundness_bits = (num_ldt_instances) * log(codeword_domain_size / |F|)
     *  Rearranging, we get:
     *      num_ldt_instances = ceil(-Interactive_soundness_bits / log(codeword_domain_size / |F|))
     *  We compute the logarithm using the identity:
     *      log(codeword_domain_size / |F|) = codeword_domain_dimension - log(|F|)
     */
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    const long double denominator = (long double)(this->codeword_domain_dim_) - field_size_bits;
    const long double num_ldt_instances = ceil(-1.0 * interactive_soundness_bits / denominator);
    this->num_output_LDT_instances_ = std::max<size_t>(1, size_t(num_ldt_instances));
}

template<typename FieldT>
long double LDT_instance_reducer_params<FieldT>::achieved_soundness() const
{
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    const long double soundness_bits_per_instance = (long double)(this->codeword_domain_dim_) - field_size_bits;
    const long double casted_num_ldt_instances = (long double)(this->num_output_LDT_instances_);
    return -soundness_bits_per_instance * casted_num_ldt_instances;
}

template<typename FieldT>
void LDT_instance_reducer_params<FieldT>::override_security_parameter(const size_t num_ldt_instances)
{
    this->override_security_parameter_ = true;
    this->interactive_soundness_bits_ = 0;
    this->num_output_LDT_instances_ = num_ldt_instances;
}

template<typename FieldT>
size_t LDT_instance_reducer_params<FieldT>::max_tested_degree_bound() const
{
    return this->max_tested_degree_bound_;
}

template<typename FieldT>
bool LDT_instance_reducer_params<FieldT>::make_zk() const
{
    return this->make_zk_;
}

template<typename FieldT>
size_t LDT_instance_reducer_params<FieldT>::absolute_proximity_parameter() const
{
    return this->absolute_proximity_parameter_;
}

template<typename FieldT>
size_t LDT_instance_reducer_params<FieldT>::locality() const
{
    if (this->make_zk_) {
        return 0;
    }
    /** One masking polynomial per instance. */
    return this->num_output_LDT_instances_;
}

template<typename FieldT>
size_t LDT_instance_reducer_params<FieldT>::num_output_LDT_instances() const
{
    return this->num_output_LDT_instances_;
}

const char* LDT_reducer_soundness_type_to_string(LDT_reducer_soundness_type soundness_type)
{
    if (soundness_type == LDT_reducer_soundness_type::proven)
    {
        return "proven";
    } else if (soundness_type == LDT_reducer_soundness_type::optimistic_heuristic)
    {
        return "heuristic";
    }
    return "Invalid soundness type";
}

template<typename FieldT>
void LDT_instance_reducer_params<FieldT>::print() const
{
    printf("\nLDT instance reducer parameters\n");
    if (this->override_security_parameter_)
    {
        print_indent(); printf("===WARNING=== LDT instance reducer security parameter was overridden\n");
    }
    print_indent(); printf("* soundness type = %s\n", LDT_reducer_soundness_type_to_string(this->soundness_type_));
    print_indent(); printf("* target interactive soundness error (bits) = %zu\n", this->interactive_soundness_bits_);
    print_indent(); printf("* achieved interactive soundness error (bits) = %.1Lf\n", this->achieved_soundness());
    print_indent(); printf("* codeword domain dim = %zu\n", this->codeword_domain_dim_);
    print_indent(); printf("* max tested degree bound = %zu\n", this->max_tested_degree_bound_);
    print_indent(); printf("* max constraint degree bound = %zu\n", this->max_constraint_degree_bound_);
    print_indent(); printf("* absolute proximity parameter = %zu\n", this->absolute_proximity_parameter_);
    print_indent(); printf("* fractional proximity parameter = %Lf\n", this->fractional_proximity_parameter_);
    print_indent(); printf("* num output LDT instances = %zu\n", this->num_output_LDT_instances_);
}

template<typename FieldT, typename multi_LDT_type>
LDT_instance_reducer<FieldT, multi_LDT_type>::LDT_instance_reducer(iop_protocol<FieldT> &IOP,
                                                                   const domain_handle &codeword_domain_handle,
                                                                   const LDT_instance_reducer_params<FieldT> &params) :
    IOP_(IOP),
    codeword_domain_handle_(codeword_domain_handle),
    reducer_params_(params)
{
    /* Check that the single LDT type given in the template is valid. */
    typedef std::is_base_of<multi_LDT_base<FieldT>, multi_LDT_type> base_checker;
    assert(base_checker::value);
    UNUSED(base_checker::value);

    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
    std::size_t codeword_domain_size = this->codeword_domain_.num_elements();

    if (this->reducer_params_.make_zk())
    {
        this->blinding_vector_handles_ = register_n_oracles(
            IOP,
            this->reducer_params_.num_output_LDT_instances(),
            this->codeword_domain_handle_,
            this->reducer_params_.max_tested_degree_bound(),
            this->reducer_params_.make_zk());
    }
}

template<typename FieldT, typename multi_LDT_type>
void LDT_instance_reducer<FieldT, multi_LDT_type>::set_LDT_params(std::shared_ptr<multi_LDT_parameter_base<FieldT>> &params)
{
    this->multi_LDT_params_ = params;
}

template<typename FieldT, typename multi_LDT_type>
void LDT_instance_reducer<FieldT, multi_LDT_type>::register_interactions(const std::vector<oracle_handle_ptr> &oracle_handles)
{
    this->input_oracle_handles_ = oracle_handles;
    const size_t num_input_oracles = this->input_oracle_handles_.size();
    this->input_oracle_degrees_.resize(num_input_oracles);
    for (size_t i = 0; i < num_input_oracles; ++i)
    {
        this->input_oracle_degrees_[i] = this->IOP_.get_oracle_degree(this->input_oracle_handles_[i]);
        if (this->input_oracle_degrees_[i] > this->reducer_params_.max_tested_degree_bound())
        {
            printf("%lu\n", i);
            std::stringstream error_message;
            error_message << "One of the oracles is registered with claimed degree ";
            error_message << this->input_oracle_degrees_[i];
            error_message << ", which is greater than the max tested degree bound,";
            error_message << " as claimed in the LDT reducer's parameterization.";
            error_message << " Increase codeword domain dimension if the degree claimed in parameterization is correct.";
            throw std::invalid_argument(error_message.str().c_str());
        }
    }
    /* Add the degree for the blinding polynomial */
    if (this->reducer_params_.make_zk())
    {
        this->input_oracle_degrees_.emplace_back(this->reducer_params_.max_tested_degree_bound());
    }

    /* We want twice as many random coefficients as the number of oracles, because we
       need a coefficient for each oracle, as well as for the bump factors for each oracle of
       submaximal degree. We could sample exactly the number of coefficients needed for
       submaximal oracles, we don't do this for simplicity as it is inexpensive.
       An additional coefficient is added in the zk case, since a blinding polynomial is used. */
    this->num_random_coefficients_ = 2 * num_input_oracles + (this->reducer_params_.make_zk() ? 2 : 0);

    /* Each round of interaction has a single LDT with the same parameters (but they will have
       different random coefficients set later). */
    this->combined_oracle_handles_.resize(this->reducer_params_.num_output_LDT_instances());

    this->combined_oracles_.resize(this->reducer_params_.num_output_LDT_instances());
    this->combined_oracle_handles_.resize(this->reducer_params_.num_output_LDT_instances());
    for (size_t i = 0; i < this->reducer_params_.num_output_LDT_instances(); ++i)
    {
        /** Each combined oracle handle uses every input oracle, and if zk,
         *  they each use a different blinding vector. */
        std::vector<oracle_handle_ptr> constituent_oracle_handles = this->input_oracle_handles_;
        if (this->reducer_params_.make_zk())
        {
            constituent_oracle_handles.emplace_back(this->blinding_vector_handles_[i]);
        }
        this->combined_oracles_[i] = std::make_shared<combined_LDT_virtual_oracle<FieldT> >(
            this->codeword_domain_,
            this->input_oracle_degrees_);
        this->combined_oracle_handles_[i] = this->IOP_.register_virtual_oracle(
            this->codeword_domain_handle_,
            this->reducer_params_.max_tested_degree_bound(),
            constituent_oracle_handles,
            this->combined_oracles_[i]);
    }

    this->random_coefficients_handles_ = register_n_verifier_messages(
        this->IOP_, this->reducer_params_.num_output_LDT_instances(), this->num_random_coefficients_);

    std::vector<oracle_handle_ptr> combined_oracle_handle_ptrs =
        virtual_oracle_handles_to_handle_ptrs(this->combined_oracle_handles_);
    this->multi_LDT_ = std::make_shared<multi_LDT_type>(
        this->IOP_,
        *this->multi_LDT_params_.get(),
        this->codeword_domain_handle_,
        combined_oracle_handle_ptrs);
    this->multi_LDT_->register_interactions();
}

template<typename FieldT, typename multi_LDT_type>
void LDT_instance_reducer<FieldT, multi_LDT_type>::register_queries()
{
    this->multi_LDT_->register_queries();
}

template<typename FieldT, typename multi_LDT_type>
void LDT_instance_reducer<FieldT, multi_LDT_type>::submit_masking_polynomial()
{
    enter_block("LDT Reducer: Submit masking polynomial");
    if (this->reducer_params_.make_zk())
    {
        for (size_t i = 0; i < this->reducer_params_.num_output_LDT_instances(); ++i)
        {
            polynomial<FieldT> random_poly = polynomial<FieldT>::random_polynomial(this->reducer_params_.max_tested_degree_bound());
            std::vector<FieldT> blinding_evaluations =
                FFT_over_field_subset<FieldT>(random_poly.coefficients(), this->codeword_domain_);
            oracle<FieldT> blinding_oracle(blinding_evaluations);
            this->IOP_.submit_oracle(this->blinding_vector_handles_[i], std::move(blinding_oracle));
        }
    }
    leave_block("LDT Reducer: Submit masking polynomial");
}

template<typename FieldT, typename multi_LDT_type>
void LDT_instance_reducer<FieldT, multi_LDT_type>::calculate_and_submit_proof()
{
    enter_block("LDT Reducer: Calculate and submit proof");
    for (size_t i = 0; i < this->reducer_params_.num_output_LDT_instances(); ++i)
    {
        const std::vector<FieldT> challenge = this->IOP_.obtain_verifier_random_message(
            this->random_coefficients_handles_[i]);
        this->combined_oracles_[i]->set_random_coefficients(challenge);
    }

    this->multi_LDT_->calculate_and_submit_proof();
    leave_block("LDT Reducer: Calculate and submit proof");
}

template<typename FieldT, typename multi_LDT_type>
bool LDT_instance_reducer<FieldT, multi_LDT_type>::verifier_predicate()
{
    for (size_t i = 0; i < this->reducer_params_.num_output_LDT_instances(); ++i)
    {
        const std::vector<FieldT> challenge = this->IOP_.obtain_verifier_random_message(
            this->random_coefficients_handles_[i]);
        this->combined_oracles_[i]->set_random_coefficients(challenge);
    }
    const bool decision = this->multi_LDT_->verifier_predicate();

    return decision;
}

} // libiop
