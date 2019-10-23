#include "libiop/algebra/field_subset/subgroup.hpp"
#include "libiop/common/common.cpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

template<typename FieldT>
FRI_protocol_parameters<FieldT>::FRI_protocol_parameters(const size_t interactive_soundness_bits,
                                                         const size_t query_soundness_bits,
                                                         const FRI_soundness_type soundness_type,
                                                         const size_t poly_degree_bound,
                                                         const size_t codeword_domain_dim,
                                                         const size_t RS_extra_dimensions,
                                                         const size_t absolute_proximity_parameter,
                                                         const std::vector<size_t> localization_parameter_array) :
    target_interactive_security_parameter_(interactive_soundness_bits),
    target_query_security_parameter_(query_soundness_bits),
    soundness_type_(soundness_type),
    poly_degree_bound_(poly_degree_bound),
    codeword_domain_dim_(codeword_domain_dim),
    RS_extra_dimensions_(RS_extra_dimensions),
    field_type_(libiop::get_field_type<FieldT>(FieldT::zero())),
    localization_parameters_(localization_parameter_array)
{
    size_t total_localizations = 0;
    for (size_t i = 0; i < localization_parameter_array.size(); i++) {
        total_localizations += localization_parameter_array[i];
    }
    /** The soundness analysis of FRI does not depend on the tested degree bound being a power of two.
     *  However if we wish to use a non-power-of-two degree bound, then the degree bound we test for
     *  must be a multiple of 2^{sum of localization parameters}.
     *  FRI is sound for other degrees, but not complete,
     *  as the final polynomial will have an insufficient number of coefficients. */
    if (poly_degree_bound % (1ull << total_localizations) != 0)
    {
        throw std::invalid_argument("FRI only supports testing degree bounds that are a multiple of "
                                    "2^{sum of localization parameters}."
                                    "Consider rounding up the tested degree, using "
                                    "FRI_protocol_parameters<FieldT>::next_testable_degree_bound");
    }
    /** rate = 2^{-RS_extra_dimensions} */
    const long double rate = exp2l(-1.0 * this->RS_extra_dimensions_);
    const long double codeword_domain_size = exp2l(this->codeword_domain_dim_);
    const long double fractional_proximity_parameter = absolute_proximity_parameter / codeword_domain_size;
    const long double field_size_bits = (long double)(soundness_log_of_field_size_helper<FieldT>(FieldT::zero()));
    // TODO: What should this be?
    const long double localization_coset_size = exp2l(1.0 * this->localization_parameters_[0]);
    if (this->soundness_type_ == FRI_soundness_type::proven)
    {
        /** We use the soundness error proven in the original FRI paper.
         *  The query soundness error is at least (1 - min(delta, term2))^{query repetitions},
         *  Where term2 is shorthand for:
         *      term2 = (1 - 3*rate - (2^{localization parameter} / sqrt(codeword domain size))) / 4
         *  Consequently the bits of security is:
         *      -query_soundness_bits = log(query_soundness) = query_repetitions * log(1 - min(delta, term2))
         *  Rearranging, we get:
         *      query_repetitions = ceil(-query_soundness_bits / log(1 - min(delta, term2)))
        */
        const long double sqrt_codeword_domain_size = sqrt(codeword_domain_size);
        const long double term2 = (1 - 3*rate - (localization_coset_size / sqrt_codeword_domain_size)) / 4;
        /** min(fractional_proximity_parameter, term2) */
        this->effective_proximity_parameter_ = std::min<long double>(fractional_proximity_parameter, term2);
        long double denominator = log2l(1 - this->effective_proximity_parameter_);
        const long double query_repetitions = ceil(-1.0 * query_soundness_bits / denominator);
        this->num_query_repetitions_ = std::max<size_t>(1, size_t(query_repetitions));

        /** Interactive soundness error proven in the original FRI paper is:
         *      (3 * codeword_domain_size / F)^(interactive_repetitions)
         *  Consequently the bits of security is:
         *      -Interactive_soundness_bits = (interactive_repetitions) * log(3 * codeword_domain_size / |F|)
         *  Rearranging for interactive repetitions, we get:
         *      interactive_repetitions = ceil(-Interactive_soundness_bits / log(3 * codeword_domain_size / |F|))
         *  We compute the log using the identity:
         *      log(3 * codeword_domain_size / |F|) = log(3) + codeword_domain_dimension - log(|F|)
         */
        this->soundness_per_interaction_ = (log2l(3) + (long double)(this->codeword_domain_dim_) - field_size_bits);
        const long double interactive_repetitions = ceil(-1.0 * interactive_soundness_bits / this->soundness_per_interaction_);
        this->num_interactive_repetitions_ = std::max<size_t>(1, size_t(interactive_repetitions));
    }
    else if (this->soundness_type_ == FRI_soundness_type::heuristic)
    {
        /** Heuristic Query soundness given by the FRI paper is:
         *      ((1 - fractional proximity parameter))^{num queries}
         *  Consequently the bits of security is:
         *      -query_soundness_bits = log(query_soundness) = query_repetitions * log(1 - delta)
         *  Rearranging for num_queries, we get:
         *      query_repetitions = ceil(-query_soundness_bits / log(1 - delta))
        */
        this->effective_proximity_parameter_ = fractional_proximity_parameter;
        long double denominator = log2l(1 - this->effective_proximity_parameter_);
        const long double query_repetitions = ceil(-1.0 * query_soundness_bits / denominator);
        this->num_query_repetitions_ = std::max<size_t>(1, size_t(query_repetitions));
        /** Heuristic Interactive soundness given by the FRI paper is:
         *      ((2^{localization parameter} - 1) / |F|)^{interactive repetitions}
         *  Consequently the bits of security is:
         *      -Interactive_soundness_bits = (interactive_repetitions) * log(2^{localization parameter} - 1 / |F|)
         *  Rearranging for interactive repetitions, we get:
         *      interactive repetitions = ceil(-Interactive_soundness_bits / log(2^{localization parameter} - 1 / |F|))
         *  We compute the log as:
         *      log(2^{localization parameter} - 1) - log(|F|)
         */
        const long double localization_coset_minus_one = localization_coset_size - 1.0;
        const long double log_localization_coset = log2l(localization_coset_minus_one);
        this->soundness_per_interaction_ = log_localization_coset - field_size_bits;
        const long double interactive_repetitions = ceil(-1.0 * interactive_soundness_bits / this->soundness_per_interaction_);
        this->num_interactive_repetitions_ = std::max<size_t>(1, size_t(interactive_repetitions));
    }
}

template<typename FieldT>
FRI_protocol_parameters<FieldT>::FRI_protocol_parameters(const size_t interactive_soundness_bits,
                                                         const size_t query_soundness_bits,
                                                         const FRI_soundness_type soundness_type,
                                                         const size_t poly_degree_bound,
                                                         const size_t codeword_domain_dim,
                                                         const size_t RS_extra_dimensions,
                                                         const size_t absolute_proximity_parameter,
                                                         const size_t localization_parameter) :
    FRI_protocol_parameters(interactive_soundness_bits,
                            query_soundness_bits,
                            soundness_type,
                            poly_degree_bound,
                            codeword_domain_dim,
                            RS_extra_dimensions,
                            absolute_proximity_parameter,
                            FRI_protocol_parameters::localization_parameter_to_array(
                                localization_parameter,
                                log2l(poly_degree_bound) + RS_extra_dimensions, /* codeword domain dim */
                                RS_extra_dimensions)
                            ) { }

template<typename FieldT>
std::vector<size_t> FRI_protocol_parameters<FieldT>::localization_parameter_to_array(
    const size_t localization_parameter,
    const size_t codeword_domain_dim,
    const size_t RS_extra_dimensions)
{
    /** Number of "collapsings" needed to reduce to constant-sized final evauation domain.
     *  The constant size domain we collapse to is of dimension RS_extra_dimensions */
    const size_t num_reductions = ((codeword_domain_dim - RS_extra_dimensions - 1) /
                                    localization_parameter) + 1;

    std::vector<size_t> localization_parameters(num_reductions - 1, localization_parameter);
    localization_parameters.insert(localization_parameters.begin(), 1);
    return localization_parameters;
}

template<typename FieldT>
size_t FRI_protocol_parameters<FieldT>::next_testable_degree_bound(
    const size_t tested_degree_bound,
    const std::vector<size_t> localization_parameter_array)
{
    size_t total_localizations = 0;
    for (size_t i = 0; i < localization_parameter_array.size(); i++) {
        total_localizations += localization_parameter_array[i];
    }
    const size_t remainder = tested_degree_bound % (1ull << total_localizations);
    if (remainder == 0) {
        return tested_degree_bound;
    }
    /** Go to next multiple of (1ull << total_localizations) */
    return tested_degree_bound - remainder + (1ull << total_localizations);
}

template<typename FieldT>
field_subset<FieldT> FRI_protocol_parameters<FieldT>::quotient_map_domain(field_subset<FieldT> codeword_domain) const
{
    const size_t coset_size = 1ull << this->localization_parameters_[0];
    return codeword_domain.get_subset_of_order(coset_size);
}

template<typename FieldT>
size_t FRI_protocol_parameters<FieldT>::queries_to_input_oracles() const
{
    const size_t queries_per_queryset = 1ull << this->localization_parameters_[0];
    return this->num_query_repetitions_ * queries_per_queryset;
}

template<typename FieldT>
void FRI_protocol_parameters<FieldT>::override_security_parameters(
    const size_t interactive_repetitions, const size_t query_repetitions)
{
    this->override_security_parameter_ = true;
    this->target_interactive_security_parameter_ = 0;
    this->target_query_security_parameter_ = 0;
    this->num_interactive_repetitions_ = interactive_repetitions;
    this->num_query_repetitions_ = query_repetitions;
}

template<typename FieldT>
size_t FRI_protocol_parameters<FieldT>::RS_extra_dimensions() const
{
    return this->RS_extra_dimensions_;
}
template<typename FieldT>
size_t FRI_protocol_parameters<FieldT>::poly_degree_bound() const
{
    return this->poly_degree_bound_;
}
template<typename FieldT>
field_type FRI_protocol_parameters<FieldT>::get_field_type() const
{
    return this->field_type_;
}
template<typename FieldT>
size_t FRI_protocol_parameters<FieldT>::query_repetitions() const
{
    return this->num_query_repetitions_;
}
template<typename FieldT>
size_t FRI_protocol_parameters<FieldT>::interactive_repetitions() const
{
    return this->num_interactive_repetitions_;
}
template<typename FieldT>
std::vector<size_t> FRI_protocol_parameters<FieldT>::get_localization_parameters() const
{
    return this->localization_parameters_;
}

template<typename FieldT>
long double FRI_protocol_parameters<FieldT>::achieved_interactive_soundness() const
{
    return -1.0 * (long double)(this->num_interactive_repetitions_) * this->soundness_per_interaction_;
}

template<typename FieldT>
long double FRI_protocol_parameters<FieldT>::achieved_query_soundness() const
{
    long double soundness_per_query = log2l(1 - this->effective_proximity_parameter_);
    return -1.0 * (long double)(this->num_query_repetitions_) * soundness_per_query;
}

const char* FRI_soundness_type_to_string(FRI_soundness_type soundness_type)
{
    if (soundness_type == FRI_soundness_type::heuristic)
    {
        return "heuristic";
    } else if (soundness_type == FRI_soundness_type::proven)
    {
        return "proven";
    }
    return "Invalid soundness type";
}

template<typename FieldT>
void FRI_protocol_parameters<FieldT>::print() const
{
    printf("\nFRI parameters\n");
    if (this->override_security_parameter_)
    {
        print_indent(); printf("===WARNING=== FRI security parameter was overridden\n");
    }
    print_indent(); printf("* soundness type = %s\n", FRI_soundness_type_to_string(this->soundness_type_));
    print_indent(); printf("* target interactive soundness error (bits) = %zu\n", this->target_interactive_security_parameter_);
    print_indent(); printf("* achieved interactive soundness error (bits) = %.1Lf\n", this->achieved_interactive_soundness());
    print_indent(); printf("* target query soundness error (bits) = %zu\n", this->target_query_security_parameter_);
    print_indent(); printf("* achieved query soundness error (bits) = %.1Lf\n", this->achieved_query_soundness());
    print_indent(); printf("* codeword domain dim = %zu\n", this->codeword_domain_dim_);
    print_indent(); printf("* effective proximity parameter = %Lf\n", this->effective_proximity_parameter_);
    print_indent(); printf("* number of interactive repetitions = %zu\n", this->num_interactive_repetitions_);
    print_indent(); printf("* number of query repetitions = %zu\n", this->num_query_repetitions_);
    print_indent(); printf("* localization parameter array = ");
        libiop::print_vector<size_t>(this->localization_parameters_);
    print_indent(); printf("* number of reductions = %zu\n", this->localization_parameters_.size());
}

template<typename FieldT>
FRI_protocol<FieldT>::FRI_protocol(iop_protocol<FieldT> &IOP,
                                   multi_LDT_parameter_base<FieldT> &params,
                                   const domain_handle &codeword_domain_handle,
                                   const std::vector<oracle_handle_ptr> &poly_handles) :
    multi_LDT_base<FieldT>(IOP, params, codeword_domain_handle, poly_handles)
{
    FRI_protocol_parameters<FieldT> *LDT_params = dynamic_cast<FRI_protocol_parameters<FieldT>*>(&params);
    if (LDT_params == 0)
    {
        throw std::invalid_argument("parameters passed to FRI are not of type FRI_protocol_parameters");
    }
    this->params_ = *LDT_params;
    const field_subset<FieldT> codeword_domain =
        this->IOP_.get_domain(this->codeword_domain_handle_);
    const std::size_t codeword_domain_dim = codeword_domain.dimension();

    this->poly_degree_bound_ = this->params_.poly_degree_bound();
    this->num_reductions_ = this->params_.get_localization_parameters().size();

    this->compute_domains();
}

template<typename FieldT>
void FRI_protocol<FieldT>::compute_domains()
{
    /* Compute the domains and localization polynomials used in the protocol */

    this->localizer_polynomials_.reserve(this->num_reductions_);
    this->localizer_domains_.reserve(this->num_reductions_);
    this->domains_.reserve(this->num_reductions_ + 1);

    const field_subset<FieldT> codeword_domain =
        this->IOP_.get_domain(this->codeword_domain_handle_);
    this->domains_.emplace_back(codeword_domain);

    if (this->params_.get_field_type() == multiplicative_field_type)
    {
        std::size_t size = 1ull << codeword_domain.dimension();
        FieldT shift = codeword_domain.shift();
        for (std::size_t i = 0; i < this->num_reductions_; ++i)
        {
            std::size_t current_localization_parameter = this->params_.get_localization_parameters()[i];
            std::size_t order = 1ull << current_localization_parameter;
            field_subset<FieldT> localizer_subgroup(order);
            localizer_polynomial<FieldT> localizer_poly(localizer_subgroup);

            shift = localizer_poly.evaluation_at_point(shift);
            size >>= current_localization_parameter;
            this->localizer_domains_.emplace_back(localizer_subgroup);
            this->localizer_polynomials_.emplace_back(localizer_poly);
            this->domains_.emplace_back(field_subset<FieldT>(size, shift));
        }
    }
    else if (this->params_.get_field_type() == additive_field_type)
    {
        for (std::size_t i = 0; i < this->num_reductions_; ++i)
        {
            const std::size_t current_localization_parameter = this->params_.get_localization_parameters()[i];

            const FieldT last_subspace_offset = this->domains_[i].offset();
            const std::vector<FieldT>& last_subspace_basis = this->domains_[i].basis();

            const std::vector<FieldT> localizer_subspace_basis(last_subspace_basis.begin(),
                                                               last_subspace_basis.begin() + current_localization_parameter);
            const affine_subspace<FieldT> localizer_subspace(localizer_subspace_basis, FieldT::zero());
            const localizer_polynomial<FieldT> localizer_poly(localizer_subspace);

            const FieldT next_subspace_offset = localizer_poly.evaluation_at_point(last_subspace_offset);
            std::vector<FieldT> next_subspace_basis(last_subspace_basis.begin() + current_localization_parameter,
                                                    last_subspace_basis.end());
            for (size_t i = 0; i < next_subspace_basis.size(); i++)
            {
                FieldT el = next_subspace_basis[i];
                next_subspace_basis[i] = localizer_poly.evaluation_at_point(el);
            }

            const affine_subspace<FieldT> next_subspace(next_subspace_basis, next_subspace_offset);

            this->domains_.emplace_back(field_subset<FieldT>(next_subspace));
            this->localizer_domains_.emplace_back(field_subset<FieldT>(localizer_subspace));
            this->localizer_polynomials_.emplace_back(localizer_poly);
        }
    }
}

template<typename FieldT>
void FRI_protocol<FieldT>::register_interactions()
{
    const std::size_t num_interactions = this->params_.interactive_repetitions();
    std::size_t total_localizations = this->params_.get_localization_parameters()[0];

    /* Register oracles and domains for each reduction and interaction. */
    this->domain_handles_.resize(this->num_reductions_);
    this->oracle_handles_.resize(this->num_reductions_);
    this->verifier_challenge_handles_.resize(this->num_reductions_);

    this->domain_handles_[0] = this->codeword_domain_handle_;
    this->oracle_handles_[0] = {this->poly_handles_};

    std::vector<verifier_random_message_handle> x_0 = register_n_verifier_messages(
            this->IOP_, num_interactions, 1);
    this->verifier_challenge_handles_[0] = x_0;

    for (std::size_t i = 1; i < this->num_reductions_; ++i)
    {
        std::size_t current_localization_parameter = this->params_.get_localization_parameters()[i];
        total_localizations += current_localization_parameter;

        const std::size_t degree_bound = this->poly_degree_bound_ >> total_localizations;
        const domain_handle L_i = this->IOP_.register_domain(this->domains_[i]);

        /* indexed by interaction, then LDT index */
        std::vector<std::vector<oracle_handle_ptr> > multi_f_i_for_all_interactions;
        for (size_t j = 0; j < num_interactions; j++)
        {
            const std::vector<oracle_handle_ptr> multi_f_i =
                register_n_oracles(this->IOP_, this->poly_handles_.size(), L_i, degree_bound, false);
            multi_f_i_for_all_interactions.emplace_back(multi_f_i);
        }

        /* Set localization domain as the round parameters, for BCS space improvements */
        const field_subset<FieldT> quotient_map_domain =
            this->domains_[i].get_subset_of_order(1ull << current_localization_parameter);
        const round_parameters<FieldT> round_params(quotient_map_domain);
        this->IOP_.set_round_parameters(round_params);

        const std::vector<verifier_random_message_handle> x_i_for_all_interactions =
            register_n_verifier_messages(this->IOP_, num_interactions, 1);

        this->domain_handles_[i] = L_i;
        this->oracle_handles_[i] = multi_f_i_for_all_interactions;
        this->verifier_challenge_handles_[i] = x_i_for_all_interactions;
    }

    this->final_polynomial_degree_bound_ = this->poly_degree_bound_ >> total_localizations;
    this->final_polynomial_handles_.resize(num_interactions);
    for (size_t j = 0; j < num_interactions; j++)
    {
        this->final_polynomial_handles_[j] = register_n_prover_messages(
            this->IOP_, this->poly_handles_.size(), this->final_polynomial_degree_bound_);
    }
}

template<typename FieldT>
void FRI_protocol<FieldT>::register_queries()
{
    /** We choose num_query_repetitions initial random query positions.
     *  We then query all the oracles in each reduction at the points determined
     *  by the initial random query position,
     *  and the localization parameter for that reduction.
     *
     *  After the first round,
     *  there are num interactive repetition oracles per reduction.
     *
     *  We call a query set a set of queries the verifier checks.
     *  So we make a different query set for each interaction,
     *  even though the query positions are re-used.
     * */

    this->query_sets_.reserve(
        this->params_.query_repetitions() * this->params_.interactive_repetitions()
        * this->poly_handles_.size());

    for (size_t q = 0; q < this->params_.query_repetitions(); q++)
    {
        /* query at first round is randomly chosen */
        const random_query_position_handle s0_position_handle =
            this->IOP_.register_random_query_position(this->domain_handles_[0]);

        /* outer index is round; inner index is each of the 2^eta queries */
        std::vector<std::vector<query_position_handle> > s_coset_position_handles(this->num_reductions_);

        s_coset_position_handles[0] = query_position_to_queries_for_entire_coset(
            this->IOP_, s0_position_handle, this->domains_[0],
            1ull << this->params_.get_localization_parameters()[0]);

        for (std::size_t i = 1; i < this->num_reductions_; ++i)
        {
            /* next queries are "roots" of previous query; i.e. coset of
               localizer polynomial containing vanishing polynomial evaluated
               at previous query */
            s_coset_position_handles[i] = calculate_next_coset_query_positions(
                this->IOP_, s_coset_position_handles[i - 1][0],
                this->domains_[i - 1], this->domains_[i],
                this->params_.get_localization_parameters()[i - 1],
                this->params_.get_localization_parameters()[i]);
        }
        /** Now we've finished selecting query positions.
         *  We then have to create a query set for the correct oracles for each interaction / LDT pair. */
        for (size_t interaction_index = 0;
                interaction_index < this->params_.interactive_repetitions(); interaction_index++)
        {
            for (size_t ldt_index = 0; ldt_index < this->poly_handles_.size(); ldt_index++)
            {
                FRI_query_set query_set;
                query_set.s0_position_handle_ = s0_position_handle;
                query_set.interaction_index_ = interaction_index;
                query_set.LDT_index_ = ldt_index;
                query_set.f_at_s_coset_query_handles_.resize(this->num_reductions_);
                for (size_t i = 0; i < this->num_reductions_; i++)
                {
                    const std::size_t current_coset_size = 1ull << this->params_.get_localization_parameters()[i];
                    query_set.f_at_s_coset_query_handles_[i].resize(current_coset_size);
                    /* first reduction always queries the same oracle, independent of interaction */
                    const size_t queried_interaction_index = (i == 0) ? 0 : interaction_index;
                    for (size_t j = 0; j < current_coset_size; j++)
                    {
                        query_set.f_at_s_coset_query_handles_[i][j] = this->IOP_.register_query(
                            this->oracle_handles_[i][queried_interaction_index][ldt_index],
                            s_coset_position_handles[i][j]);
                    }
                }
                this->query_sets_.emplace_back(query_set);
            }
        }
    }
}

template<typename FieldT>
void FRI_protocol<FieldT>::calculate_and_submit_proof()
{
    /* First set of codewords: the original purported codewords we're testing. */
    std::vector<std::shared_ptr<std::vector<FieldT>>> multi_f_i_evaluations =
        get_all_oracle_evaluations(this->IOP_, this->poly_handles_);

    /* indexed by interaction, then LDT instance index */
    std::vector<std::vector<std::shared_ptr<std::vector<FieldT>>>>
        multi_f_i_evaluations_by_interaction;
    for (size_t j = 0; j < this->params_.interactive_repetitions(); j++)
    {
        multi_f_i_evaluations_by_interaction.emplace_back(multi_f_i_evaluations);
    }

    for (std::size_t i = 0; i < this->num_reductions_; ++i)
    {
        std::size_t current_localization_parameter = this->params_.get_localization_parameters()[i];
        const size_t coset_size = 1ull << current_localization_parameter;

        /** Send over the oracle produced for each interaction.
         *  When i is 0, it has already been submitted. */
        if (i > 0)
        {
            for (size_t j = 0; j < this->params_.interactive_repetitions(); j++)
            {
                for (size_t ldt_index = 0; ldt_index < this->poly_handles_.size(); ldt_index++)
                {
                    oracle<FieldT> cur_f_i_oracle(multi_f_i_evaluations_by_interaction[j][ldt_index]);
                    this->IOP_.submit_oracle(this->oracle_handles_[i][j][ldt_index], std::move(cur_f_i_oracle));
                }
            }

            enter_block("LDT signal prover round done");
            this->IOP_.signal_prover_round_done();
            leave_block("LDT signal prover round done");
        }

        /* For each interaction, receive the verifier challenge and create f_{i + 1} */
        for (size_t j = 0; j < this->params_.interactive_repetitions(); j++)
        {
            const FieldT x_i = this->IOP_.obtain_verifier_random_message(
                this->verifier_challenge_handles_[i][j])[0];

            enter_block("evaluating next FRI codeword");
            for (size_t ldt_index = 0; ldt_index < this->poly_handles_.size(); ldt_index++)
            {
                multi_f_i_evaluations_by_interaction[j][ldt_index] = evaluate_next_f_i_over_entire_domain(
                    multi_f_i_evaluations_by_interaction[j][ldt_index],
                    this->domains_[i],
                    coset_size,
                    x_i);
            }
            leave_block("evaluating next FRI codeword");
        }
    }

    /* Finally, recover the coefficients of final polynomial using
       polynomial interpolation and directly send them. */
    for (size_t j = 0; j < this->params_.interactive_repetitions(); j++)
    {
        for (size_t ldt_index = 0; ldt_index < this->poly_handles_.size(); ldt_index++)
        {
            std::vector<FieldT> final_poly_coeffs = IFFT_over_field_subset<FieldT>(
                *multi_f_i_evaluations_by_interaction[j][ldt_index].get(), this->domains_[this->num_reductions_]);
            final_poly_coeffs.resize(this->final_polynomial_degree_bound_);
            this->IOP_.submit_prover_message(this->final_polynomial_handles_[j][ldt_index], std::move(final_poly_coeffs));
        }
    }

    enter_block("LDT signal prover round done");
    this->IOP_.signal_prover_round_done();
    leave_block("LDT signal prover round done");
}

template<typename FieldT>
bool FRI_protocol<FieldT>::verifier_predicate()
{
    this->num_queries_made_ = 0;

    bool decision = true;
    for (auto &Q : this->query_sets_)
    {
        if (!this->predicate_for_query_set(Q))
        {
            decision = false;
        }
    }

    print_indent(); printf("* Number of FRI interactions: %zu\n", this->params_.interactive_repetitions());
    print_indent(); printf("* Number of FRI query sets per interaction: %zu\n",
        this->query_sets_.size() / this->params_.interactive_repetitions());
    print_indent(); printf("* Total number of FRI queries over all query sets (incl. repeated queries): %zu\n", this->num_queries_made_);
    //print_indent(); printf("* Analytical expression: %zu\n", (2 + ((this->num_reductions_ - 1) << this->localization_parameter_)) * this->query_sets_.size());

    return decision;
}

template<typename FieldT>
bool FRI_protocol<FieldT>::predicate_for_query_set(const FRI_query_set &Q)
{
    /* Verifier work */
    const std::size_t s0_idx = this->IOP_.obtain_query_position(Q.s0_position_handle_);
    const FieldT s0 = this->domains_[0].element_by_index(s0_idx);

    FieldT si = s0;
    size_t si_idx = s0_idx;
    FieldT last_interpolation;

    bool decision = true;

    for (std::size_t i = 0; i < this->num_reductions_; ++i)
    {
        const FieldT x_i = this->IOP_.obtain_verifier_random_message(
            this->verifier_challenge_handles_[i][Q.interaction_index_])[0];

        const size_t current_coset_size = 1ull << this->params_.get_localization_parameters()[i];
        size_t si_j = this->domains_[i].coset_index(si_idx, current_coset_size);
        size_t si_k = this->domains_[i].intra_coset_index(si_idx, current_coset_size);
        si_idx = si_j; /* next si position is the same as the coset index for si */

        /* Query the entire si coset */
        std::vector<FieldT> fi_on_si_coset;
        for (std::size_t k = 0; k < current_coset_size; ++k)
        {
            fi_on_si_coset.emplace_back(
                this->IOP_.obtain_query_response(Q.f_at_s_coset_query_handles_[i][k]));
            ++(this->num_queries_made_);
        }

        /* Check that the coset matches interpolation computed in the
           previous round. */
        if (i > 0)
        {
            if (last_interpolation != fi_on_si_coset[si_k])
            {
                return false;
            }
        }

        /* Now compute interpolant of f_i|S_i evaluated at x_i
           (for use in next round). */
        const size_t offset_position =
            this->domains_[i].position_by_coset_indices(si_j, 0, current_coset_size);
        const FieldT offset = this->domains_[i].element_by_index(offset_position);
        FieldT interpolation = evaluate_next_f_i_at_coset(
            fi_on_si_coset,
            this->localizer_domains_[i],
            offset,
            this->localizer_polynomials_[i],
            x_i);
        last_interpolation = interpolation;
        si = this->localizer_polynomials_[i].evaluation_at_point(si);
    }

    /* Finally, check that the LAST round polynomial, evaluated at si,
       matches the final interpolation from the loop. */
    const polynomial<FieldT> last_poly(
        this->IOP_.receive_prover_message(this->final_polynomial_handles_[Q.interaction_index_][Q.LDT_index_]));
    const FieldT last_poly_at_si = last_poly.evaluation_at_point(si);

    if (last_poly_at_si != last_interpolation)
    {
        decision = false;
    }

    return decision;
}

} // namespace libiop
