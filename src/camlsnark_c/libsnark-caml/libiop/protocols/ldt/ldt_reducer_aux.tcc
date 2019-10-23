namespace libiop {

template<typename FieldT>
combined_LDT_virtual_oracle<FieldT>::combined_LDT_virtual_oracle(
    const field_subset<FieldT> &codeword_domain,
    const std::vector<std::size_t>& input_oracle_degrees) :
    codeword_domain_(codeword_domain),
    input_oracle_degrees_(input_oracle_degrees)
{
    this->num_input_oracles_ = this->input_oracle_degrees_.size();
    this->num_random_coefficients_ = 2 * this->num_input_oracles_;
    this->max_degree_ = *std::max_element(this->input_oracle_degrees_.begin(), this->input_oracle_degrees_.end());
    for (std::size_t i = 0; i < this->num_input_oracles_; ++i)
    {
        if (this->input_oracle_degrees_[i] < this->max_degree_)
        {
            this->submaximal_oracle_indices_.emplace_back(i);
        }
        else
        {
            this->maximal_oracle_indices_.emplace_back(i);
        }
    }
}

template<typename FieldT>
void combined_LDT_virtual_oracle<FieldT>::set_random_coefficients(
    const std::vector<FieldT>& random_coefficients)
{
    if (random_coefficients.size() != this->num_random_coefficients_)
    {
        throw std::invalid_argument("Expected the nunmber of random coefficients to be twice the number of oracles.");
    }

    this->coefficients_ = { FieldT::one() };
    this->coefficients_.insert(this->coefficients_.end(), random_coefficients.begin(), random_coefficients.end());
}

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> combined_LDT_virtual_oracle<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
{
    if (constituent_oracle_evaluations.size() != this->num_input_oracles_)
    {
        throw std::invalid_argument("Expected same number of evaluations as in registration.");
    }

    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>
        (constituent_oracle_evaluations[0]->size(), FieldT::zero());

    /* Handle maximal degree oracles */
    for (size_t i = 0; i < this->maximal_oracle_indices_.size(); i++)
    {
        const size_t index = this->maximal_oracle_indices_[i];
        if (constituent_oracle_evaluations[index]->size() != result->size())
        {
            throw std::invalid_argument("Vectors of mismatched size.");
        }

        for (std::size_t j = 0; j < result->size(); ++j)
        {
            result->operator[](j) += this->coefficients_[index] *
                constituent_oracle_evaluations[index]->operator[](j);
        }
    }

    /** Now deal with submaximal oracles.
     *  We use the identity that:
     *  r_1 * f + r_2 * x^shift * f = (r_1 + r_2 * x^shift) * f
     *
     *  In the multiplicative case, we can compute r_2 * x^shift directly
     *  with no additional multiplications.
     *  In the additive case, we re-use shift coefficients whenever possible instead.
     *  TODO: Implement re-using coefficients,
     *        multiplicative case benchmark re-using coefficients vs combined method,
     *        re-using may be faster after some threshold due to data-dependency concerns
     * */
    if (this->codeword_domain_.type() == affine_subspace_type)
    {
        for (std::size_t i = 0; i < this->submaximal_oracle_indices_.size(); ++i)
        {
            /* Which oracle are we dealing with now? */
            const std::size_t submaximal_oracle_index =
                this->submaximal_oracle_indices_[i];

            /* Raise each element in the codeword domain to the power of the degree difference. */
            const std::vector<FieldT> bump_factors =
                subset_element_powers(
                    this->codeword_domain_,
                    this->max_degree_ - this->input_oracle_degrees_[submaximal_oracle_index]);

            for (std::size_t j = 0; j < result->size(); ++j)
            {
                result->operator[](j) += (
                    this->coefficients_[submaximal_oracle_index] +
                    this->coefficients_[this->num_input_oracles_ + i] *
                    bump_factors[j]) *
                    constituent_oracle_evaluations[submaximal_oracle_index]->operator[](j);
            }
        }
    }
    else if (this->codeword_domain_.type() == multiplicative_coset_type)
    {
        for (std::size_t i = 0; i < this->submaximal_oracle_indices_.size(); ++i)
        {
            /* Which oracle are we dealing with now? */
            const std::size_t submaximal_oracle_index =
                this->submaximal_oracle_indices_[i];

            /* cur_bump_factor = r_{shifted index} * elem^shift */
            const size_t shift = this->max_degree_ - this->input_oracle_degrees_[submaximal_oracle_index];
            FieldT cur_bump_factor = this->coefficients_[this->num_input_oracles_ + i] *
                libiop::power(this->codeword_domain_.shift(), shift);
            const FieldT bump_factor_inc =
                libiop::power(this->codeword_domain_.generator(), shift);

            for (std::size_t j = 0; j < result->size(); ++j)
            {
                result->operator[](j) += (
                    this->coefficients_[submaximal_oracle_index] +
                    cur_bump_factor) *
                    constituent_oracle_evaluations[submaximal_oracle_index]->operator[](j);
                cur_bump_factor *= bump_factor_inc;
            }
        }
    }

    return result;
}

template<typename FieldT>
FieldT combined_LDT_virtual_oracle<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    UNUSED(evaluation_position);

    if (constituent_oracle_evaluations.size() != this->input_oracle_degrees_.size())
    {
        throw std::invalid_argument("Expected same number of evaluations as in registration.");
    }

    FieldT result = FieldT::zero();

    for (std::size_t i = 0; i < constituent_oracle_evaluations.size(); ++i)
    {
        result += this->coefficients_[i] * constituent_oracle_evaluations[i];
    }

    /* Now deal with submaximal oracles */
    for (std::size_t i = 0; i < this->submaximal_oracle_indices_.size(); ++i)
    {
        /* Which oracle are we dealing with now? */
        const std::size_t submaximal_oracle_index =
            this->submaximal_oracle_indices_[i];

        /* Raise each element in the codeword domain to the power of the degree difference. */
        const FieldT bump_factor =
            libiop::power(evaluation_point,
                            this->max_degree_ - this->input_oracle_degrees_[submaximal_oracle_index]);

        result += this->coefficients_[this->num_input_oracles_ + i] *
            bump_factor *
            constituent_oracle_evaluations[submaximal_oracle_index];
    }

    return result;
}

} // namespace libiop
