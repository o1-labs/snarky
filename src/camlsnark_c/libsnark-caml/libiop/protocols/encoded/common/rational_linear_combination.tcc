#include "libiop/algebra/utils.hpp"

namespace libiop {

template<typename FieldT>
combined_denominator<FieldT>::combined_denominator(const std::size_t num_rationals) :
    num_rationals_(num_rationals)
{
}

/* Returns the product of all the denominators */
template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> combined_denominator<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
{
    if (constituent_oracle_evaluations.size() != this->num_rationals_)
    {
        throw std::invalid_argument("Expected same number of evaluations as in registration.");
    }

    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>(
        *constituent_oracle_evaluations[0].get());
    for (std::size_t i = 1; i < constituent_oracle_evaluations.size(); ++i)
    {
        if (constituent_oracle_evaluations[i]->size() != result->size())
        {
            throw std::invalid_argument("Vectors of mismatched size.");
        }
        for (std::size_t j = 0; j < result->size(); ++j)
        {
            result->operator[](j) *= constituent_oracle_evaluations[i]->operator[](j);
        }
    }

    return result;
}

/* Takes a random linear combination of the constituent oracle evaluations. */
template<typename FieldT>
FieldT combined_denominator<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    libiop::UNUSED(evaluation_position);
    libiop::UNUSED(evaluation_point);

    if (constituent_oracle_evaluations.size() != this->num_rationals_)
    {
        throw std::invalid_argument("Expected same number of evaluations as in registration.");
    }

    FieldT result = constituent_oracle_evaluations[0];

    for (std::size_t i = 1; i < constituent_oracle_evaluations.size(); ++i)
    {
        result *= constituent_oracle_evaluations[i];
    }

    return result;
}

template<typename FieldT>
combined_numerator<FieldT>::combined_numerator(const std::size_t num_rationals) :
    num_rationals_(num_rationals)
{
}

template<typename FieldT>
void combined_numerator<FieldT>::set_coefficients(const std::vector<FieldT>& coefficients)
{
    if (coefficients.size() != this->num_rationals_)
    {
        throw std::invalid_argument("Expected same number of random coefficients as oracles.");
    }

    this->coefficients_ = coefficients;
}

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> combined_numerator<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const{
    if (constituent_oracle_evaluations.size() != 2*this->num_rationals_)
    {
        throw std::invalid_argument("Expected same number of evaluations as in registration.");
    }

    const size_t codeword_domain_size = constituent_oracle_evaluations[0]->size();
    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>(
        codeword_domain_size, FieldT::zero());
    for (size_t j = 0; j < codeword_domain_size; j++)
    {
        for (size_t i = 0; i < this->num_rationals_; i++)
        {
            FieldT cur = this->coefficients_[i];
            /** Multiply by numerator */
            cur *= constituent_oracle_evaluations[i]->operator[](j);
            /** Multiply by all other denominators */
            for (size_t k = this->num_rationals_; k < 2 * this->num_rationals_; k++)
            {
                if (k - this->num_rationals_ == i)
                {
                    continue;
                }
                cur *= constituent_oracle_evaluations[k]->operator[](j);
            }
            result->operator[](j) += cur;
        }
    }

    return result;
}

template<typename FieldT>
FieldT combined_numerator<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    libiop::UNUSED(evaluation_position);
    libiop::UNUSED(evaluation_point);

    if (constituent_oracle_evaluations.size() != 2*this->num_rationals_)
    {
        throw std::invalid_argument("Expected same number of evaluations as in registration.");
    }

    FieldT result = FieldT::zero();
    for (size_t i = 0; i < this->num_rationals_; i++)
    {
        FieldT cur = this->coefficients_[i];
        /** Multiply by numerator */
        cur *= constituent_oracle_evaluations[i];
        /** Multiply by all other denominators */
        for (size_t j = this->num_rationals_; j < 2 * this->num_rationals_; j++)
        {
            if (j - this->num_rationals_ == i)
            {
                continue;
            }
            cur *= constituent_oracle_evaluations[j];
        }
        result += cur;
    }

    return result;
}

template<typename FieldT>
rational_linear_combination<FieldT>::rational_linear_combination(
    iop_protocol<FieldT> &IOP,
    const std::size_t num_rationals,
    const std::vector<oracle_handle_ptr> numerator_handles,
    const std::vector<oracle_handle_ptr> denominator_handles) :
    IOP_(IOP),
    num_rationals_(num_rationals)
{
    if (numerator_handles.size() != this->num_rationals_ || denominator_handles.size() != this->num_rationals_)
    {
        throw std::invalid_argument("Rational Linear Combination: "
            "#numerator handles passed in != #denominator handles passed in");
    }
    this->numerator_ = std::make_shared<combined_numerator<FieldT>>(this->num_rationals_);
    this->denominator_ = std::make_shared<combined_denominator<FieldT>>(this->num_rationals_);
    const domain_handle domain = this->IOP_.get_oracle_domain(numerator_handles[0]);
    size_t denominator_degree = 1;
    for (size_t i = 0; i < this->num_rationals_; i++)
    {
        denominator_degree += this->IOP_.get_oracle_degree(denominator_handles[i]) - 1;
    }
    this->combined_denominator_handle_ = this->IOP_.register_virtual_oracle(
        domain, denominator_degree, denominator_handles, this->denominator_);
    size_t numerator_degree = 0;
    /** numerator degree = max(deg(N_i) + sum_{j \neq i} D_j)
     *  sum_{j \neq i} D_j = (sum D_i) - D_j
    */
    for (size_t i = 0; i < this->num_rationals_; i++)
    {
        size_t candidate_numerator_degree =
            this->IOP_.get_oracle_degree(numerator_handles[i])
            + denominator_degree - this->IOP_.get_oracle_degree(denominator_handles[i]);
        if (candidate_numerator_degree > numerator_degree)
        {
            numerator_degree = candidate_numerator_degree;
        }
    }
    std::vector<oracle_handle_ptr> all_handles(numerator_handles);
    all_handles.insert(all_handles.end(), denominator_handles.begin(), denominator_handles.end());
    this->combined_numerator_handle_ = this->IOP_.register_virtual_oracle(
        domain, numerator_degree, all_handles, this->numerator_);
}

template<typename FieldT>
void rational_linear_combination<FieldT>::set_coefficients(
    const std::vector<FieldT>& coefficients)
{
    this->numerator_->set_coefficients(coefficients);
}

template<typename FieldT>
std::vector<FieldT> rational_linear_combination<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &numerator_evals,
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &denominator_evals) const
{
    std::vector<FieldT> combined_denominator_evals =
        *this->denominator_->evaluated_contents(denominator_evals).get();
    const bool denominator_can_contain_zeroes = false;
    combined_denominator_evals = batch_inverse<FieldT>(
        combined_denominator_evals, denominator_can_contain_zeroes);
    std::vector<std::shared_ptr<std::vector<FieldT>>> all_evals;
    for (size_t i = 0; i < this->num_rationals_; i++)
    {
        all_evals.emplace_back(numerator_evals[i]);
    }
    for (size_t i = 0; i < this->num_rationals_; i++)
    {
        all_evals.emplace_back(denominator_evals[i]);
    }
    std::vector<FieldT> result = *this->numerator_->evaluated_contents(all_evals).get();
    for (size_t i = 0; i < numerator_evals[0]->size(); i++)
    {
        result[i] *= combined_denominator_evals[i];
    }
    return result;
}

template<typename FieldT>
oracle_handle_ptr rational_linear_combination<FieldT>::get_denominator_handle() const
{
    return std::make_shared<virtual_oracle_handle>(this->combined_denominator_handle_);
}

template<typename FieldT>
oracle_handle_ptr rational_linear_combination<FieldT>::get_numerator_handle() const
{
    return std::make_shared<virtual_oracle_handle>(this->combined_numerator_handle_);
}

} // libiop
