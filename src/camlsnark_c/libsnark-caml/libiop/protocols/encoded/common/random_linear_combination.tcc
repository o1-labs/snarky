#include "libiop/algebra/lagrange.hpp"
#include "libiop/algebra/utils.hpp"

namespace libiop {

template<typename FieldT>
random_linear_combination_oracle<FieldT>::random_linear_combination_oracle(const std::size_t num_oracles) :
    num_oracles_(num_oracles)
{
}

template<typename FieldT>
void random_linear_combination_oracle<FieldT>::set_random_coefficients(const std::vector<FieldT>& random_coefficients)
{
    if (random_coefficients.size() != this->num_oracles_ )
    {
        printf("Got %lu random coefficients\n", random_coefficients.size());
        throw std::invalid_argument("Random Linear Combination Oracle: "
            "Expected same number of random coefficients as oracles.");
    }

    this->random_coefficients_ = random_coefficients;
}

/* Multiplies each oracle evaluation vector by the corresponding random coefficient */
template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> random_linear_combination_oracle<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
{
    if (constituent_oracle_evaluations.size() != this->num_oracles_)
    {
        throw std::invalid_argument("Random Linear Combination Oracle: "
            "Expected same number of evaluations as in registration.");
    }
    const size_t codeword_size = constituent_oracle_evaluations[0]->size();
    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>();
    result->reserve(codeword_size);
    for (std::size_t j = 0; j < codeword_size; ++j) {
        result->emplace_back(this->random_coefficients_[0] *
            constituent_oracle_evaluations[0]->operator[](j));
    }
    for (std::size_t i = 1; i < constituent_oracle_evaluations.size(); ++i)
    {
        if (constituent_oracle_evaluations[i]->size() != codeword_size)
        {
            throw std::invalid_argument("Vectors of mismatched size.");
        }
        for (std::size_t j = 0; j < codeword_size; ++j)
        {
            result->operator[](j) +=
                this->random_coefficients_[i] * constituent_oracle_evaluations[i]->operator[](j);
        }
    }

    return result;
}

/* Takes a random linear combination of the constituent oracle evaluations. */
template<typename FieldT>
FieldT random_linear_combination_oracle<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    libiop::UNUSED(evaluation_position);
    libiop::UNUSED(evaluation_point);

    if (constituent_oracle_evaluations.size() != this->num_oracles_)
    {
        throw std::invalid_argument("Expected same number of evaluations as in registration.");
    }

    FieldT result = FieldT::zero();

    for (std::size_t i = 0; i < constituent_oracle_evaluations.size(); ++i)
    {
        result += this->random_coefficients_[i] * constituent_oracle_evaluations[i];
    }

    return result;
}

} // libiop
