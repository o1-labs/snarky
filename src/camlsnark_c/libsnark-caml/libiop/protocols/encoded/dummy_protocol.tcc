#include <stdexcept>

#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

template<typename FieldT>
dummy_oracle<FieldT>::dummy_oracle(const std::size_t num_oracles) :
    num_oracles_(num_oracles)
{
}

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> dummy_oracle<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>>> &constituent_oracle_evaluations) const
{
    if (constituent_oracle_evaluations.size() != this->num_oracles_)
    {
        throw std::invalid_argument("Expected same number of evaluations as in registration.");
    }

    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>();
    result->reserve(constituent_oracle_evaluations[0]->size());
    for (size_t i = 0; i < result->size(); ++i)
    {
        result->emplace_back(FieldT::zero());
    }

    return result;
}

template<typename FieldT>
FieldT dummy_oracle<FieldT>::evaluation_at_point(
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

    return FieldT::zero();
}

template<typename FieldT>
dummy_protocol<FieldT>::dummy_protocol(
    iop_protocol<FieldT> &IOP,
    const std::size_t num_oracles,
    const std::size_t RS_extra_dimensions,
    const domain_handle codeword_domain_handle,
    const bool make_zk) :
    IOP_(IOP),
    num_oracles_(num_oracles),
    RS_extra_dimensions_(RS_extra_dimensions),
    codeword_domain_handle_(codeword_domain_handle),
    make_zk_(make_zk)
{
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);
    this->codeword_domain_size_ = this->codeword_domain_.num_elements();
    this->codeword_domain_dim_ = this->codeword_domain_.dimension();

    this->degree_ = 1ull << (this->codeword_domain_dim_ - this->RS_extra_dimensions_);

    this->constituent_oracles_.reserve(this->num_oracles_);
    this->constituent_oracle_ptrs_.reserve(this->num_oracles_);
    for (size_t i = 0; i < this->num_oracles_; ++i)
    {
        this->constituent_oracles_.emplace_back(
            this->IOP_.register_oracle(this->codeword_domain_handle_, this->degree_, this->make_zk_));
        this->constituent_oracle_ptrs_.emplace_back(
            std::make_shared<oracle_handle>(this->constituent_oracles_[i]));
    }

    this->oracle_ = std::make_shared<dummy_oracle<FieldT> >(this->num_oracles_);

    this->oracle_handle_ptr_ = std::make_shared<virtual_oracle_handle>(
        this->IOP_.register_virtual_oracle(
            this->codeword_domain_handle_,
            this->degree_,
            this->constituent_oracle_ptrs_,
            this->oracle_));
}

/* Proving */
template<typename FieldT>
void dummy_protocol<FieldT>::calculate_and_submit_response()
{
    for (size_t i = 0; i < this->num_oracles_; ++i)
    {
        std::vector<FieldT> random_coefficients;
        random_coefficients.reserve(this->degree_);
        for (size_t j = 0; j < this->degree_; ++j)
        {
            random_coefficients.emplace_back(FieldT::random_element());
        }
        std::vector<FieldT> random_codeword =
            FFT_over_field_subset<FieldT>(random_coefficients, this->codeword_domain_);
        oracle<FieldT> random_oracle(random_codeword);
        this->IOP_.submit_oracle(this->constituent_oracles_[i], std::move(random_oracle));
    }
}

/* Verification */
template<typename FieldT>
bool dummy_protocol<FieldT>::verifier_predicate()
{
    return true;
}

template<typename FieldT>
oracle_handle_ptr dummy_protocol<FieldT>::get_oracle_handle()
{
    return this->oracle_handle_ptr_;
}

} // namespace libiop
