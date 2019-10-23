#include <memory>
#include <stdexcept>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

/** This is the virtual oracle for rational sumcheck.
 *  It takes as input a polynomial p, and a rational function f.
 *  (Represented internally as a numerator and denominator oracle)
 *
 *  This oracle being low degree implies that the sum of p over the summation domain
 *  is the claimed sum, and that p is equal to f over the summation domain.
*/
template<typename FieldT>
class sumcheck_constraint_oracle : public virtual_oracle<FieldT> {
protected:
    field_subset<FieldT> summation_domain_;
    field_subset<FieldT> codeword_domain_;

    const field_subset_type field_subset_type_;
    const vanishing_polynomial<FieldT> Z_;

    FieldT claimed_sum_;
    FieldT order_H_inv_times_claimed_sum_;
    FieldT eps_;
    FieldT eps_inv_times_claimed_sum_;
public:
    sumcheck_constraint_oracle(
        const field_subset<FieldT> &summation_domain,
        const field_subset<FieldT> &codeword_domain,
        const field_subset_type domain_type) :
        summation_domain_(summation_domain),
        codeword_domain_(codeword_domain),
        field_subset_type_(domain_type),
        Z_(vanishing_polynomial<FieldT>(summation_domain))
    {
        if (this->field_subset_type_ == affine_subspace_type) {
            /* coefficient for the linear term of the vanishing polynomial */
            this->eps_ = this->Z_.get_linearized_polynomial().coefficients()[1];
        }
    }

    void set_claimed_sum(const FieldT claimed_sum)
    {
        this->claimed_sum_ = claimed_sum;
        if (this->field_subset_type_ == multiplicative_coset_type)
        {
            /** This only works when Fr is a finite field with
             *  characteristic > summation_domain.num_elements() */
            const FieldT order_H = FieldT(this->summation_domain_.num_elements());
            this->order_H_inv_times_claimed_sum_ = order_H.inverse() * this->claimed_sum_;
        }
        else if (this->field_subset_type_ == affine_subspace_type)
        {
            this->eps_inv_times_claimed_sum_ = this->eps_.inverse() * this->claimed_sum_;
        }
    }

    virtual std::shared_ptr<std::vector<FieldT>> evaluated_contents(
        const std::vector<std::shared_ptr<std::vector<FieldT>> > &constituent_oracle_evaluations) const
    {
        /** The input is expected to be of the form: (p, N, D)
         *  where p is output by rational sumcheck,
         *  N is the numerator of the rational function, and D its denominator.
        */
        if (constituent_oracle_evaluations.size() != 3)
        {
            throw std::invalid_argument("sumcheck_constraint_oracle has three constituent oracles");
        }

        /* evaluations of p */
        std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>(
            *constituent_oracle_evaluations[0].get());
        const std::vector<FieldT> Z_inv_over_L = batch_inverse(
            this->Z_.evaluations_over_field_subset(this->codeword_domain_));
        if (this->field_subset_type_ == affine_subspace_type)
        {
            /** In the additive case q(x) is:
             *  q(x) = (D(x) * (p(x) + eps^{-1} * claimed_sum * X^{|H| - 1}) - N(x)) / Z_H
             */
            const std::vector<FieldT> eps_inv_times_claimed_sum_times_x_to_H_minus_1 =
                constant_times_subspace_to_order_H_minus_1(
                    this->eps_inv_times_claimed_sum_,
                    this->codeword_domain_.subspace(),
                    this->summation_domain_.num_elements());

            /** Compute q, by performing the correct arithmetic on the evaluations */
            for (std::size_t i = 0; i < result->size(); ++i)
            {
                const FieldT N_x = constituent_oracle_evaluations[1]->operator[](i);
                const FieldT D_x = constituent_oracle_evaluations[2]->operator[](i);
                result->operator[](i) = ((D_x *
                    (
                        result->operator[](i) + eps_inv_times_claimed_sum_times_x_to_H_minus_1[i])) - N_x
                    ) * Z_inv_over_L[i];
            }
        } else if (this->field_subset_type_ == multiplicative_coset_type) {
            /** In the multiplicative case q(x) is:
             *  q(x) = (D(x) * (x*p(x) + |H|^{-1} * claimed_sum) - N(x)) / Z_H
             */
            std::vector<FieldT> domain_elems = this->codeword_domain_.all_elements();
            /** Compute q, by performing the correct arithmetic on the evaluations */
            for (std::size_t i = 0; i < result->size(); ++i)
            {
                const FieldT x = domain_elems[i];
                const FieldT N_x = constituent_oracle_evaluations[1]->operator[](i);
                const FieldT D_x = constituent_oracle_evaluations[2]->operator[](i);
                result->operator[](i) = ((D_x *
                    (
                        result->operator[](i) * x + this->order_H_inv_times_claimed_sum_)) - N_x
                    ) * Z_inv_over_L[i];
            }
        }
        return result;
    }

    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const
    {
        UNUSED(evaluation_position);
        /** The input is expected to be of the form: (p, N, D)
         *  where p is the codeword outputted by rational sumcheck,
         *  N is the numerator of the rational function, and D its denominator.
        */
        if (constituent_oracle_evaluations.size() != 3)
        {
            throw std::invalid_argument("sumcheck_constraint_oracle has three constituent oracles");
        }

        const FieldT x = evaluation_point;
        const FieldT g_at_x = constituent_oracle_evaluations[0];
        const FieldT N_at_x = constituent_oracle_evaluations[1];
        const FieldT D_at_x = constituent_oracle_evaluations[2];
        const FieldT Z_at_x_inv = this->Z_.evaluation_at_point(x).inverse();
        if (this->field_subset_type_ == affine_subspace_type) {
            /** In the additive case this is computing q(x), where
             *  q(x) = (D(x) * (p(x) + eps^{-1} * claimed_sum * x^{|H| - 1}) - N(x)) / Z_H
             */
            const FieldT x_to_H_minus_1 = libiop::power(x, this->summation_domain_.num_elements() - 1);

            /** Compute q(x), by performing the correct arithmetic on the evaluations */
            return (D_at_x * (g_at_x + x_to_H_minus_1 * this->eps_inv_times_claimed_sum_) - N_at_x)
                     * Z_at_x_inv;
        } else if (this->field_subset_type_ == multiplicative_coset_type) {
            /** In the multiplicative case this is computing q(x), where
             *  q(x) = (D(x) * (x*p(x) + |H|^{-1} * claimed_sum) - N(x)) / Z_H
             *  with H being the summation domain.
             */

            /** Compute q(x), by performing the correct arithmetic on the evaluations */
            return (D_at_x * (x * g_at_x + this->order_H_inv_times_claimed_sum_) - N_at_x)
                     * Z_at_x_inv;
        }
        assert(false);
        return FieldT::zero();
    }
};

/* Initialize domains, domain sizes, and the degree of g and h */
template<typename FieldT>
rational_sumcheck_protocol<FieldT>::rational_sumcheck_protocol(
    iop_protocol<FieldT> &IOP,
    const domain_handle &summation_domain_handle,
    const domain_handle &codeword_domain_handle,
    const std::size_t numerator_degree_bound,
    const std::size_t denominator_degree_bound,
    const field_subset_type domain_type) :
    IOP_(IOP),
    summation_domain_handle_(summation_domain_handle),
    codeword_domain_handle_(codeword_domain_handle),
    numerator_degree_bound_(numerator_degree_bound),
    denominator_degree_bound_(denominator_degree_bound),
    field_subset_type_(domain_type)
{
    this->summation_domain_ = this->IOP_.get_domain(this->summation_domain_handle_);
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);

    this->summation_domain_size_ = this->summation_domain_.num_elements();

    this->reextended_oracle_degree_ = this->summation_domain_size_ - 1;
    this->constraint_oracle_degree_ =
        std::max(numerator_degree_bound,
                 denominator_degree_bound + this->summation_domain_size_ - 1)
            - this->summation_domain_size_;
}

template<typename FieldT>
void rational_sumcheck_protocol<FieldT>::register_summation_oracle(
    const oracle_handle_ptr &numerator_handle,
    const oracle_handle_ptr &denominator_handle)
{
    /** TODO: Check degrees here */
    this->numerator_handle_ = numerator_handle;
    this->denominator_handle_ = denominator_handle;
}

template<typename FieldT>
void rational_sumcheck_protocol<FieldT>::register_proof()
{
    const bool make_zk = false;
    this->reextended_oracle_handle_ =
        this->IOP_.register_oracle(
            this->codeword_domain_handle_,
            this->reextended_oracle_degree_,
            make_zk);
    this->constraint_oracle_ = std::make_shared<sumcheck_constraint_oracle<FieldT>>(
        this->summation_domain_, this->codeword_domain_, this->field_subset_type_);
    this->constraint_oracle_handle_ = this->IOP_.register_virtual_oracle(
        this->codeword_domain_handle_,
        this->constraint_oracle_degree_,
        {   this->get_reextended_oracle_handle(),
            this->numerator_handle_,
            this->denominator_handle_},
        this->constraint_oracle_);
}

template<typename FieldT>
void rational_sumcheck_protocol<FieldT>::calculate_and_submit_proof(
    const std::vector<FieldT> &rational_function_over_summation_domain)
{
    std::vector<FieldT> reextended_poly_coeffs = IFFT_over_field_subset<FieldT>(
        rational_function_over_summation_domain, this->summation_domain_);
    if (this->field_subset_type_ == multiplicative_coset_type)
    {
        const FieldT order_H = FieldT(this->summation_domain_.num_elements());
        this->claimed_sum_ = reextended_poly_coeffs[0] * order_H;
        reextended_poly_coeffs.erase(reextended_poly_coeffs.begin());
    }
    else if (this->field_subset_type_ == affine_subspace_type)
    {
        const vanishing_polynomial<FieldT> Z_H(this->summation_domain_);
        const FieldT eps = Z_H.get_linearized_polynomial().coefficients()[1];
        this->claimed_sum_ = eps * reextended_poly_coeffs[this->summation_domain_size_-1];
        reextended_poly_coeffs.pop_back();
    }
    this->IOP_.submit_oracle(this->reextended_oracle_handle_,
        FFT_over_field_subset<FieldT>(reextended_poly_coeffs, this->codeword_domain_));
    this->constraint_oracle_->set_claimed_sum(this->claimed_sum_);
}

template<typename FieldT>
FieldT rational_sumcheck_protocol<FieldT>::get_claimed_sum() const
{
    return this->claimed_sum_;
}

/* Verification */
template<typename FieldT>
void rational_sumcheck_protocol<FieldT>::construct_verifier_state(const FieldT claimed_sum)
{
    this->claimed_sum_ = claimed_sum;
    this->constraint_oracle_->set_claimed_sum(claimed_sum);
}

template<typename FieldT>
oracle_handle_ptr rational_sumcheck_protocol<FieldT>::get_reextended_oracle_handle() const
{
    return std::make_shared<oracle_handle>(this->reextended_oracle_handle_);
}

template<typename FieldT>
oracle_handle_ptr rational_sumcheck_protocol<FieldT>::get_constraint_oracle_handle() const
{
    return std::make_shared<virtual_oracle_handle>(this->constraint_oracle_handle_);
}

template<typename FieldT>
std::vector<oracle_handle_ptr> rational_sumcheck_protocol<FieldT>::get_all_oracle_handles() const
{
    return {this->get_reextended_oracle_handle(), this->get_constraint_oracle_handle()};
}

} // namespace libiop
