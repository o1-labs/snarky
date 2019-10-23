#include <memory>
#include <stdexcept>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/utils.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/common/profiling.hpp"

namespace libiop {

/** This is the virtual oracle for g which the verifier constructs using evaluations of
 *  h and f, along with the claimed sum. If the claimed was wrong, the constructed
 *  polynomial will be of degree greater than |H| - 1.
*/
template<typename FieldT>
class sumcheck_g_oracle : public virtual_oracle<FieldT> {
protected:
    field_subset<FieldT> summation_domain_;
    field_subset<FieldT> codeword_domain_;

    FieldT claimed_sum_;
    FieldT order_H_inv_times_claimed_sum_;
    FieldT eps_;
    FieldT eps_inv_times_claimed_sum_;
    const field_subset_type field_subset_type_;
    const vanishing_polynomial<FieldT> Z_;
public:
    sumcheck_g_oracle(const field_subset<FieldT> &summation_domain,
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
        /** [BCRSVW18] protocol 5.3, step 3, computing p in RS[L, (|H|-1) / L] */
        if (constituent_oracle_evaluations.size() != 2)
        {
            throw std::invalid_argument("sumcheck_g_oracle has two constituent oracles");
        }

        enter_block("Sumcheck: g evaluated contents");

        /* evaluations of \hat{f} */
        std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>(
            *constituent_oracle_evaluations[0].get());
        const std::vector<FieldT> Z_over_L = this->Z_.evaluations_over_field_subset(this->codeword_domain_);
        if (this->field_subset_type_ == affine_subspace_type) {
            /** In the additive case this is computing p in RS[L, (|H|-1) / L],
             *  where p as described in the paper is:
             *  p(x) = eps * f(x) - mu * x^{|H| - 1} - eps * Z_H(x) * h(x)
             *
             *  It is equivalent to check if the following is low degree:
             *  p'(x) = eps^{-1} * p(x)
             *        = (f(x) - eps^{-1} * mu * x^{|H| - 1} - Z_H(x) * h(x))
             *  We use the latter due to the reduced prover time.
             */

            const std::vector<FieldT> eps_inv_times_claimed_sum_times_x_to_H_minus_1 =
                constant_times_subspace_to_order_H_minus_1(
                    this->eps_inv_times_claimed_sum_,
                    this->codeword_domain_.subspace(),
                    this->summation_domain_.num_elements());

            /** Compute p, by performing the correct arithmetic on the evaluations */
            for (std::size_t i = 0; i < result->size(); ++i)
            {
                result->operator[](i) -= (eps_inv_times_claimed_sum_times_x_to_H_minus_1[i]
                    + Z_over_L[i] * constituent_oracle_evaluations[1]->operator[](i));
            }
        } else if (this->field_subset_type_ == multiplicative_coset_type) {
            /** In the multiplicative case this is computing p in RS[L, (|H|-1) / L],
             *  where p as described in the paper is:
             *  p(x) = (|H| * f(x) - mu - |H| * Z_H(x) * h(x)) * (x^-1)
             *
             *  It is equivalent to check if the following is low degree:
             *  p'(x) = |H|^{-1}p(x)
             *        = (f(x) - |H|^{-1} * mu - Z_H(x) * h(x)) * (x^-1)
             *  We use the latter due to the reduced prover time.
             */

            FieldT cur_x_inv = this->codeword_domain_.shift().inverse();
            FieldT generator_inv = this->codeword_domain_.generator().inverse();

            /** Compute p, by performing the correct arithmetic on the evaluations */
            for (std::size_t i = 0; i < result->size(); ++i)
            {
                result->operator[](i) -= (this->order_H_inv_times_claimed_sum_ +
                    Z_over_L[i] * constituent_oracle_evaluations[1]->operator[](i));
                result->operator[](i) *= cur_x_inv;
                cur_x_inv *= generator_inv;
            }
        }
        leave_block("Sumcheck: g evaluated contents");
        return result;
    }

    virtual FieldT evaluation_at_point(
        const std::size_t evaluation_position,
        const FieldT evaluation_point,
        const std::vector<FieldT> &constituent_oracle_evaluations) const
    {
        UNUSED(evaluation_position);
        /* oracle evaluations should be f(x) and h(x) */
        if (constituent_oracle_evaluations.size() != 2)
        {
            throw std::invalid_argument("sumcheck_g_oracle has two constituent oracles");
        }
        /** [BCRSVW18] protocol 5.3, step 3, computing p in RS[L, (|H|-1) / L] */

        const FieldT f_at_x = constituent_oracle_evaluations[0];
        const FieldT h_at_x = constituent_oracle_evaluations[1];
        const FieldT Z_at_x = this->Z_.evaluation_at_point(evaluation_point);

        if (this->field_subset_type_ == affine_subspace_type) {
            /** In the additive case this is computing p'(x), where
             *  p'(x) = f(x) - eps^{-1} * mu * x^{|H| - 1} - Z_H(x) * h(x)
             */

            return (f_at_x
                - this->eps_inv_times_claimed_sum_ * libiop::power(evaluation_point, this->summation_domain_.num_elements() - 1)
                - Z_at_x * h_at_x);
        } else if (this->field_subset_type_ == multiplicative_coset_type) {
            /** In the multiplicative case this is computing p'(x), where
             *  p'(x) = (f(x) - |H|^{-1} * mu - Z_H(x) * h(x)) * (x^-1)
             */

            const FieldT x_inv = evaluation_point.inverse();

            /** Compute p(x), by performing the correct arithmetic on the evaluations */
            return (f_at_x
                - this->order_H_inv_times_claimed_sum_
                - Z_at_x * h_at_x)
                * x_inv;
        }
        return FieldT::zero();
    }
};

/* Initialize domains, domain sizes, and the degree of g and h */
template<typename FieldT>
batch_sumcheck_protocol<FieldT>::batch_sumcheck_protocol(
    iop_protocol<FieldT> &IOP,
    const domain_handle &summation_domain_handle,
    const domain_handle &codeword_domain_handle,
    const std::size_t degree_bound,
    const bool make_zk,
    const field_subset_type domain_type) :
    IOP_(IOP),
    summation_domain_handle_(summation_domain_handle),
    codeword_domain_handle_(codeword_domain_handle),
    degree_bound_(degree_bound),
    make_zk_(make_zk),
    field_subset_type_(domain_type)
{
    this->summation_domain_ = this->IOP_.get_domain(this->summation_domain_handle_);
    this->codeword_domain_ = this->IOP_.get_domain(this->codeword_domain_handle_);

    this->summation_domain_size_ = this->summation_domain_.num_elements();

    this->g_degree_ = this->summation_domain_size_ - 1;
    this->h_degree_ = this->degree_bound_ - this->summation_domain_size_;
}

template<typename FieldT>
void batch_sumcheck_protocol<FieldT>::register_masking_polynomial()
{
    this->masking_poly_handle_ = this->IOP_.register_oracle(this->codeword_domain_handle_, this->degree_bound_, this->make_zk_);
}

template<typename FieldT>
void batch_sumcheck_protocol<FieldT>::register_challenge()
{
    /** If there are n sumcheck instances, n - 1 random points are required.
     *  In the zk case, an additional point of verifier randomness is required. */
    int64_t num_oracles = this->oracle_handles_.size() + (this->make_zk_ ? 1 : 0);
    this->challenge_handle_ = this->IOP_.register_verifier_random_message(num_oracles);
}

template<typename FieldT>
void batch_sumcheck_protocol<FieldT>::attach_oracle_for_summing(const oracle_handle_ptr &handle,
                                                                const FieldT claimed_sum)
{
    if (this->combined_f_oracle_)
    {
        throw std::logic_error("Called attach_oracle_for_summing after register_proof.");
    }

    this->oracle_uid_to_registration_index_[handle->uid()] = this->oracle_handles_.size();
    this->oracle_handles_.emplace_back(handle);
    this->claimed_sums_.emplace_back(claimed_sum);
}

template<typename FieldT>
void batch_sumcheck_protocol<FieldT>::set_oracle_claimed_sum(const oracle_handle_ptr &handle,
                                                             const FieldT claimed_sum)
{
    if (this->oracle_uid_to_registration_index_.count(handle->uid()) == 0)
    {
        throw std::invalid_argument("Sumcheck: set_oracle_claimed_sum"
            " was called on a handle that was not attached for sumchecking");
    }
    const size_t registration_index = this->oracle_uid_to_registration_index_[handle->uid()];
    this->claimed_sums_[registration_index] = claimed_sum;
}

template<typename FieldT>
void batch_sumcheck_protocol<FieldT>::register_proof()
{
    /* h does not have to be in a zk commitment, as zk-sumcheck operates on a uniform codeword of the given rate. */
    const bool make_h_oracle_zk = false;
    this->h_handle_ = this->IOP_.register_oracle(this->codeword_domain_handle_, this->h_degree_, make_h_oracle_zk);

    /* We know that at this point all oracles have been registered already */
    /* +1 accounts for the pad, which is only used in the zero knowledge case */
    int64_t pad = this->make_zk_ ? 1 : 0;
    this->combined_f_oracle_ = std::make_shared<random_linear_combination_oracle<FieldT> >(pad + this->oracle_handles_.size());

    std::vector<oracle_handle_ptr> all_constituent_oracles;
    if (this->make_zk_) {
        all_constituent_oracles.emplace_back(std::make_shared<oracle_handle>(this->masking_poly_handle_));
    }
    all_constituent_oracles.insert(all_constituent_oracles.end(),
                                   this->oracle_handles_.begin(),
                                   this->oracle_handles_.end());

    this->combined_f_oracle_handle_ = this->IOP_.register_virtual_oracle(
        this->codeword_domain_handle_,
        this->degree_bound_,
        all_constituent_oracles,
        this->combined_f_oracle_,
        true);

    this->g_oracle_ = std::make_shared<sumcheck_g_oracle<FieldT> >(
        this->summation_domain_, this->codeword_domain_, this->field_subset_type_);

    this->g_handle_ = this->IOP_.register_virtual_oracle(
        this->codeword_domain_handle_,
        this->g_degree_,
        { std::make_shared<virtual_oracle_handle>(this->combined_f_oracle_handle_),
          std::make_shared<oracle_handle>(this->h_handle_) },
        this->g_oracle_);
}

template<typename FieldT>
oracle_handle batch_sumcheck_protocol<FieldT>::get_masking_poly_oracle_handle() const
{
    return this->masking_poly_handle_;
}

template<typename FieldT>
oracle_handle batch_sumcheck_protocol<FieldT>::get_h_oracle_handle() const
{
    return this->h_handle_;
}

template<typename FieldT>
virtual_oracle_handle batch_sumcheck_protocol<FieldT>::get_g_oracle_handle() const
{
    return this->g_handle_;
}

/* Proving */
template<typename FieldT>
void batch_sumcheck_protocol<FieldT>::submit_masking_polynomial()
{
    /** The sum of any polynomial m, over all of H is:
     *  Σ_{a in H} m(a) = Σ_{a in H} g(a) + Z_H * h(a) = Σ_{a in H} g(a), where deg(g) < |H|
     *  We seek to sample a random polynomial of degree d, which sums to 0 over H.
     *  We do this as follows:
     *  1) sample a random polynomial g of deg |H| - 1, and h of degree (d - |H|)
     *  2) alter g such that its sum over H is 0
     *  3) compute m using the identity m = Z_H * h + g
     *  4) convert m to the codeword domain and submit it */
    enter_block("Sumcheck: sample masking polynomial components");
    polynomial<FieldT> masking_g_poly = polynomial<FieldT>::random_polynomial(this->summation_domain_size_);
    const polynomial<FieldT> masking_h_poly = polynomial<FieldT>::random_polynomial(this->h_degree_);
    leave_block("Sumcheck: sample masking polynomial components");

    enter_block("Sumcheck: compute masking polynomial codeword");
    const vanishing_polynomial<FieldT> summation_vp(this->summation_domain_);

    if (this->field_subset_type_ == multiplicative_coset_type) {
        /** When H is a multiplicative group, Σ_{a in H} g(a) = g(0) * |H|,
         *  thus it sums to 0 if and only if g(0) = 0 */
        masking_g_poly[0] = FieldT::zero();
    } else if (this->field_subset_type_ == affine_subspace_type) {
        /** When H is an additive subspace, Σ_{a in H} g(a) = beta * Σ_{a in H} a^{|H| - 1},
         *  where beta is the term of g of degree (|H| - 1).
         *  Σ_{a in H} a^{|H| - 1} is equal to the linear term of Z_H.
         *  See section 5 of Aurora for more detail of the above.
         *  Consequently, it sums to 0 if and only if the coefficient of a^{|H| - 1} is 0 */
        masking_g_poly[this->summation_domain_size_-1] = FieldT::zero();
    }
    this->masking_poly_ = (summation_vp * masking_h_poly) + masking_g_poly;
    this->masking_poly_oracle_ = this->IOP_.submit_oracle(
        this->masking_poly_handle_,
        oracle<FieldT>(FFT_over_field_subset<FieldT>(
            this->masking_poly_.coefficients(), this->codeword_domain_)));
    leave_block("Sumcheck: compute masking polynomial codeword");
}

template<typename FieldT>
FieldT batch_sumcheck_protocol<FieldT>::get_combined_claimed_sum(std::vector<FieldT> challenge) {
    FieldT combined_f_oracle_sum = FieldT::zero();
    std::size_t start_challenge = 0;
    if (this->make_zk_) {
        // Masking polynomial sum is always 0
        start_challenge = 1;
    }
    for (std::size_t i = 0; i < this->claimed_sums_.size(); ++i)
    {
        combined_f_oracle_sum += challenge[i + start_challenge] * this->claimed_sums_[i];
    }
    return combined_f_oracle_sum;
}

template<typename FieldT>
void batch_sumcheck_protocol<FieldT>::calculate_and_submit_proof()
{
    const std::vector<FieldT> challenge =
        this->IOP_.obtain_verifier_random_message(this->challenge_handle_);
    this->combined_f_oracle_->set_random_coefficients(challenge);

    const std::shared_ptr<std::vector<FieldT>> combined_f_oracle_evaluations
        = this->IOP_.get_oracle_evaluations(std::make_shared<virtual_oracle_handle>(this->combined_f_oracle_handle_));
    std::vector<FieldT> combined_f_oracle_polynomial =
        IFFT_of_known_degree_over_field_subset<FieldT>(
            *combined_f_oracle_evaluations.get(), this->degree_bound_, this->codeword_domain_);
    combined_f_oracle_polynomial.resize(this->degree_bound_);

    FieldT combined_f_oracle_sum = get_combined_claimed_sum(challenge);
    this->g_oracle_->set_claimed_sum(combined_f_oracle_sum);

    /* Calculate g, h and actual sum from f */
    const vanishing_polynomial<FieldT> vanishing_polynomial(this->summation_domain_);
    std::pair<polynomial<FieldT>, polynomial<FieldT>> h_and_g =
        polynomial_over_vanishing_polynomial<FieldT>(
            polynomial<FieldT>(std::move(combined_f_oracle_polynomial)),
            vanishing_polynomial);
    const polynomial<FieldT> h = h_and_g.first;
#ifdef DEBUG
    polynomial<FieldT> g = h_and_g.second;
    FieldT actual_sum;
    if (this->field_subset_type_ == affine_subspace_type) {
        /* The second component is actually g + sum * x^{H_size-1}, but
            we immediately remove its highest degree term */
        actual_sum = g[this->summation_domain_size_-1];
        g.set_degree(this->summation_domain_size_-2, true);
    } else if (this->field_subset_type_ == multiplicative_coset_type) {
        /* The second component is actually x*g + sum, but
            we immediately remove its lowest degree term */
        actual_sum = g[0];
        g.remove_term(0);
    }
#endif // DEBUG

    // The prover does not submit g, as it is computed using f, h, and the claimed sum
    oracle<FieldT> h_oracle(FFT_over_field_subset<FieldT>(
                                h.coefficients(),
                                this->codeword_domain_));
    this->IOP_.submit_oracle(this->h_handle_, std::move(h_oracle));
}

/* Verification */
template<typename FieldT>
void batch_sumcheck_protocol<FieldT>::construct_verifier_state()
{
    std::vector<FieldT> challenge =
        this->IOP_.obtain_verifier_random_message(this->challenge_handle_);
    this->combined_f_oracle_->set_random_coefficients(challenge);
    // In the code, the masking polynomial sum is always 0.
    FieldT combined_f_oracle_sum = get_combined_claimed_sum(challenge);
    this->g_oracle_->set_claimed_sum(combined_f_oracle_sum);
}

template<typename FieldT>
std::vector<oracle_handle_ptr> batch_sumcheck_protocol<FieldT>::get_all_oracle_handles()
{
    std::vector<oracle_handle_ptr> result;
    if (this->make_zk_) {
        result.emplace_back(std::make_shared<oracle_handle>(this->get_masking_poly_oracle_handle()));
    }
    result.emplace_back(std::make_shared<oracle_handle>(this->get_h_oracle_handle()));
    result.emplace_back(std::make_shared<virtual_oracle_handle>(this->get_g_oracle_handle()));
    return result;
}

} // namespace libiop
