#include <cstdint>

#include "libiop/algebra/utils.hpp"

namespace libiop {

template<typename FieldT>
void lagrange_cache<FieldT>::construct_internal(const affine_subspace<FieldT> &domain)
{
    this->called_ = false;
    this->last_interpolation_point_ = FieldT::zero();
    this->c_ = this->vp_.get_linearized_polynomial().coefficients()[1].inverse();
}

template<typename FieldT>
void lagrange_cache<FieldT>::construct_internal(const multiplicative_coset<FieldT> &domain)
{
    this->called_ = false;
    this->last_interpolation_point_ = FieldT::zero();

    // cache inverted evaluations of v
    const size_t m = domain.num_elements();
    const FieldT g_inv = domain.generator().inverse();
    FieldT v_inv_i = FieldT(m) * libiop::power(domain.shift(), m - 1);
    this->v_inv_.reserve(m);
    for (std::size_t i = 0; i < m; i++) {
        this->v_inv_.emplace_back(v_inv_i);
        v_inv_i *= g_inv;
    }
}

template<typename FieldT>
lagrange_cache<FieldT>::lagrange_cache(
    const affine_subspace<FieldT> &domain,
    const bool cache_evaluations,
    const bool interpolation_domain_intersects_domain) :
    domain_(domain),
    vp_(domain_),
    cache_evaluations_(cache_evaluations),
    interpolation_domain_intersects_domain_(interpolation_domain_intersects_domain)
{
    this->construct_internal(domain);
}

template<typename FieldT>
lagrange_cache<FieldT>::lagrange_cache(
    const multiplicative_coset<FieldT> &domain,
    const bool cache_evaluations,
    const bool interpolation_domain_intersects_domain) :
    domain_(domain),
    vp_(domain_),
    cache_evaluations_(cache_evaluations),
    interpolation_domain_intersects_domain_(interpolation_domain_intersects_domain)
{
    this->construct_internal(domain.coset());
}

template<typename FieldT>
lagrange_cache<FieldT>::lagrange_cache(
    const field_subset<FieldT> &domain,
    const bool cache_evaluations,
    const bool interpolation_domain_intersects_domain) :
    domain_(domain),
    vp_(domain_),
    cache_evaluations_(cache_evaluations),
    interpolation_domain_intersects_domain_(interpolation_domain_intersects_domain)
{
    if (domain.type() == affine_subspace_type) {
        this->construct_internal(domain.subspace());
    } else if (domain.type() == multiplicative_coset_type) {
        this->construct_internal(domain.coset());
    } else {
        throw std::invalid_argument(
            "lagrange cache only supports affine subspace and multiplicative coset subset types");
    }
}

template<typename FieldT>
std::vector<FieldT> lagrange_cache<FieldT>::subspace_coefficients_for(
    const FieldT &interpolation_point)
{
    /* This function produces the same output as lagrange_coefficients below.
     * This already has c cached, which allows the shifted V to be calculated directly in one pass. */
    const FieldT k = this->vp_.evaluation_at_point(interpolation_point) * this->c_;
    const std::vector<FieldT> V =
        all_subset_sums<FieldT>(this->domain_.basis(), interpolation_point + this->domain_.offset());
    // Handle check if interpolation point is in domain
    if (this->interpolation_domain_intersects_domain_ && k == FieldT::zero()) {
        std::vector<FieldT> result(this->domain_.num_elements(), FieldT::zero());
        for (std::size_t i = 0; i < V.size(); i++) {
            if (V[i] == FieldT::zero()) {
                result[i] = FieldT::one();
                return result;
            }
        }
    }
    std::vector<FieldT> V_inv = batch_inverse_and_mul<FieldT>(V, k);
    return V_inv;
}

template<typename FieldT>
std::vector<FieldT> lagrange_cache<FieldT>::coset_coefficients_for(
    const FieldT &interpolation_point)
{
    /* This function produces the same output as lagrange_coefficients below.
     * This already has v_inv cached, which enables removing one multiplication per loop by taking advantage of
     * batch_inverse_and_mul. */
    const FieldT g = this->domain_.generator();
    const size_t m = this->domain_.num_elements();
    std::vector<FieldT> result;
    result.reserve(m);
    const FieldT x_to_m = libiop::power(interpolation_point, m);
    const FieldT Z_x = x_to_m + this->vp_.constant_coefficient();
    // Check the easy case
    if (this->interpolation_domain_intersects_domain_ && Z_x == FieldT::zero()) {
        FieldT cur = this->domain_.shift();
        for (std::size_t i = 0; i < m; i++) {
            if (cur == interpolation_point) {
                result.emplace_back(FieldT::one());
            } else {
                result.emplace_back(FieldT::zero());
            }
            cur *= g;
        }
        return result;
    }
    FieldT r = this->domain_.shift();
    for (size_t i = 0; i < m; ++i)
    {
        result.emplace_back(this->v_inv_[i] * (interpolation_point - r));
        // TODO: Consider caching computation of r
        r *= g;
    }
    result = batch_inverse_and_mul(result, Z_x);

    return result;
}

template<typename FieldT>
std::vector<FieldT> lagrange_cache<FieldT>::coefficients_for(
    const FieldT &interpolation_point)
{
    if (this->cache_evaluations_ && this->called_ &&
        this->last_interpolation_point_ == interpolation_point)
    {
        return this->last_interpolation_result_;
    }
    std::vector<FieldT> result;
    if (this->domain_.type() == affine_subspace_type) {
        result = this->subspace_coefficients_for(interpolation_point);
    } else if (this->domain_.type() == multiplicative_coset_type) {
        result = this->coset_coefficients_for(interpolation_point);
    }

    if (this->cache_evaluations_) {
        this->called_ = true;
        this->last_interpolation_point_ = interpolation_point;
        this->last_interpolation_result_ = result;
    }

    return result;
}

template<typename FieldT>
std::vector<FieldT> lagrange_coefficients(const field_subset<FieldT> &domain,
                                          const FieldT &interpolation_point)
{
    if (domain.type() == affine_subspace_type) {
        return lagrange_coefficients(domain.subspace(), interpolation_point);
    } else if (domain.type() == multiplicative_coset_type) {
        return lagrange_coefficients(domain.coset(), interpolation_point);
    }
    throw std::invalid_argument(
        "lagrange coefficients only supports affine subspace and multiplicative coset subset types");
}

template<typename FieldT>
std::vector<FieldT> lagrange_coefficients(const multiplicative_coset<FieldT> &domain,
                                          const FieldT &interpolation_point)
{
    /* We assume that the interpolation point is not in domain. */
    const FieldT g = domain.generator();
    const FieldT h = domain.shift();
    const FieldT g_inv = g.inverse();
    const size_t m = domain.num_elements();
    std::vector<FieldT> result;
    result.reserve(m);

    /*
     * Let t be the interpolation point, H be the multiplicative coset, with elements of the form h*g^i.
     Compute each L_{i,H}(t) as Z_{H}(t) * v_i / (t- h g^i)
     where:
     - Z_{H}(t) = \prod_{j} (t-h*g^j) = (t^m-h^m), and
     - v_{i} = 1 / \prod_{j \neq i} h(g^i-g^j).
     Below we use the fact that v_{0} = 1/(m * h^(m-1)) and v_{i+1} = g * v_{i}.
     We compute the inverse of each coefficient, and then batch invert the entire result.
     TODO: explain deriviation more step by step
     */

    const FieldT Z_x = libiop::power(interpolation_point, m) - libiop::power(h, m);
    FieldT l = Z_x.inverse() * FieldT(m) * libiop::power(h, m - 1);
    FieldT r = h;
    for (size_t i = 0; i < m; ++i)
    {
        result.emplace_back(l * (interpolation_point - r));
        // TODO: Consider caching computation of r
        l *= g_inv;
        r *= g;
    }
    result = batch_inverse(result);

    return result;
}

template<typename FieldT>
std::vector<FieldT> lagrange_coefficients(const affine_subspace<FieldT> &domain,
                                          const FieldT &interpolation_point)
{
    /*
      Let V[0], .., V[2^n-1] be elements of the affine
      subspace. Lagrange coefficients at \alpha take the following
      form:

      L[j] = (\prod_{i \neq j} \alpha - V[i]) / (\prod_{i \neq j} V[j] - V[i])

      Note that since V is an affine subspace, the denominator always
      has a *constant* value that only depends on the linear basis
      vectors, and is independent of j and the affine shift.

      Our computation below computes:

      k = (\prod_{i} \alpha - V[i]) = Zero_V(\alpha)
      c = 1/{\prod_{j > 0} (V[j] - domain.offset()) = 1 / (Z_{V - offset} / X)(0)

      and inverses of (\alpha - V[0]), ..., (\alpha - V[2^n-1])

      Then L[j] = k * c / (\alpha - V[j]).
    */

    vanishing_polynomial<FieldT> Z(domain);
    /* (Z_{V - offset} / X)(0) is the formal derivative of Z_V,
     * as the affine shift only affects the constant coefficient. */
    const FieldT c = Z.formal_derivative_at_point(FieldT::zero()).inverse();
    const FieldT k = Z.evaluation_at_point(interpolation_point);

    std::vector<FieldT> V =
        all_subset_sums<FieldT>(domain.basis(), interpolation_point + domain.offset());

    const std::vector<FieldT> V_inv = batch_inverse_and_mul<FieldT>(V, c * k);

    return V_inv;
}

} // namespace libiop
