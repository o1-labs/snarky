#include <algorithm>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/fft.hpp"

namespace libiop {

/* linearized_polynomial */
template<typename FieldT>
linearized_polynomial<FieldT>::linearized_polynomial() :
    polynomial<FieldT>()
{
}

template<typename FieldT>
linearized_polynomial<FieldT>::linearized_polynomial(std::vector<FieldT> &&coefficients) :
    polynomial<FieldT>(std::move(coefficients))
{
}

template<typename FieldT>
FieldT linearized_polynomial<FieldT>::constant_coefficient() const
{
    return (this->coefficients_.empty() ? FieldT(0) :
            this->coefficients_[0]);
}

template<typename FieldT>
FieldT linearized_polynomial<FieldT>::evaluation_at_point(const FieldT &evalpoint) const
{
    if (this->coefficients_.empty())
    {
        return FieldT(0);
    }

    FieldT result = FieldT(*this->coefficients_.begin());
    FieldT evalpoint_pow = evalpoint;

    for (auto it = this->coefficients_.begin() + 1;
         it != this->coefficients_.end(); ++it)
    {
        result += (*it) * evalpoint_pow;
        evalpoint_pow = evalpoint_pow.squared(); // instead of .square, for compatibility with Fp_model from libff
    }

    return result;
}

template<typename FieldT>
std::vector<FieldT> linearized_polynomial<FieldT>::evaluations_over_field_subset(const field_subset<FieldT> &S) const
{
    if (S.type() == affine_subspace_type)
    {
        return this->evaluations_over_subspace(S.subspace());
    }
    throw std::invalid_argument(
        "linearized_polynomial.evaluations_over_field_subset() is only supported for subspaces");
}

template<typename FieldT>
std::vector<FieldT> linearized_polynomial<FieldT>::evaluations_over_subspace(const affine_subspace<FieldT> &S) const
{
    /* We implement /affine/ linearized polynomials for which the
    bilinear property is not directly applicable, as we need to make
    sure that constant term is included only once.

    Therefore, evaluating over subspace below, we subtract constant
    term from evaluations over the basis, but include the constant
    term in the offset calculation. */

    std::vector<FieldT> eval_at_basis(S.basis());
    std::for_each(eval_at_basis.begin(), eval_at_basis.end(),
                  [this](FieldT &el) {
                      el = (this->evaluation_at_point(el) -
                            this->constant_coefficient());
                  });
    const FieldT offset = this->evaluation_at_point(S.offset());

    return all_subset_sums<FieldT>(eval_at_basis, offset);
}

template<typename FieldT>
void linearized_polynomial<FieldT>::square()
{
    if (this->coefficients_.empty())
    {
        return;
    }

    const size_t k = this->coefficients_.size();
    this->coefficients_.resize(k+1);

    for (size_t i = k; i > 0; i--)
    {
        this->coefficients_[i] = this->coefficients_[i-1].squared();
    }

    this->coefficients_[0] = FieldT(0);
}

template<typename FieldT>
linearized_polynomial<FieldT> linearized_polynomial<FieldT>::squared() const
{
    linearized_polynomial<FieldT> result(*this);
    result.square();
    return result;
}

template<typename FieldT>
polynomial<FieldT> linearized_polynomial<FieldT>::expand_as_polynomial() const
{
    const std::size_t m = this->coefficients_.size();
    if (m == 0)
    {
        return polynomial<FieldT>();
    }
    else if (m == 1)
    {
        std::vector<FieldT> coeffs(1, FieldT(this->coefficients_[0]));
        return polynomial<FieldT>(std::move(coeffs));
    }
    else
    {
        std::vector<FieldT> coeffs((1ull<<(m-2))+1);
        coeffs[0] = this->coefficients_[0];

        for (size_t idx = 1; idx < m; ++idx)
        {
            coeffs[1ull<<(idx-1)] = this->coefficients_[idx];
        }

        return polynomial<FieldT>(std::move(coeffs));
    }
}

template<typename FieldT>
std::size_t linearized_polynomial<FieldT>::degree() const
{
    if (this->coefficients_.size() <= 1)
    {
        return 0;
    }
    else
    {
        return (1ull<<(this->coefficients_.size() - 2));
    }
}

template<typename FieldT>
linearized_polynomial<FieldT>& linearized_polynomial<FieldT>::operator+=(const linearized_polynomial<FieldT> &other)
{
    this->add_coefficients_of(other);
    return (*this);
}

template<typename FieldT>
linearized_polynomial<FieldT> linearized_polynomial<FieldT>::operator+(const linearized_polynomial<FieldT> &other) const
{
    linearized_polynomial<FieldT> result(*this);
    result += other;
    return result;
}

template<typename FieldT>
linearized_polynomial<FieldT>& linearized_polynomial<FieldT>::operator*=(const FieldT &el)
{
    this->multiply_coefficients_by(el);
    return (*this);
}

template<typename FieldT>
linearized_polynomial<FieldT> linearized_polynomial<FieldT>::operator*(const FieldT &el) const
{
    linearized_polynomial<FieldT> result(*this);
    result *= el;
    return result;
}

template<typename FieldT>
bool linearized_polynomial<FieldT>::operator==(const linearized_polynomial<FieldT> &other) const
{
    return (this->coefficients_equivalent_with(other));
}

template<typename FieldT>
bool linearized_polynomial<FieldT>::operator!=(const linearized_polynomial<FieldT> &other) const
{
    return !(this->operator==(other));
}

template<typename FieldT>
void add_scalar_multiple_at_offset(std::vector<FieldT> &result,
                                   const std::vector<FieldT> &p,
                                   const FieldT &factor,
                                   const size_t offset) {
    // This is a helper method for linearized polynomial * polynomial
    // It adds factor * p to result, starting at offset
    if (factor == FieldT::zero()) {
        return;
    }
    for (std::size_t i = 0; i < p.size(); i++) {
        result[i + offset] += p[i] * factor;
    }
}

template<typename FieldT>
polynomial<FieldT> linearized_polynomial<FieldT>::operator*(const polynomial<FieldT> &p) const
{
    // This is implementing naive polynomial multiplication,
    //   which is  L[0] * p + L[1] * p * x + L[2] * p * x^2 + L[3] * p * x^4 + ...
    // The polynomial coefficient representation has the constant term on the left,
    // and higher degrees growing towards the right.
    // This adds each term progressively, using add_scalar_multiple_at_offset

    // Set num elements in result correctly
    std::vector<FieldT> result(p.degree() + 1 + this->degree(), FieldT::zero());
    const std::vector<FieldT> p_coeff = p.coefficients();
    // set result to be L[0] * p
    add_scalar_multiple_at_offset(result, p_coeff, this->coefficients_[0], 0);
    for (std::size_t i = 1; i < this->coefficients_.size(); i++) {
        std::size_t offset = 1 << (i - 1);
        add_scalar_multiple_at_offset(result, p_coeff, this->coefficients_[i], offset);
    }
    return polynomial<FieldT>(std::move(result));
}

template<typename FieldT>
linearized_polynomial<FieldT> linearized_polynomial<FieldT>::random_linearized_polynomial(const size_t degree_exponent)
{
    std::vector<FieldT> random_coefficients = random_vector<FieldT>(degree_exponent+2);
    return linearized_polynomial<FieldT>(std::move(random_coefficients));
}

/* Returns P / Z as a pair.
   The first element in the pair is the quotient, the latter is the remainder. */
template<typename FieldT>
std::pair<polynomial<FieldT>,
          polynomial<FieldT> >
polynomial_over_linearized_polynomial(const polynomial<FieldT> &P,
                                      const linearized_polynomial<FieldT> &Z)
{
    /* inverse of the leading term */
    const FieldT linv = Z[Z.num_terms()-1].inverse();
    const std::size_t deg_Z = Z.degree();

    if (P.degree() < deg_Z)
    {
        polynomial<FieldT> quotient;
        std::vector<FieldT> coeff_copy = P.coefficients();
        polynomial<FieldT> remainder(std::move(coeff_copy));
        remainder.set_degree(deg_Z-1);

        return std::make_pair(std::move(quotient), std::move(remainder));
    }

    std::vector<FieldT> quotient(P.coefficients().begin() + deg_Z,
                          P.coefficients().end());
    std::vector<FieldT> remainder(P.coefficients().begin(),
                          P.coefficients().begin() + deg_Z);

    for (std::size_t i = quotient.size(); i--; )
    {
        const FieldT twist = quotient[i] * linv;
        quotient[i] = twist;

        /* subtract twist*Z * y^i thus clearing the i-th term of P */
        if (Z.num_terms() >= 2)
        {
            std::size_t p_pow = deg_Z/2;
            for (std::size_t j = Z.num_terms()-1; j--; )
            {
                if (i + p_pow < deg_Z)
                {
                    remainder[i + p_pow] -= twist * Z[j];
                }
                else
                {
                    quotient[i + p_pow - deg_Z] -= twist * Z[j];
                }

                p_pow /= 2; /* note that this will go from 1 to 0 for the last term */
            }
        }
    }

    return std::make_pair(std::move(polynomial<FieldT>(std::move(quotient))),
                          std::move(polynomial<FieldT>(std::move(remainder))));
}

} // namespace libiop
