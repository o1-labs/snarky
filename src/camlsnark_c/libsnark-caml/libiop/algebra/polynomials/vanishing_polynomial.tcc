#include <algorithm>

#include "libiop/algebra/exponentiation.hpp"
#include "libiop/algebra/field_subset/field_subset.hpp"
#include "libiop/algebra/field_subset/basis_utils.hpp"
#include "libiop/algebra/polynomials/polynomial.hpp"
#include "libiop/algebra/polynomials/linearized_polynomial.hpp"
#include "libiop/algebra/fft.hpp"
#include "libiop/algebra/utils.hpp"

namespace libiop {

/* vanishing_polynomial<FieldT>(field_subset<FieldT>) */
template<typename FieldT>
vanishing_polynomial<FieldT>::vanishing_polynomial(const field_subset<FieldT> &S) :
    type_(S.type())
{
    this->vp_degree_ = S.num_elements();
    if (this->type_ == affine_subspace_type) {
        this->linearized_polynomial_ = vanishing_polynomial_from_subspace(S.subspace());
    } else if (this->type_ == multiplicative_coset_type) {
        this->vp_offset_ = libiop::power(S.coset().shift(), this->vp_degree_);
    } else {
        throw std::invalid_argument("field_subset type unsupported.");
    }
}

template<typename FieldT>
vanishing_polynomial<FieldT>::vanishing_polynomial(const affine_subspace<FieldT> &S) :
    type_(affine_subspace_type)
{
    this->vp_degree_ = S.num_elements();
    this->linearized_polynomial_ = vanishing_polynomial_from_subspace(S);
}

template<typename FieldT>
vanishing_polynomial<FieldT>::vanishing_polynomial(const multiplicative_coset<FieldT> &S) :
    type_(multiplicative_coset_type)
{
    this->vp_degree_ = S.num_elements();
    this->vp_offset_ = libiop::power(S.coset().shift(), this->vp_degree_);
}

template<typename FieldT>
FieldT vanishing_polynomial<FieldT>::evaluation_at_point(const FieldT &evalpoint) const {
    if (this->type_ == affine_subspace_type) {
        return this->linearized_polynomial_.evaluation_at_point(evalpoint);
    } else if (this->type_ == multiplicative_coset_type) {
        return libiop::power(evalpoint, this->vp_degree_) - this->vp_offset_;
    }
    throw std::logic_error("vanishing_polynomial<FieldT>::evaluation_at_point: "
        " this shouldn't happen");
}

template<typename FieldT>
FieldT vanishing_polynomial<FieldT>::formal_derivative_at_point(const FieldT &evalpoint) const {
    /** This calculates (DZ_S)(x) */
    if (this->type_ == multiplicative_coset_type)
    {
        /** In the multiplicative case, the formal derivative is |S|x^{|S| - 1} */
        return FieldT(this->vp_degree_) *
            libiop::power(evalpoint, this->vp_degree_ - 1);
    }
    else if (this->type_ == affine_subspace_type)
    {
        /** The formal derivative in the additive case is the linear coefficient of Z_S,
         *  due to Z_S being a linearized polynomial.
         *  To see this, consider any monomial x^{2^i}.
         *  Its formal derivative is 2^i x^{(2^i) - 1},
         *  which for i != 0 is zero, as we are in a binary field.
         *  Consequently the formal derivative is just the linear coefficient */
        return this->linearized_polynomial_.coefficients()[1];
    }
    return FieldT::zero();
}

template<typename FieldT>
std::vector<FieldT> vanishing_polynomial<FieldT>::unique_evaluations_over_field_subset(const field_subset<FieldT> &S) const {
    assert(S.num_elements() % this->vp_degree_ == 0);
    field_subset<FieldT> unique_domain = this->associated_k_to_1_map_at_domain(S);
    std::vector<FieldT> evals = unique_domain.all_elements();
    // In the additive case, the associated k to 1 map is the vanishing polynomial,
    // so the unique domain's evaluations is {Z_H(x) | x in S}
    // In the multiplicative case, the associated k to 1 map is Z_H(x) + h_offset^|H|
    // Hence te unique domain's evaluations are:
    //   {Z_H(x) + h_offset^|H| | x in S}
    // So we subtract h^|H| from all evals. h^|H| is the vp offset
    if (S.type() == multiplicative_coset_type)
    {
        for (size_t i = 0; i < evals.size(); i++)
        {
            evals[i] -= this->vp_offset_;
        }
    }
    return evals;
}

template<typename FieldT>
std::vector<FieldT> vanishing_polynomial<FieldT>::evaluations_over_field_subset(const field_subset<FieldT> &S) const {
    if (S.type() == affine_subspace_type) {
        return this->evaluations_over_subspace(S.subspace());
    } else if (S.type() == multiplicative_coset_type) {
        return this->evaluations_over_coset(S.coset());
    }
    throw std::invalid_argument("field_subset type unsupported");
}

template<typename FieldT>
std::vector<FieldT> vanishing_polynomial<FieldT>::evaluations_over_subspace(const affine_subspace<FieldT> &S) const {
    if (this->type_ != affine_subspace_type) {
        throw std::invalid_argument("evaluations_over_subspace can only be used on subspace vanishing polynomials.");
    }
    return this->linearized_polynomial_.evaluations_over_subspace(S);
}

template<typename FieldT>
std::vector<FieldT> vanishing_polynomial<FieldT>::evaluations_over_coset(const multiplicative_coset<FieldT> &S) const {
    if (this->type_ != multiplicative_coset_type) {
        throw std::invalid_argument("evaluations_over_coset can only be used on multiplicative_coset vanishing polynomials.");
    }
    // P is of the form X^|G| - vp_offset
    const std::size_t order_s = S.num_elements();
    const std::size_t order_g = this->vp_degree_;
    // points in S are of the form hg^i, where h is the shift of the coset, and g is its generator.
    // We cache h^|G|
    const FieldT shift_to_order_g = libiop::power(S.shift(), order_g);
    std::vector<FieldT> evals;
    evals.reserve(order_s);
    if (order_g % order_s == 0)
    {
        // In this case |S| <= |G|, and |G| % |S| = 0.
        // Therefore g^{i|G|} = 1, consequently
        // P(s) = h^|G| - vp_offset, for all s \in S
        evals.resize(order_s, shift_to_order_g - this->vp_offset_);
        return evals;
    }
    size_t evaluation_repetitions = 1;
    size_t number_of_distinct_evaluations = order_s;
    if (order_s % order_g == 0)
    {
        // In this case |G| divides |S|,
        // therefore X^|G| is a homomorphism from S to a subgroup of order |S| / |G|.
        // Since P = x^|G| - 1, and since 1 is independent of X, it follows that there are
        // only |S| / |G| distinct evaluations, and these repeat.
        // TODO: (low priority as none of our protocols need this)
        // The number of distinct evaluations is order_s / GCD(order_s, order_g)
        number_of_distinct_evaluations = order_s / order_g;
        evaluation_repetitions = order_g;
    }
    const FieldT generator_to_order_g = libiop::power(S.generator(), order_g);
    FieldT cur = shift_to_order_g;
    for (std::size_t i = 0; i < number_of_distinct_evaluations; i++)
    {
        evals.emplace_back(cur - this->vp_offset_);
        cur = cur * generator_to_order_g;
    }
    // Place these distinct evaluations in the remaining locations.
    for (std::size_t i = 1; i < evaluation_repetitions; i++) {
        for (std::size_t j = 0; j < number_of_distinct_evaluations; j++) {
            evals.emplace_back(evals[j]);
        }
    }
    return evals;
}

template<typename FieldT>
std::size_t vanishing_polynomial<FieldT>::degree() const {
    return this->vp_degree_;
}

template<typename FieldT>
FieldT vanishing_polynomial<FieldT>::constant_coefficient() const {
    if (this->type() == affine_subspace_type) {
        return this->linearized_polynomial_.constant_coefficient();
    }
    // subgroup / coset type
    return -this->vp_offset_;
}

template<typename FieldT>
std::shared_ptr<polynomial_base<FieldT>>
    vanishing_polynomial<FieldT>::associated_k_to_1_map()
{
    if (this->type() == affine_subspace_type)
    {
        linearized_polynomial<FieldT> copy = this->linearized_polynomial_;
        return std::make_shared<linearized_polynomial<FieldT>>(copy);
    }
    else if (this->type_ == multiplicative_coset_type)
    {
        /** This returns the polynomial x^{|H|}.
         *  It does this by altering vp_offset, making a copy of this vanishing polynomial,
         *  restoring the previous vp_offset, and returning the copy. */
        const FieldT vp_offset_copy = this->vp_offset_;
        this->vp_offset_ = FieldT::zero();
        std::shared_ptr<polynomial_base<FieldT>> copy =
            std::static_pointer_cast<polynomial_base<FieldT>>(std::make_shared<vanishing_polynomial<FieldT>>(*this));
        this->vp_offset_ = vp_offset_copy;
        return copy;
    }
    throw std::logic_error("should not happen");
}

template<typename FieldT>
field_subset<FieldT> vanishing_polynomial<FieldT>::associated_k_to_1_map_at_domain(const field_subset<FieldT> domain) const
{
    if (domain.type() != this->type())
    {
        throw std::invalid_argument("domain type doesn't match vp domain type");
    }
    vanishing_polynomial<FieldT> copy = *this;
    std::shared_ptr<polynomial_base<FieldT>> k_to_1_map = copy.associated_k_to_1_map();
    if (this->type() == affine_subspace_type)
    {
        /** The vanishing polynomial is a k to 1 map for the input subspace, not the whole domain.
         *  Consequently we have to evaluate the vanishing polynomial at each basis vector,
         *  and then remove any duplicates or zero values.
        */
        const std::vector<FieldT> domain_basis = domain.basis();
        const std::vector<FieldT> transformed_basis =
            transform_basis_by_polynomial<FieldT>(k_to_1_map, domain_basis);
        /** Now we remove duplicates from the transformed basis.
         *  Currently there is no FieldT::hash, so this does naive duplicate removal, and is log(n)^2 */
        std::vector<FieldT> returned_basis;
        for (size_t i = 0; i < transformed_basis.size(); i++)
        {
            bool is_dup = false;
            /** The basis element was in the kernel of the vanishing polynomial. */
            if (transformed_basis[i] == FieldT::zero())
            {
                continue;
            }
            for (size_t j = 0; j < returned_basis.size(); j++)
            {
                if (returned_basis[j] == transformed_basis[i])
                {
                    is_dup = true;
                    break;
                }
            }
            if (!is_dup)
            {
                returned_basis.emplace_back(transformed_basis[i]);
            }
        }
        const FieldT transformed_offset = k_to_1_map->evaluation_at_point(domain.offset());
        return field_subset<FieldT>(affine_subspace<FieldT>(returned_basis, transformed_offset));
    }
    else if (this->type_ == multiplicative_coset_type)
    {
        const FieldT new_shift = k_to_1_map->evaluation_at_point(domain.shift());
        /** The multiplicative vanishing polynomial with no offset is a
         *  k to 1 map over any domain of order divisible by k. */
        if (domain.num_elements() % this->vp_degree_ == 0)
        {
            const size_t new_domain_size = domain.num_elements() / this->vp_degree_;
            return field_subset<FieldT>(new_domain_size, new_shift);
        }
        else if (gcd(domain.num_elements(), this->vp_degree_) == 1)
        {
            const FieldT new_generator = libiop::power(domain.generator(), this->vp_degree_);
            return field_subset<FieldT>(multiplicative_coset<FieldT>(domain.num_elements(), new_shift, new_generator));
        }
        throw std::invalid_argument("We currently don't implement associated_k_to_1_map_of_domain(domain)"
            "when gcd(domain.num_elements(), vp_degree) is not in {1, vp_degree}");
    }
    throw std::logic_error("should not happen");
}

template<typename FieldT>
polynomial<FieldT> vanishing_polynomial<FieldT>::operator*(const polynomial<FieldT> &p) const
{
    if (this->type_ == affine_subspace_type) {
        return this->linearized_polynomial_ * p;
    }
    // in the multiplicative case just shift p, and subtract by p * this->vp_offset_
    std::vector<FieldT> result(p.degree() + this->vp_degree_ + 1, FieldT(0));
    const std::vector<FieldT> p_coeff = p.coefficients();
    add_scalar_multiple_at_offset(result, p_coeff, FieldT(1), this->vp_degree_);
    add_scalar_multiple_at_offset(result, p_coeff, FieldT(0) - this->vp_offset_, 0);
    return polynomial<FieldT>(std::move(result));
}

template<typename FieldT>
linearized_polynomial<FieldT> vanishing_polynomial<FieldT>::get_linearized_polynomial() const {
    if (this->type_ == multiplicative_coset_type) {
        throw std::invalid_argument(
            "linearized polynomials can't be constructed for multiplicative vanishing polynomials");
    }
    return this->linearized_polynomial_;
}

template<typename FieldT>
polynomial<FieldT> vanishing_polynomial<FieldT>::get_polynomial() const {
    if (this->type_ == affine_subspace_type) {
        return this->linearized_polynomial_.expand_as_polynomial();
    }
    polynomial<FieldT> poly;
    poly.set_degree(this->vp_degree_);
    poly[0] = FieldT(-1);
    poly[this->vp_degree_] = FieldT(1);
    return poly;
}

template<typename FieldT>
field_subset_type vanishing_polynomial<FieldT>::type() const {
    return this->type_;
}

/* Returns P / Z as a pair.
   The first element in the pair is the quotient, the latter is the remainder. */
template<typename FieldT>
std::pair<polynomial<FieldT>,
          polynomial<FieldT> >
polynomial_over_multiplicative_vanishing_polynomial(const polynomial<FieldT> &P,
                                              const FieldT vp_offset,
                                              const size_t vp_degree)
{
    /* inverse of the leading term */
    const FieldT linv = FieldT::one().inverse();

    if (P.degree() < vp_degree)
    {
        polynomial<FieldT> quotient;
        std::vector<FieldT> coeff_copy = P.coefficients();
        polynomial<FieldT> remainder(std::move(coeff_copy));
        remainder.set_degree(vp_degree-1);

        return std::make_pair(std::move(quotient), std::move(remainder));
    }

    std::vector<FieldT> quotient(P.coefficients().begin() + vp_degree,
                          P.coefficients().end());
    std::vector<FieldT> remainder(P.coefficients().begin(),
                          P.coefficients().begin() + vp_degree);

    FieldT Z_0 = vp_offset;
    for (std::size_t i = quotient.size(); i--; )
    {
        // Z only has 2 terms, the leading term and the constant term.
        const FieldT twist = quotient[i] * linv;
        // correct the quotient for the leading term
        quotient[i] = twist;

        /* Handle the remainder by subtracting twist*Z[0] * y^i,
         * thus clearing the i-th term of P */
        if (i < vp_degree)
        {
            remainder[i] -= twist * Z_0;
        }
        else
        {
            quotient[i - vp_degree] -= twist * Z_0;
        }
    }

    return std::make_pair(std::move(polynomial<FieldT>(std::move(quotient))),
                          std::move(polynomial<FieldT>(std::move(remainder))));
}

template<typename FieldT>
std::pair<polynomial<FieldT>,
    polynomial<FieldT> >
polynomial_over_vanishing_polynomial(const polynomial<FieldT> &f,
                                     const vanishing_polynomial<FieldT> &Z)
{
    if (Z.type() == affine_subspace_type) {
        return polynomial_over_linearized_polynomial(f, Z.get_linearized_polynomial());
    } else {
        return polynomial_over_multiplicative_vanishing_polynomial(f, Z.constant_coefficient(), Z.degree());
    }
};

template<typename FieldT>
linearized_polynomial<FieldT> vanishing_polynomial_from_subspace(const affine_subspace<FieldT> &S)
{
    /* Vanishing polynomial for empty subspace is just y */
    linearized_polynomial<FieldT> poly({ FieldT(0), FieldT(1) });

    for (const FieldT &c : S.basis())
    {
        /* Note that since we are in a binary field,
          Z_{<S_1,...,S_k>}(y)
            = Z_{<S_1,...,S_{k-1}>}(y) * Z_{<S_1,...,S_{k-1}>}(y+S_k)
          By linearity, this is equivalent to:
            = Z_{<S_1,...,S_{k-1}>}(y)^2 +
                (Z_{<S_1,...,S_{k-1}>}(y)*Z_{<S_1,...,S_{k-1}>}(S_k)) */
        const FieldT poly_c = poly.evaluation_at_point(c);
        poly = poly.squared() + (poly * poly_c);
    }

    const FieldT poly_shift = poly.evaluation_at_point(S.offset());
    poly[0] += poly_shift;

    return poly;
}

} // namespace libiop
