#include "libiop/algebra/polynomials/polynomial.hpp"

namespace libiop {

template<typename FieldT>
identity_matrix<FieldT>::identity_matrix(const size_t num_rows) :
    num_rows_(num_rows)
{
}

template<typename FieldT>
size_t identity_matrix<FieldT>::num_rows() const
{
    return this->num_rows_;
}

template<typename FieldT>
size_t identity_matrix<FieldT>::num_columns() const
{
    return this->num_rows_;
}

template<typename FieldT>
std::shared_ptr<polynomial_base<FieldT>> identity_matrix<FieldT>::extend_Mz(
    const std::shared_ptr<polynomial_base<FieldT>> &z) const
{
    return z;
}

template<typename FieldT>
size_t identity_matrix<FieldT>::Mz_degree(const size_t d) const
{
    return d;
}

template<typename FieldT>
class shifted_identity_extended_polynomial : public polynomial_base<FieldT> {
protected:
    FieldT z_at_second_elem_;
    /** TODO: When optimizing succinct aurora performance,
     *  there may be a way to cache evaluations of this
     *  and its evaluation within the additive successor ordering. */
    lagrange_polynomial<FieldT> first_elem_indicator_;
    std::shared_ptr<piecewise_polynomial_base<FieldT>> composed_poly_;
public:
    shifted_identity_extended_polynomial(
        const field_subset<FieldT> &S,
        const successor_ordering<FieldT> &ordering,
        const std::shared_ptr<polynomial_base<FieldT>> &z)
    {
        const FieldT second_elem = ordering.next_elem(ordering.first_elem());
        this->z_at_second_elem_ = z->evaluation_at_point(second_elem);

        this->first_elem_indicator_ = lagrange_polynomial<FieldT>(ordering.first_elem(), S);
        this->composed_poly_ = ordering.piecewise_polynomial()->compose(z);
    }

    FieldT evaluation_at_point(const FieldT &x) const
    {
        /** z(successor(x)) - z(successor(1)) * lagrange_indicator(x) */
        return this->composed_poly_->evaluation_at_point(x)
            - this->z_at_second_elem_ * this->first_elem_indicator_.evaluation_at_point(x);
    }

    std::vector<FieldT> evaluations_over_field_subset(const field_subset<FieldT> &U) const
    {
        /** z(successor(x)) - z(successor(1)) * lagrange_indicator(x) */
        std::vector<FieldT> result = this->composed_poly_->evaluations_over_field_subset(U);
        const std::vector<FieldT> lagrange_indicator_evals =
            this->first_elem_indicator_.evaluations_over_field_subset(U);
        for (size_t i = 0; i < result.size(); i++)
        {
            result[i] -= this->z_at_second_elem_ * lagrange_indicator_evals[i];
        }
        return result;
    }

    std::size_t degree() const
    {
        return std::max<size_t>(
            this->composed_poly_->degree(), this->first_elem_indicator_.degree());
    }
};

template<typename FieldT>
shifted_identity_matrix<FieldT>::shifted_identity_matrix(
    const field_subset<FieldT> &S, const successor_ordering<FieldT> &ordering) :
    S_(S),
    successor_ordering_(ordering)
{
}

template<typename FieldT>
size_t shifted_identity_matrix<FieldT>::num_rows() const
{
    return this->S_.num_elements();
}

template<typename FieldT>
size_t shifted_identity_matrix<FieldT>::num_columns() const
{
    return this->S_.num_elements();
}

template<typename FieldT>
std::shared_ptr<polynomial_base<FieldT>> shifted_identity_matrix<FieldT>::extend_Mz(
    const std::shared_ptr<polynomial_base<FieldT>> &z) const
{
    return std::make_shared<shifted_identity_extended_polynomial<FieldT>>
        (this->S_, this->successor_ordering_, z);
}

template<typename FieldT>
size_t shifted_identity_matrix<FieldT>::Mz_degree(const size_t d) const
{
    return std::max<size_t>(this->S_.num_elements() - 1, d);
}

} // libiop