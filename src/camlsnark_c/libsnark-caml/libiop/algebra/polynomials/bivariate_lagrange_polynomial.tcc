namespace libiop {

template<typename FieldT>
bivariate_lagrange_polynomial<FieldT>::bivariate_lagrange_polynomial(const field_subset<FieldT> &S) :
    S_(S),
    Z_S_(vanishing_polynomial<FieldT>(S))
{
}

template<typename FieldT>
FieldT bivariate_lagrange_polynomial<FieldT>::evaluation_at_point(const FieldT &x, const FieldT &y) const
{
    /** f(x, y) = (Z_S(x) - Z_S(y)) / (x - y)
     *  The evaluation point is y, but the above formula must be special cased when x = y
     *  In that case, f(x, x) is the formal derivative of Z_S, evaluated at X. */
    if (x == y)
    {
        return this->Z_S_.formal_derivative_at_point(x);
    }
    const FieldT denominator = x - y;
    /** TODO: If we need to optimize this, in the additive case, by linearity numerator = Z_S(x - y) */
    const FieldT numerator = this->Z_S_.evaluation_at_point(x) - this->Z_S_.evaluation_at_point(y);
    return numerator * denominator.inverse();
}

template<typename FieldT>
lagrange_polynomial<FieldT> bivariate_lagrange_polynomial<FieldT>::fix_x(const FieldT &x) const
{
    const bool is_normalized = false;
    return lagrange_polynomial<FieldT>(x, this->S_, is_normalized);
}

template<typename FieldT>
std::vector<FieldT> bivariate_lagrange_polynomial<FieldT>::evaluations_over_field_subset(
    const FieldT &x, const field_subset<FieldT> &evaldomain) const
{
    return this->fix_x(x).evaluations_over_field_subset(evaldomain);
}

} // namespace libiop
