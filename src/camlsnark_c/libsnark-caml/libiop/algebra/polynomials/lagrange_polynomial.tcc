namespace libiop {

template<typename FieldT>
lagrange_polynomial<FieldT>::lagrange_polynomial(
    const FieldT x_evaluation,
    const field_subset<FieldT> &S) :
    x_(x_evaluation),
    S_(S),
    Z_S_(vanishing_polynomial<FieldT>(S)),
    is_normalized_(true)
{
    this->Z_S_at_x_ = this->Z_S_.evaluation_at_point(this->x_);
    this->normalization_coefficient_ = this->Z_S_.formal_derivative_at_point(this->x_).inverse();
}

template<typename FieldT>
lagrange_polynomial<FieldT>::lagrange_polynomial(
    const FieldT x_evaluation,
    const field_subset<FieldT> &S,
    const bool is_normalized) :
    x_(x_evaluation),
    S_(S),
    Z_S_(vanishing_polynomial<FieldT>(S)),
    is_normalized_(is_normalized)
{
    this->Z_S_at_x_ = this->Z_S_.evaluation_at_point(this->x_);
    if (is_normalized)
    {
        this->normalization_coefficient_ = this->Z_S_.formal_derivative_at_point(this->x_).inverse();
    }
    else
    {
        this->normalization_coefficient_ = FieldT::one();
    }
}

template<typename FieldT>
FieldT lagrange_polynomial<FieldT>::evaluation_at_point(const FieldT &evalpoint) const
{
    /** f(x, y) = (Z_S(x) - Z_S(y)) / (x - y)
     *  The evaluation point is y, but the above formula must be special cased when x = y
     *  In that case, f(x, x) is the formal derivative of Z_S, evaluated at X. */
    if (this->x_ == evalpoint)
    {
        // Normalization makes this one
        if (this->is_normalized_)
        {
            return FieldT::one();
        }
        else
        {
            return this->Z_S_.formal_derivative_at_point(this->x_);
        }
    }
    const FieldT denominator = this->x_ - evalpoint;
    const FieldT numerator = this->Z_S_at_x_ - this->Z_S_.evaluation_at_point(evalpoint);
    if (this->is_normalized_)
    {
        return numerator * denominator.inverse() * this->normalization_coefficient_;
    }
    return numerator * denominator.inverse();
}

template<typename FieldT>
std::vector<FieldT> lagrange_polynomial<FieldT>::evaluations_over_field_subset(
    const field_subset<FieldT> &evaldomain) const
{
    /** f(x, y) = (Z_S(x) - Z_S(y)) / (x - y) */
    std::vector<FieldT> denominator;
    /** First we create the denominator.
     *  The additive case admits a faster method to create it, hence the separation */
    if (this->S_.type() == affine_subspace_type)
    {
        denominator = all_subset_sums<FieldT>(evaldomain.basis(), this->x_ + evaldomain.offset());
    }
    else if (this->S_.type() == multiplicative_coset_type)
    {
        denominator = evaldomain.all_elements();
        for (size_t i = 0; i < denominator.size(); i++)
        {
            denominator[i] = this->x_ - denominator[i];
        }
    }
    // If x is in the evaluation domain, then the denominator is 0 at some point,
    // which requires special casing.
    // We determine if x is in the evaluation domain in logarithmic time, and
    // if it is, we do a linear search to find this position.
    const bool x_in_evaldomain = evaldomain.element_in_subset(this->x_);
    size_t zero_denominator_position = 0;
    if (x_in_evaldomain)
    {
        FieldT zero = FieldT::zero();
        for (size_t i = 0; i < denominator.size(); i++)
        {
            if (denominator[i] == zero)
            {
                // Change the zero to a one, so we can avoid zero searching in batch inversion
                denominator[i] = FieldT::one();
                zero_denominator_position = i;
                break;
            }
        }
    }
    if (evaldomain == this->S_)
    {
        // In this case, Z_S(y) = 0
        // The conditional is written like this to avoid an additional copy when x not in evaldomain.
        // This optimization is useful, as this logic runs in the Aurora verifier.
        if (x_in_evaldomain)
        {
            std::vector<FieldT> result =
                batch_inverse_and_mul(denominator, this->Z_S_at_x_ * this->normalization_coefficient_);
            result[zero_denominator_position] = this->evaluation_at_point(this->x_);
            return result;
        }
        return batch_inverse_and_mul(denominator, this->Z_S_at_x_  * this->normalization_coefficient_);
    }
    denominator = batch_inverse_and_mul(denominator, this->normalization_coefficient_);
    std::vector<FieldT> result = this->Z_S_.evaluations_over_field_subset(evaldomain);
    /** At this point, denominator[i] = 1 / (x - evaldomain[i]),
     *  and result[i] is Z_S(Y) */
    for (size_t i = 0; i < result.size(); i++)
    {
        /** result[i] = (Z_S(x) - Z_S(evaldomain[i])) * (x - evaldomain[i])^{-1}*/
        result[i] = (this->Z_S_at_x_ - result[i]) * denominator[i];
    }
    // Correct the evaluation for the position where y = x
    if (x_in_evaldomain)
    {
        result[zero_denominator_position] = this->evaluation_at_point(this->x_);
    }
    return result;
}

template<typename FieldT>
polynomial<FieldT> lagrange_polynomial<FieldT>::expand_as_polynomial() const
{
    const std::vector<FieldT> evals = this->evaluations_over_field_subset(this->S_);
    return polynomial<FieldT>(IFFT_over_field_subset(evals, this->S_));
}

template<typename FieldT>
size_t lagrange_polynomial<FieldT>::degree() const
{
    return this->S_.num_elements() - 1;
}


} // namespace libiop
