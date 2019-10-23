namespace libiop {

template<typename FieldT>
successor_ordering<FieldT>::successor_ordering(const field_subset<FieldT> &domain)
{
    this->field_subset_type_ = domain.type();
    if (this->field_subset_type_ == affine_subspace_type)
    {
        this->additive_ordering_ = additive_successor_ordering<FieldT>(domain);
    }
    else if (this->field_subset_type_ == multiplicative_coset_type)
    {
        this->multiplicative_ordering_ = multiplicative_successor_ordering<FieldT>(domain);
    }
    else
    {
        throw std::invalid_argument("domain is not an affine subspace or multiplicative coset type");
    }
}

template<typename FieldT>
FieldT successor_ordering<FieldT>::first_elem() const
{
    if (this->field_subset_type_ == affine_subspace_type)
    {
        return this->additive_ordering_.first_elem();
    }
    else if (this->field_subset_type_ == multiplicative_coset_type)
    {
        return this->multiplicative_ordering_.first_elem();
    }
    assert(false);
    return FieldT(0);
}

template<typename FieldT>
FieldT successor_ordering<FieldT>::next_elem(const FieldT &cur_elem) const
{
    if (this->field_subset_type_ == affine_subspace_type)
    {
        return this->additive_ordering_.next_elem(cur_elem);
    }
    else if (this->field_subset_type_ == multiplicative_coset_type)
    {
        return this->multiplicative_ordering_.next_elem(cur_elem);
    }
    assert(false);
    return FieldT(0);
}

template<typename FieldT>
std::shared_ptr<piecewise_polynomial_base<FieldT>>
    successor_ordering<FieldT>::piecewise_polynomial() const
{
    if (this->field_subset_type_ == affine_subspace_type)
    {
        return this->additive_ordering_.piecewise_polynomial();
    }
    else if (this->field_subset_type_ == multiplicative_coset_type)
    {
        return this->multiplicative_ordering_.piecewise_polynomial();
    }
    assert(false);
    // Just need something to make it compile here.
    return this->multiplicative_ordering_.piecewise_polynomial();
}

} // namespace libiop
