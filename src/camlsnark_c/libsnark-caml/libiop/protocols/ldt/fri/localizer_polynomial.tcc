namespace libiop {

template<typename FieldT>
localizer_polynomial<FieldT>::localizer_polynomial(const field_subset<FieldT> domain) :
    type_(domain.type()),
    degree_(domain.num_elements())
{
    if (this->type_ == affine_subspace_type)
    {
        vanishing_polynomial<FieldT> vp(domain);
        this->additive_poly_ = vp.get_linearized_polynomial();
    }
}

template<typename FieldT>
FieldT localizer_polynomial<FieldT>::evaluation_at_point(const FieldT &eval_point) const
{
    if (this->type_ == affine_subspace_type)
    {
        return this->additive_poly_.evaluation_at_point(eval_point);
    }
    else if (this->type_ == multiplicative_coset_type)
    {
        return libiop::power(eval_point, this->degree_);
    }
    return FieldT::zero();
}

template<typename FieldT>
linearized_polynomial<FieldT> localizer_polynomial<FieldT>::get_linearized_polynomial() const
{
    return this->additive_poly_;
}

} // namespace libiop
