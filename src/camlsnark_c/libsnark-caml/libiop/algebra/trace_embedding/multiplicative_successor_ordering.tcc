namespace libiop {

template<typename FieldT>
multiplicative_successor_polynomial<FieldT>::multiplicative_successor_polynomial(const FieldT &generator) :
    generator_(generator),
    composed_(false)
{
}

template<typename FieldT>
multiplicative_successor_polynomial<FieldT>::multiplicative_successor_polynomial(const FieldT &generator,
        const std::shared_ptr<polynomial_base<FieldT>> composed_poly) :
    generator_(generator),
    composed_(true),
    composed_poly_(composed_poly)
{
}

template<typename FieldT>
FieldT multiplicative_successor_polynomial<FieldT>::evaluation_at_point(const FieldT &evalpoint) const
{
    const FieldT value = this->generator_ * evalpoint;
    if (this->composed_)
    {
        return this->composed_poly_->evaluation_at_point(value);
    }
    return value;
}

template<typename FieldT>
std::vector<FieldT> multiplicative_successor_polynomial<FieldT>::evaluations_over_field_subset(
    const field_subset<FieldT> &S) const
{
    /** Note that the evaluations of the successor polynomial are all of elements
     *  of S, with an additional shift of g.
    */
    field_subset<FieldT> S_shifted(S.num_elements(), S.shift() * this->generator_);
    if (this->composed_)
    {
        return this->composed_poly_->evaluations_over_field_subset(S_shifted);
    }
    return S_shifted.all_elements();
}

template<typename FieldT>
polynomial<FieldT> multiplicative_successor_polynomial<FieldT>::expand_as_polynomial() const
{
    return polynomial<FieldT>({this->generator_, FieldT::zero()});
}

template<typename FieldT>
size_t multiplicative_successor_polynomial<FieldT>::degree() const
{
    if (this->composed_)
    {
        return this->composed_poly_->degree();
    }
    return 1;
}

template<typename FieldT>
size_t multiplicative_successor_polynomial<FieldT>::piecewise_degree() const
{
    return 1;
}

template<typename FieldT>
std::shared_ptr<piecewise_polynomial_base<FieldT>> multiplicative_successor_polynomial<FieldT>::compose(
        const std::shared_ptr<polynomial_base<FieldT>> poly) const
{
    return std::make_shared<multiplicative_successor_polynomial<FieldT>>(
        multiplicative_successor_polynomial<FieldT>(this->generator_, poly));
}

template<typename FieldT>
multiplicative_successor_ordering<FieldT>::multiplicative_successor_ordering(const field_subset<FieldT> &domain)
{
    if (domain.type() != multiplicative_coset_type)
    {
        throw std::invalid_argument(
            "multiplicative successor ordering was instantiated with a subset that is not a multiplicative subgroup.");
    }
    this->domain_ = domain.coset();
    this->successor_polynomial_ = multiplicative_successor_polynomial<FieldT>(this->domain_.generator());
}

template<typename FieldT>
FieldT multiplicative_successor_ordering<FieldT>::first_elem() const
{
    return this->domain_.shift();
}

template<typename FieldT>
FieldT multiplicative_successor_ordering<FieldT>::next_elem(const FieldT &cur_elem) const
{
    return this->successor_polynomial_.evaluation_at_point(cur_elem);
}

template<typename FieldT>
std::shared_ptr<piecewise_polynomial_base<FieldT>>
    multiplicative_successor_ordering<FieldT>::piecewise_polynomial() const
{
    return std::make_shared<multiplicative_successor_polynomial<FieldT>>(this->successor_polynomial_);
}

} // namespace libiop
