namespace libiop {

template<typename FieldT>
std::vector<FieldT> monomial_basis(const size_t dimension, const size_t smallest_exponent)
{
    assert(smallest_exponent + dimension < 64);
    std::vector<FieldT> basis;
    basis.reserve(dimension);
    for (size_t i = 0; i < dimension; ++i)
    {
        basis.emplace_back(FieldT(1ull<<(smallest_exponent + i)));
    }
    return basis;
}

template<typename FieldT>
std::vector<FieldT> transform_basis_by_polynomial(
    const std::shared_ptr<polynomial_base<FieldT>> transform, const std::vector<FieldT> basis)
{
    std::vector<FieldT> transformed;
    for (size_t i = 0; i < basis.size(); i++)
    {
        transformed.emplace_back(transform->evaluation_at_point(basis[i]));
    }
    return transformed;
}

} // namespace libiop
