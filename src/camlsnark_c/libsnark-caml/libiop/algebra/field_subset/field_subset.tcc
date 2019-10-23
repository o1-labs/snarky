namespace libiop {

template<typename FieldT>
field_subset<FieldT>::field_subset(const std::size_t num_elements)
{
    if (is_multiplicative<FieldT>::value) {
        this->construct_internal(num_elements, FieldT::one());
    } else {
        this->construct_internal(num_elements, FieldT::zero());
    }
}

template<typename FieldT>
field_subset<FieldT>::field_subset(const std::size_t num_elements,
                                   const FieldT coset_shift)
{
    this->construct_internal(num_elements, coset_shift);
}

template<typename FieldT>
field_subset<FieldT>::field_subset(const affine_subspace<FieldT> subspace) :
    subspace_(std::make_shared<affine_subspace<FieldT>>(subspace)),
    type_(affine_subspace_type)
{ }

template<typename FieldT>
field_subset<FieldT>::field_subset(const multiplicative_coset<FieldT> coset) :
    coset_(std::make_shared<multiplicative_coset<FieldT>>(coset)),
    type_(multiplicative_coset_type)
{ }

template<typename FieldT>
void field_subset<FieldT>::construct_internal(const std::size_t num_elements,
                                              const typename libiop::enable_if<is_multiplicative<FieldT>::value, FieldT>::type coset_shift)
{
    if (coset_shift == FieldT::zero()) {
        throw std::invalid_argument("coset_shift was supplied as 0, it was likely intended to be 1");
    }
    this->coset_ = std::make_shared<multiplicative_coset<FieldT>>(
        multiplicative_coset<FieldT>(num_elements, coset_shift));
    this->type_ = multiplicative_coset_type;
}

template<typename FieldT>
void field_subset<FieldT>::construct_internal(const std::size_t num_elements,
                                              const typename libiop::enable_if<is_additive<FieldT>::value, FieldT>::type coset_shift)
{
    assert(is_power_of_2(num_elements));
    const std::size_t dimension = log2(num_elements);

    if (coset_shift != FieldT(0))
    {
        this->subspace_ = std::make_shared<affine_subspace<FieldT>>(
            affine_subspace<FieldT>::shifted_standard_basis(dimension, coset_shift));
    }
    else
    {
        this->subspace_ = std::make_shared<affine_subspace<FieldT>>(
            linear_subspace<FieldT>::standard_basis(dimension));
    }
    this->type_ = affine_subspace_type;
}

template<typename FieldT>
field_subset_type field_subset<FieldT>::type() const
{
    return this->type_;
}

template<typename FieldT>
std::size_t field_subset<FieldT>::dimension() const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->dimension();
        case multiplicative_coset_type:
            return this->coset_->dimension();
        default:
            return -1;
    }
}

template<typename FieldT>
std::size_t field_subset<FieldT>::num_elements() const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->num_elements();
        case multiplicative_coset_type:
            return this->coset_->num_elements();
        default:
            return -1;
    }
}

template<typename FieldT>
std::vector<FieldT> field_subset<FieldT>::all_elements() const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->all_elements();
        case multiplicative_coset_type:
            return this->coset_->all_elements();
        default:
            return std::vector<FieldT>();
    }
}

template<typename FieldT>
FieldT field_subset<FieldT>::element_by_index(const std::size_t index) const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->element_by_index(index);
        case multiplicative_coset_type:
            return this->coset_->element_by_index(index);
        default:
            return FieldT::zero();
    }
}

/** Given an index which assumes this field subset begins with another subset of dim reindex_subset_dim,
 *  returns the actual index into this subset. In the additive case, this assumes that the subspaces
 *  share the first reindex_subset_dim basis vectors. */
template<typename FieldT>
std::size_t field_subset<FieldT>::reindex_by_subset(const std::size_t reindex_subset_dim,
                                                    const std::size_t index) const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return index;
        case multiplicative_coset_type:
            return this->coset_->reindex_by_subgroup(reindex_subset_dim, index);
        default:
            return 0;
    }
}

template<typename FieldT>
size_t field_subset<FieldT>::coset_index(const std::size_t position, const std::size_t coset_size) const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->coset_index(position, coset_size);
        case multiplicative_coset_type:
            return this->coset_->coset_index(position, coset_size);
        default:
            return 0;
    }
}

template<typename FieldT>
size_t field_subset<FieldT>::intra_coset_index(const std::size_t position, const std::size_t coset_size) const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->intra_coset_index(position, coset_size);
        case multiplicative_coset_type:
            return this->coset_->intra_coset_index(position, coset_size);
        default:
            return 0;
    }
}

template<typename FieldT>
size_t field_subset<FieldT>::position_by_coset_indices(
    const size_t coset_index, const size_t intra_coset_index, const size_t coset_size) const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->position_by_coset_indices(coset_index, intra_coset_index, coset_size);
        case multiplicative_coset_type:
            return this->coset_->position_by_coset_indices(coset_index, intra_coset_index, coset_size);
        default:
            return 0;
    }
}

template<typename FieldT>
std::vector<size_t> field_subset<FieldT>::all_positions_in_coset_i(
    const size_t coset_index, const size_t coset_size) const
{
    /* TODO: If we need more performance we can make direct methods for these. */
    std::vector<size_t> positions;
    for (size_t i = 0; i < coset_size; i++)
    {
        positions.emplace_back(this->position_by_coset_indices(coset_index, i, coset_size));
    }
    return positions;
}

template<typename FieldT>
std::vector<size_t> field_subset<FieldT>::all_positions_with_intra_coset_index_i(
    const size_t intra_coset_index, const size_t coset_size) const
{
    /* TODO: If we need more performance we can make direct methods for these. */
    std::vector<size_t> positions;
    size_t num_cosets = this->num_elements() / coset_size;
    for (size_t i = 0; i < num_cosets; i++)
    {
        positions.emplace_back(this->position_by_coset_indices(i, intra_coset_index, coset_size));
    }
    return positions;
}

/** Returns a subset of this field subset, with the provided order.
 *  In the additive case, it returns the affine subspace with the same shift,
 *  and the first log_2(order) basis vectors of this subset. */
template<typename FieldT>
field_subset<FieldT> field_subset<FieldT>::get_subset_of_order(const std::size_t order) const
{
    switch (this->type_)
    {
        case affine_subspace_type:
        {
            const std::size_t subset_dim = log2(order);
            std::vector<FieldT> input_subspace_basis = this->subspace_->basis();
            input_subspace_basis.resize(subset_dim);
            return field_subset<FieldT>(
                    affine_subspace<FieldT>(input_subspace_basis,
                                            this->offset()));
        }
        case multiplicative_coset_type:
            // Assumes this subgroup's generator is the default generator for a subgroup of that order.
            return field_subset<FieldT>(order, this->shift());
        default:
            return 0;
    }
}

template<typename FieldT>
FieldT field_subset<FieldT>::element_outside_of_subset() const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->element_outside_of_subset();
        case multiplicative_coset_type:
            return this->coset_->element_outside_of_subset();
        default:
            return FieldT::zero();
    }
}

template<typename FieldT>
bool field_subset<FieldT>::element_in_subset(const FieldT x) const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_->element_in_subset(x);
        case multiplicative_coset_type:
            return this->coset_->element_in_subset(x);
        default:
            return false;
    }
}

template<typename FieldT>
affine_subspace<FieldT> field_subset<FieldT>::subspace() const
{
    assert(this->type_ == affine_subspace_type);

    return *(this->subspace_);
}

template<typename FieldT>
multiplicative_coset<FieldT> field_subset<FieldT>::coset() const
{
    assert(this->type_ == multiplicative_coset_type);

    return *(this->coset_);
}

template<typename FieldT>
FieldT field_subset<FieldT>::generator() const
{
    assert(this->type_ == multiplicative_coset_type);

    return this->coset_->generator();
}

template<typename FieldT>
const FieldT& field_subset<FieldT>::offset() const
{
    assert(this->type_ == affine_subspace_type);

    return this->subspace_->offset();
}

template<typename FieldT>
const FieldT field_subset<FieldT>::shift() const
{
    assert(this->type_ == multiplicative_coset_type);

    return this->coset_->shift();
}

template<typename FieldT>
const std::vector<FieldT>& field_subset<FieldT>::basis() const
{
    assert(this->type_ == affine_subspace_type);

    return this->subspace_->basis();
}

template<typename FieldT>
bool field_subset<FieldT>::operator==(const field_subset<FieldT> &other) const
{
    switch (this->type_)
    {
        case affine_subspace_type:
            return this->subspace_ == other.subspace_;
        case multiplicative_coset_type:
            return this->coset_ == other.coset_;
        default:
            return false;
    }
}

template<typename FieldT>
bool field_subset<FieldT>::operator!=(const field_subset<FieldT> &other) const
{
    return !(this->operator==(other));
}

} // namespace libiop
