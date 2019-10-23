#include <stdexcept>

namespace libiop {

template<typename FieldT>
standard_semisuccinct_matrix<FieldT>::standard_semisuccinct_matrix(
    std::shared_ptr<succinct_matrix<FieldT>> succinct,
    std::shared_ptr<sparse_matrix<FieldT>> sparse) :
    succinct_matrix_(succinct),
    sparse_matrix_(sparse)
{
}

template<typename FieldT>
std::shared_ptr<succinct_matrix<FieldT>> standard_semisuccinct_matrix<FieldT>::get_succinct_matrix() const
{
    return this->succinct_matrix_;
}

template<typename FieldT>
std::shared_ptr<sparse_matrix<FieldT>> standard_semisuccinct_matrix<FieldT>::get_unstructured_matrix() const
{
    return this->sparse_matrix_;
}

} // libiop
