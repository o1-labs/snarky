namespace libiop {

template<typename FieldT>
trace_embedding<FieldT>::trace_embedding(
    field_subset<FieldT> &H,
    field_subset<FieldT> &row_domain,
    field_subset<FieldT> &col_domain) :
    bivariate_embedding_(bivariate_embedding<FieldT>(H, row_domain, col_domain)),
    successor_ordering_(successor_ordering<FieldT>(row_domain))
{
}

template<typename FieldT>
bivariate_embedding<FieldT> trace_embedding<FieldT>::bivariate_embedding() const
{
    return this->bivariate_embedding_;
}

template<typename FieldT>
successor_ordering<FieldT> trace_embedding<FieldT>::successor_ordering() const
{
    return this->successor_ordering_;
}

} // namespace libiop
