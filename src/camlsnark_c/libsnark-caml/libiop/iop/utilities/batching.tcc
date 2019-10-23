namespace libiop {

template<typename FieldT>
std::vector<std::shared_ptr<std::vector<FieldT>>> get_all_oracle_evaluations(
    iop_protocol<FieldT> &IOP, const std::vector<oracle_handle_ptr> poly_handles)
{
    std::vector<std::shared_ptr<std::vector<FieldT>>> f_i_evaluations;
    f_i_evaluations.reserve(poly_handles.size());
    for (size_t j = 0; j < poly_handles.size(); j++)
    {
        f_i_evaluations.emplace_back(IOP.get_oracle_evaluations(poly_handles[j]));
    }
    return f_i_evaluations;
}

template<typename FieldT>
std::vector<prover_message_handle> register_n_prover_messages(
    iop_protocol<FieldT> &IOP, const size_t n, const size_t message_size)
{
    std::vector<prover_message_handle> msgs;
    msgs.reserve(n);
    for (size_t i = 0; i < n; i++)
    {
        msgs.emplace_back(IOP.register_prover_message(message_size));
    }
    return msgs;
}

template<typename FieldT>
std::vector<verifier_random_message_handle> register_n_verifier_messages(
    iop_protocol<FieldT> &IOP, const size_t n, const size_t message_size)
{
    std::vector<verifier_random_message_handle> msgs;
    msgs.reserve(n);
    for (size_t i = 0; i < n; i++)
    {
        msgs.emplace_back(IOP.register_verifier_random_message(message_size));
    }
    return msgs;
}

template<typename FieldT>
std::vector<oracle_handle_ptr> register_n_oracles(
    iop_protocol<FieldT> &IOP,
    const size_t n,
    const domain_handle domain,
    const size_t degree_bound,
    const bool make_zk)
{
    std::vector<oracle_handle_ptr> oracles;
    oracles.reserve(n);
    for (size_t i = 0; i < n; i++)
    {
        oracles.emplace_back(std::make_shared<oracle_handle>(
            IOP.register_oracle(domain, degree_bound, make_zk)));
    }
    return oracles;
}

std::vector<oracle_handle_ptr> virtual_oracle_handles_to_handle_ptrs(
    const std::vector<virtual_oracle_handle> handles)
{
    std::vector<oracle_handle_ptr> oracles;
    oracles.reserve(handles.size());
    for (size_t i = 0; i < handles.size(); i++)
    {
        oracles.emplace_back(std::make_shared<virtual_oracle_handle>(handles[i]));
    }
    return oracles;
}

template<typename FieldT>
std::vector<query_handle> register_queries_for_same_pos(
    iop_protocol<FieldT> &IOP,
    const std::vector<oracle_handle_ptr> oracle_handles,
    const query_position_handle query_position)
{
    std::vector<query_handle> queries;
    queries.reserve(oracle_handles.size());
    for (size_t i = 0; i < oracle_handles.size(); i++)
    {
        queries.emplace_back(IOP.register_query(oracle_handles[i], query_position));
    }
    return queries;
}

} // namespace libiop
