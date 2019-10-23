namespace libiop {

template<typename FieldT>
std::vector<query_position_handle> query_position_to_queries_for_entire_coset(
    iop_protocol<FieldT> &IOP,
    const query_position_handle &initial_query,
    const field_subset<FieldT> &domain,
    const size_t coset_size)
{
    std::vector<query_position_handle> query_pos(coset_size);
    for (size_t i = 0; i < coset_size; i++)
    {
        query_pos[i] = IOP.register_deterministic_query_position(
            { initial_query },
            [domain, coset_size, i]
            (const std::vector<std::size_t> &seed_positions)
            -> std::size_t {
                const std::size_t index = seed_positions[0];
                const size_t coset_index = domain.coset_index(index, coset_size);
                return domain.position_by_coset_indices(coset_index, i, coset_size);
            });
    }
    return query_pos;
}

} // namespace libiop
