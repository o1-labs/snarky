namespace libiop {

template<typename FieldT>
std::vector<FieldT> compute_p_alpha_M(
    const size_t input_variable_dim,
    const field_subset<FieldT> &summation_domain,
    const std::vector<FieldT> &p_alpha_over_H,
    const std::vector<FieldT> &r_Mz,
    const std::vector<std::shared_ptr<sparse_matrix<FieldT> >> &matrices)
{
    std::vector<FieldT> p_alpha_M_over_H(
        summation_domain.num_elements(), FieldT::zero());
    for (std::size_t m_index = 0; m_index < matrices.size(); m_index++)
    {
        const std::shared_ptr<sparse_matrix<FieldT>> M = matrices[m_index];
        // M is cons_domain X var_domain
        for (std::size_t i = 0; i < summation_domain.num_elements(); i++)
        {
            const linear_combination<FieldT> row = M->get_row(i);

            for (auto &term : row.terms)
            {
                const std::size_t summation_index = summation_domain.reindex_by_subset(
                    input_variable_dim, term.index_);
                // TODO: Change this to only do |H| multiplications by r_Mz
                p_alpha_M_over_H[summation_index] +=
                    r_Mz[m_index] * term.coeff_ * p_alpha_over_H[i];
            }
        }
    }
    enter_block("multi_lincheck IFFT p_alpha_M");
    std::vector<FieldT> p_alpha_M =
        IFFT_over_field_subset<FieldT>(p_alpha_M_over_H, summation_domain);
    leave_block("multi_lincheck IFFT p_alpha_M");
    return p_alpha_M;
}

} // libiop
