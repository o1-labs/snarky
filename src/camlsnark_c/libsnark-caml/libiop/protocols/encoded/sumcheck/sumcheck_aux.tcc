namespace libiop {

template<typename FieldT>
std::vector<FieldT> constant_times_subspace_to_order_H_minus_1(
    const FieldT constant,
    const affine_subspace<FieldT> &subspace,
    const size_t order_H)
{
    /** In the additive case, the prover has to evaluate X^{|H| - 1} for all x in L.
     *  Using subspace_element_powers directly would take log(H)|L| multiplications,
     *  whereas the prover can instead compute X^{|H|} in |L| additions,
     *  batch invert all elements with 3|L| multiplications,
     *  and multiply these results together, with a total of 4|L| multiplications.
     *
     *  However the prover really has to evaluate a constant times X^{|H| - 1},
     *  since we are doing a batch inversion, this can essentially be done for free.
    */
    const std::vector<FieldT> x_to_H =
        subspace_element_powers(subspace, order_H);
    /** TODO: If we make the codeword domain non-affine in the future,
     *        then we should just remove the zero element before batch inversion. */
    const bool codeword_domain_contains_zero = (subspace.offset() == FieldT::zero());
    const std::vector<FieldT> x_inv_times_constant = batch_inverse_and_mul(
        subspace.all_elements(), constant, codeword_domain_contains_zero);
    std::vector<FieldT> constant_times_x_to_H_minus_1(
        subspace.num_elements(), FieldT::zero());
    for (size_t i = 0; i < subspace.num_elements(); i++)
    {
        constant_times_x_to_H_minus_1[i] = x_to_H[i] * x_inv_times_constant[i];
    }
    return constant_times_x_to_H_minus_1;
}

} // libiop
