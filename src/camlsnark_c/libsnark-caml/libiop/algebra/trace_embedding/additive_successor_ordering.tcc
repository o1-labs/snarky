namespace libiop {

/** precomputed table of primitive polynomials for F_2^n.
 *  Currently these are the Conway polynomials.
 *  Conway polynomials are overly restrictive,
 *  since the 'compatibility' requirement between the generator and generators of the subfields is not needed here.
 *  However we don't know of any speed difference for the domain sizes of interest
 *  without making custom binary field operations.
 *  Consequently we use the Conway polynomial for now as it is fairly standard.
 *
 *  In the table we store the Conway polynomial evaluated at an irreducible element of the field.
 *  The irreducible element we use is the variable of the primitive polynomial,
 *  which is the multiplicative generator we are concerned with.
 *  These are generated from the following sage script:
 *  for dim in range(2, 41):
        K.<a> = GF(2^dim, name='a', modulus='conway')
        p = K.polynomial()
        # Now we print the modulus for cpp
        # coeff contains the terms starting from the constant term going to the highest degree term
        coeff = p.coefficients(sparse=False)
        coeff = coeff[::-1]
        built_str = ("    [%s] = 0b" % dim) + ''.join([str(x) for x in coeff]) + ","
        print(built_str)
 */
static const uint64_t primitive_polynomial_table[] =
{
    [0] = 0, /** Unused*/
    [1] = 0, /** Unused */
    [2] = 0b111,
    [3] = 0b1011,
    [4] = 0b10011,
    [5] = 0b100101,
    [6] = 0b1011011,
    [7] = 0b10000011,
    [8] = 0b100011101,
    [9] = 0b1000010001,
    [10] = 0b10001101111,
    [11] = 0b100000000101,
    [12] = 0b1000011101011,
    [13] = 0b10000000011011,
    [14] = 0b100000010101001,
    [15] = 0b1000000000110101,
    [16] = 0b10000000000101101,
    [17] = 0b100000000000001001,
    [18] = 0b1000001010000000011,
    [19] = 0b10000000000000100111,
    [20] = 0b100000000011011110011,
    [21] = 0b1000000000000001100101,
    [22] = 0b10000000001111101100001,
    [23] = 0b100000000000000000100001,
    [24] = 0b1000000011110011010101001,
    [25] = 0b10000000000000000101000101,
    [26] = 0b100000000000100010111010011,
    [27] = 0b1000000000000001011010101101,
    [28] = 0b10000000000000010000011100101,
    [29] = 0b100000000000000000000000000101,
    [30] = 0b1000000000000110010100010101111,
    [31] = 0b10000000000000000000000000001001,
    [32] = 0b100000000000000001000001010011001,
    [33] = 0b1000000000000000000011110101001001,
    [34] = 0b10000000000000000011001100111110111,
    [35] = 0b100000000000000000000000110010100101,
    [36] = 0b1000000000000110110100110000101100011,
    [37] = 0b10000000000000000000000000000000111111,
    [38] = 0b100000000000000000000000100011100100111,
    [39] = 0b1000000000000000000000001001111011100101,
    [40] = 0b10000000000000000101001011011000100101011};

template<typename FieldT>
additive_successor_polynomial<FieldT>::additive_successor_polynomial(const affine_subspace<FieldT> &domain) :
    subspace_(domain)
{
    /** The idea of the additive successor polynomial is to identify the subspace with
     *  the multiplicative group of a field, where the field has the same order as the subspace.*/

    /** Check that the provided domain's basis is the standard basis.
     *  We currently don't support more basis, though more do have similar identifications. */
    if (this->subspace_.is_standard_basis() == false)
    {
        throw std::invalid_argument("Provided basis is not the standard basis.");
    }
    /** Due to the representation of binary field elements,
     *  FieldT(2) will always be the multiplicative generator of the field */
    this->multiplicative_generator_ = FieldT(2);
    /** Now we need to create a primitive polynomial for the field which the subspace is identified with.
     *  We only need its evaluation at the multiplicative_generator, so the following suffices */
    this->primitive_polynomial_at_multiplicative_generator_ = FieldT(primitive_polynomial_table[domain.dimension()]);
    /** The additive successor polynomial is a piecewise polynomial,
     *  which in the case of a binary field consists of 3 partitions.
     *  If the subspace is S, with dimension i, g is the multiplicative generator,
     *  note that S is spanned by {1, g, ..., g^{i - 1}}
     *  let S' be the subspace of S spanned by {1, g, ..., g^{i - 2}}
     *  Notably, the basis of S' does not include g^{i - 1}
     *  The 3 partitions we consider are then:
     *      {0},
     *      S' / {0},
     *      S' + g^(i - 1)
     *  The successor of 0 will be 1,
     *  the successor of x in (S' / {0}) will be g*x,
     *  and the successor of x in (S' + g^(i - 1)) will be g*x + primitive_polynomial(g)
     *
     *  The following code is calculating constituent terms for each partition polynomial.
     *  For the affine case, the partitions will account for the offset, but otherwise remain the same.
     *  In the below computations, the vanishing polynomials will be over the affine subspaces.
     */

    /** For partition {0}, the successor of {0} is 1 + affine_offset, and the indicator polynomial is
     *  L_{S, 0}, which is the normalized lagrange basis polynomial for the 0th element of S. */
    const FieldT zeroth_element_of_S = this->subspace_.offset();
    const bool is_normalized = true;
    this->lagrange_indicator_polynomial_ =
        lagrange_polynomial<FieldT>(zeroth_element_of_S, field_subset<FieldT>(this->subspace_), is_normalized);

    /** S_truncated is the subspace of S with its final basis vector removed.
     *  This is needed for the final two partitions. It is also denoted as S' in comments */
    affine_subspace<FieldT> S_truncated =
        affine_subspace<FieldT>::shifted_standard_basis(domain.dimension() - 1, domain.offset());
    this->Z_S_truncated_ = vanishing_polynomial<FieldT>(S_truncated);
    /** For partitions S' / {0} and S' + x^(i - 1),
     *      we need polynomials L_c, for c \in [0,1] such that L_c(cZ_{S'}(g^{i - 1} + affine_offset)) = 1,
     *      and L_c((1 - c)Z_{S'}(g^{i - 1} + affine_offset)) = 0. */
    const FieldT multiplicative_generator_to_i_minus_one =
        libiop::power(this->multiplicative_generator_, domain.dimension() - 1) + this->subspace_.offset();
    this->Z_S_truncated_at_multiplicative_generator_to_i_minus_one_ =
        this->Z_S_truncated_.evaluation_at_point(multiplicative_generator_to_i_minus_one);
    /** L_0 is the degree 1 polynomial that is 1 when the argument is 0,
     *  and 0 when the argument is Z_S_at_multiplicative_generator_to_i_minus_one.
     *  So L_0 = k * (X - Z_S_at_multiplicative_generator_to_i_minus_one) */
    this->L_0_coefficient_ = (-this->Z_S_truncated_at_multiplicative_generator_to_i_minus_one_).inverse();
    /** L_1 is the degree 1 polynomial that is 0 when the argument is 0,
     *  and 1 when the argument is Z_S_at_multiplicative_generator_to_i_minus_one.
     *  So L_1 = k * X */
    this->L_1_coefficient_ = this->Z_S_truncated_at_multiplicative_generator_to_i_minus_one_.inverse();
}

template<typename FieldT>
FieldT additive_successor_polynomial<FieldT>::evaluation_at_point(const FieldT &evalpoint) const
{
    /** Affine shift of the subspace. */
    const FieldT offset = this->subspace_.offset();
    FieldT result = FieldT::zero();
    FieldT Z_S_truncated_at_x = this->Z_S_truncated_.evaluation_at_point(evalpoint);

    /** Partition 0 at x is the lagrange_indicator_polynomial at x */
    const FieldT partition_0_eval = this->lagrange_indicator_polynomial_.evaluation_at_point(evalpoint);
    /** The value at this partition is (1 + offset) */
    result += partition_0_eval * (FieldT::one() + offset);

    /** L_0 is the polynomial that is the degree 1 polynomial that is 1 when the argument is 0,
     *  and 0 when the argument is Z_S_at_multiplicative_generator_to_i_minus_one.
     *  So L_0 = k * (X - Z_S_at_multiplicative_generator_to_i_minus_one)
     *  L_0 is evaluated at Z_S'(X)*/
    const FieldT L_0_eval = this->L_0_coefficient_ *
        (Z_S_truncated_at_x - this->Z_S_truncated_at_multiplicative_generator_to_i_minus_one_);
    /** This partition is L_0 - partition 0,
     *  and has value: multiplicative_generator * (X - offset) + offset */
    result += (L_0_eval - partition_0_eval) *
        (this->multiplicative_generator_ * (evalpoint - offset) + offset);
    /** L_1 is the polynomial that is the degree 1 polynomial that is 0 when the argument is 0,
     *  and 1 when the argument is Z_S_at_multiplicative_generator_to_i_minus_one.
     *  So L_1 = k * X
     *  L_1 is evaluated at Z_S'(X) */
    const FieldT L_1_eval = this->L_1_coefficient_ * Z_S_truncated_at_x;
    /** This partition is L_1, and has value (g * (X - offset) + offset + (primitive polynomial at g)) */
    result += L_1_eval *
        (this->multiplicative_generator_ * (evalpoint - offset) +
         offset + this->primitive_polynomial_at_multiplicative_generator_);

    return result;
}

template<typename FieldT>
std::vector<FieldT> additive_successor_polynomial<FieldT>::evaluations_over_field_subset(
    const field_subset<FieldT> &U) const
{
    const FieldT S_offset = this->subspace_.offset();
    std::vector<FieldT> Z_S_truncated_over_U = this->Z_S_truncated_.evaluations_over_field_subset(U);
    /** We only need (x + S_offset) in the evaluation procedure, for x in U.
     *  Note that we are in a binary field, so (x + S_offset) = (x - S_offset) */
    std::vector<FieldT> shifted_U_elements =
        all_subset_sums<FieldT>(U.basis(), S_offset + U.offset());

    // If we need to squeeze performance out of this method,
    // then the lagrange polynomial's evaluation over domain can be opened up here,
    // and we can take the following optimization:
    //      Z_S(x) = Z_S_truncated(x) * Z_S_truncated(x + g^{i - 1}) optimization
    // and additionally can de-duplicate the shifted_U_elements calculation.
    std::vector<FieldT> lagrange_indicator_evaluations =
        this->lagrange_indicator_polynomial_.evaluations_over_field_subset(U);

    std::vector<FieldT> result(U.num_elements(), FieldT::zero());
    const FieldT one_plus_S_offset = FieldT::one() + S_offset;
    for (size_t i = 0; i < result.size(); i++)
    {
        // The partition 0 polynomial is the lagrange indicator polynomial
        // Value at partition 0 is (1 + offset)
        result[i] += lagrange_indicator_evaluations[i] * one_plus_S_offset;

        // The partition 1 polynomial is: L_0(Z_S_truncated(X)) - partition_0(X)
        const FieldT L_0_eval = this->L_0_coefficient_ *
            (Z_S_truncated_over_U[i] - this->Z_S_truncated_at_multiplicative_generator_to_i_minus_one_);
        const FieldT partition_1_eval = L_0_eval - lagrange_indicator_evaluations[i];
        // Value at partition 1 is multiplicative_generator * (X - S_offset) + S_offset
        result[i] += partition_1_eval *
            (this->multiplicative_generator_ * shifted_U_elements[i] + S_offset);

        // The partition 2 polynomial is: L_1(Z_S_truncated(X))
        const FieldT partition_2_eval = this->L_1_coefficient_ * Z_S_truncated_over_U[i];
        // Value at partition 2 is (multiplicative_generator * (X - S_offset) + S_offset +
        //                          primitive_polynomial at multiplicative_generator)
        result[i] += partition_2_eval *
            (this->multiplicative_generator_ * shifted_U_elements[i] + S_offset +
             this->primitive_polynomial_at_multiplicative_generator_);
    }
    return result;
}

template<typename FieldT>
size_t additive_successor_polynomial<FieldT>::degree() const
{
    /** The partition polynomials are degree |H| - 1, the corresponding value polynomial is degree 1
     *  which results in total degree |H| */
    return this->subspace_.num_elements();
}

template<typename FieldT>
size_t additive_successor_polynomial<FieldT>::piecewise_degree() const {
    return 1;
}

template<typename FieldT>
std::shared_ptr<piecewise_polynomial_base<FieldT>> additive_successor_polynomial<FieldT>::compose(
        const std::shared_ptr<polynomial_base<FieldT>> poly) const
{
    /** TODO: Unimplemented */
    return std::make_shared<additive_successor_polynomial<FieldT>>(
        additive_successor_polynomial<FieldT>());
}

template<typename FieldT>
additive_successor_ordering<FieldT>::additive_successor_ordering(const field_subset<FieldT> &domain)
{
    if (domain.type() != affine_subspace_type)
    {
        throw std::invalid_argument(
            "additive successor ordering was instantiated with a subset that is not a subspace.");
    }
    this->subspace_ = domain.subspace();
    this->successor_polynomial_ = additive_successor_polynomial<FieldT>(this->subspace_);
}

template<typename FieldT>
FieldT additive_successor_ordering<FieldT>::first_elem() const
{
    return this->subspace_.offset();
}

template<typename FieldT>
FieldT additive_successor_ordering<FieldT>::next_elem(const FieldT &cur_elem) const
{
    return this->successor_polynomial_.evaluation_at_point(cur_elem);
}

template<typename FieldT>
std::shared_ptr<piecewise_polynomial_base<FieldT>>
    additive_successor_ordering<FieldT>::piecewise_polynomial() const
{
    return std::make_shared<additive_successor_polynomial<FieldT>>(this->successor_polynomial_);
}

} // namespace libiop
