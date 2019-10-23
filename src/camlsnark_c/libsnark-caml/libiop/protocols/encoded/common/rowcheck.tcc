#include "libiop/algebra/lagrange.hpp"
#include "libiop/algebra/utils.hpp"

namespace libiop {

template<typename FieldT>
rowcheck_ABC_virtual_oracle<FieldT>::rowcheck_ABC_virtual_oracle(
    const field_subset<FieldT> &codeword_domain,
    const field_subset<FieldT> &constraint_domain) :
    codeword_domain_(codeword_domain),
    constraint_domain_(constraint_domain),
    Z_(constraint_domain)
{
}

/** Takes as input oracles Az, Bz, Cz. */
template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> rowcheck_ABC_virtual_oracle<FieldT>::evaluated_contents(
    const std::vector<std::shared_ptr<std::vector<FieldT>> > &constituent_oracle_evaluations) const
{
    enter_block("rowcheck evaluated contents");
    if (constituent_oracle_evaluations.size() != 3)
    {
        throw std::invalid_argument("rowcheck_ABC has three constituent oracles.");
    }

    const std::shared_ptr<std::vector<FieldT>> &Az = constituent_oracle_evaluations[0];
    const std::shared_ptr<std::vector<FieldT>> &Bz = constituent_oracle_evaluations[1];
    const std::shared_ptr<std::vector<FieldT>> &Cz = constituent_oracle_evaluations[2];
    /** Since evaluations of Z_H repeat, we evaluate Z over its unique evaluations
     *  Invert those evaluations, and use those within building the final codeword.
     *  These evaluations are the same for every coset of H in L.
     *  TODO: Add assert that codeword domain basis is prefixed by constraint domain basis.
     *  We assume this in how we index domains
     */
    const std::vector<FieldT> &Z_inv = batch_inverse(
        this->Z_.unique_evaluations_over_field_subset(this->codeword_domain_));

    const size_t n = this->codeword_domain_.num_elements();
    const size_t order_H = this->constraint_domain_.num_elements();
    const size_t num_cosets_of_H = n / order_H;
    std::shared_ptr<std::vector<FieldT>> result = std::make_shared<std::vector<FieldT>>();
    result->reserve(n);

    /** We compute result(x) = Z_H(x)^{-1} (Az(x) * Bz(x) - Cz(x)))
     *  Since we want to optimize and use Z_H being a k to 1 map,
     *  we have to split up the additive and multiplicative cases.*/
    if (this->codeword_domain_.type() == multiplicative_coset_type)
    {
        /** We iterate over all elements in order.
         *  We use 2 loops, so we always know which coset a particular position is in.
         *  The outer loop specifies that we are looking at the ith element of a coset.
         *  The inner loop specifies which coset we are looking at,
         *  and consequently what its vanishing polynomial evaluation is.
        */
        for (size_t i = 0; i < order_H; i++)
        {
            for (size_t j = 0; j < num_cosets_of_H; j++)
            {
                const size_t cur_pos = i*num_cosets_of_H + j;
                result->emplace_back(Z_inv[j] * (
                    Az->operator[](cur_pos) * Bz->operator[](cur_pos) - Cz->operator[](cur_pos)));
            }
        }
    }
    else if (this->codeword_domain_.type() == affine_subspace_type)
    {
        /** We iterate over all elements in order.
         *  We use 2 loops, so we always know which coset a particular position is in.
         *  The outer loop specifies which coset we are looking at.
         *  The inner loop ranges over all elements of that coset.
         */
        for (size_t i = 0; i < num_cosets_of_H; i++)
        {
            const FieldT Z_inv_val = Z_inv[i];
            const size_t coset_pos_upper_bound = (i + 1) * order_H;
            for (size_t cur_pos = i * order_H;
                cur_pos < coset_pos_upper_bound; cur_pos++)
            {
                result->emplace_back(Z_inv_val *
                    (Az->operator[](cur_pos) * Bz->operator[](cur_pos) - Cz->operator[](cur_pos)));
            }
        }
    }
    leave_block("rowcheck evaluated contents");
    return result;
}

template<typename FieldT>
FieldT rowcheck_ABC_virtual_oracle<FieldT>::evaluation_at_point(
    const std::size_t evaluation_position,
    const FieldT evaluation_point,
    const std::vector<FieldT> &constituent_oracle_evaluations) const
{
    UNUSED(evaluation_position);

    if (constituent_oracle_evaluations.size() != 3)
    {
        throw std::invalid_argument("rowcheck_ABC has three constituent oracles.");
    }

    const FieldT &A_X = constituent_oracle_evaluations[0];
    const FieldT &B_X = constituent_oracle_evaluations[1];
    const FieldT &C_X = constituent_oracle_evaluations[2];
    const FieldT &Z_X_inv = this->Z_.evaluation_at_point(evaluation_point).inverse();

    return (Z_X_inv*(A_X*B_X-C_X));
}

} // libiop
