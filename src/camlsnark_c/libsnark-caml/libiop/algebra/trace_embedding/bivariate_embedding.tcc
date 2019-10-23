namespace libiop {

template<typename FieldT>
bivariate_embedding<FieldT>::bivariate_embedding(const field_subset<FieldT> &H,
    const field_subset<FieldT> &H1, const field_subset<FieldT> &H2)
{
    if (H.type() != H1.type() || H1.type() != H2.type())
    {
        throw std::invalid_argument("field_subset types don't match");
    }
    else if (H.num_elements() != H1.num_elements() * H2.num_elements())
    {
        throw std::invalid_argument("|H| != |H1| * |H2|, expected this property to hold.");
    }
    if (H.type() == multiplicative_coset_type)
    {
        /** In the multiplicative case, the two domains must be coprime.
         *  A GCD could potentially be too expensive for the verifier,
         *  as they vary in running time from O(log(x)) to O(log(x^2)) depending on
         *  the cost model and domain sizes, whereas we want the bivariate embedding
         *  to be O(log(|H|)) in the multiplicative case.
         *  In the protocols of interest, one of the domains will be a power of 2,
         *  so we just check that, and ensure that the other domain is not a power of 2.
         */
        if (H.shift() != FieldT::one() || H.shift() != H1.shift() || H1.shift() != H2.shift())
        {
            /** We can implement it for the following set of affine shifts with no change:
             *  if the affine shift of H is h, the affine shift of H1 is h^|H2|,
             *  and the shift of H2 is h^|H1|.
             *  To do other affine shifts, we have to vary the constant term of the k to 1 map,
             *  hence this is left as unimplemented at the moment.
             *  */
            throw std::invalid_argument(
                "The bivariate embedding isn't implemented for multiplicative cosets, only subgroups");
        }
        if (is_power_of_2(H1.num_elements()))
        {
            if (H2.num_elements() % 2 == 0 && H1.num_elements() != 1)
            {
                throw std::invalid_argument("H1 and H2 are not coprime");
            }
        }
        else if (is_power_of_2(H2.num_elements()))
        {
            if (H1.num_elements() % 2 == 0 && H2.num_elements() != 1)
            {
                throw std::invalid_argument("H1 and H2 are not coprime");
            }
        }
        else
        {
            throw std::invalid_argument("neither H1 or H2 is a power of 2");
        }
        this->projection_into_row_vp_ = vanishing_polynomial<FieldT>(H2);
        this->projection_into_col_vp_ = vanishing_polynomial<FieldT>(H1);
        this->projection_into_row_ = this->projection_into_row_vp_.associated_k_to_1_map();
        this->projection_into_col_ = this->projection_into_col_vp_.associated_k_to_1_map();
    }
    else if (H1.type() == affine_subspace_type)
    {
        /** We assume that the first log2(|H1|) basis elements of H correespond to V,
         *  and the latter log2(|H2|) elements correspond to W.
         *  We then check that Z_W(V) = H1, and Z_V(W) = H2.
         *  If not throw an error, otherwise Z_W is the map into H1, and Z_V is the map into H2.
         */
        std::vector<FieldT> H_basis = H.basis();
        std::vector<FieldT> V_basis(H_basis.begin(), H_basis.begin()+H1.dimension());
        std::vector<FieldT> W_basis(H_basis.begin() + H1.dimension(), H_basis.end());
        affine_subspace<FieldT> V(V_basis);
        affine_subspace<FieldT> W(W_basis);
        this->projection_into_row_vp_ = vanishing_polynomial<FieldT>(W);
        this->projection_into_col_vp_ = vanishing_polynomial<FieldT>(V);
        this->projection_into_row_ = this->projection_into_row_vp_.associated_k_to_1_map();
        this->projection_into_col_ = this->projection_into_col_vp_.associated_k_to_1_map();

        /** Now test that Z_W(V) = H1, and Z_V(W) = H2 */
        std::vector<FieldT> actual_H1_basis =
            transform_basis_by_polynomial<FieldT>(this->projection_into_row_, V_basis);
        std::vector<FieldT> actual_H2_basis =
            transform_basis_by_polynomial<FieldT>(this->projection_into_col_, W_basis);
        if (actual_H1_basis != H1.basis() || actual_H2_basis != H2.basis())
        {
            throw std::invalid_argument("H1 or H2 was constructed incorrectly.");
        }
    }
}

template<typename FieldT>
FieldT bivariate_embedding<FieldT>::project_to_row(const FieldT &x) const
{
    return this->projection_into_row_->evaluation_at_point(x);
}

template<typename FieldT>
FieldT bivariate_embedding<FieldT>::project_to_col(const FieldT &x) const
{
    return this->projection_into_col_->evaluation_at_point(x);
}

template<typename FieldT>
std::shared_ptr<polynomial_base<FieldT>> bivariate_embedding<FieldT>::polynomial_map_into_row_domain() const
{
    return this->projection_into_row_;
}

template<typename FieldT>
std::shared_ptr<polynomial_base<FieldT>> bivariate_embedding<FieldT>::polynomial_map_into_col_domain() const
{
    return this->projection_into_col_;
}

template<typename FieldT>
std::shared_ptr<polynomial_base<FieldT>> bivariate_embedding<FieldT>::compose_polynomial_with_row_projection(
        const std::shared_ptr<polynomial_base<FieldT>> &poly) const
{
    std::shared_ptr<polynomial_composed_with_projection<FieldT>> composed_poly =
        std::make_shared<polynomial_composed_with_projection<FieldT>>(
            this->projection_into_row_vp_, this->projection_into_row_, poly);
    return composed_poly;
}

template<typename FieldT>
std::shared_ptr<polynomial_base<FieldT>> bivariate_embedding<FieldT>::compose_polynomial_with_col_projection(
        const std::shared_ptr<polynomial_base<FieldT>> &poly) const
{
    std::shared_ptr<polynomial_composed_with_projection<FieldT>> composed_poly =
        std::make_shared<polynomial_composed_with_projection<FieldT>>(
            this->projection_into_col_vp_, this->projection_into_col_, poly);
    return composed_poly;
}


template<typename FieldT>
class polynomial_composed_with_projection : public polynomial_base<FieldT> {
protected:
    vanishing_polynomial<FieldT> projection_vp_;
    std::shared_ptr<polynomial_base<FieldT>> projection_map_;
    std::shared_ptr<polynomial_base<FieldT>> poly_;
    // TODO: Add method to cache evaluated contents
public:
    polynomial_composed_with_projection(
        const vanishing_polynomial<FieldT> projection_vp,
        const std::shared_ptr<polynomial_base<FieldT>> projection_map,
        const std::shared_ptr<polynomial_base<FieldT>> poly) :
        projection_vp_(projection_vp),
        projection_map_(projection_map),
        poly_(poly)
    {
    }

    FieldT evaluation_at_point(const FieldT &evalpoint) const
    {
        return this->poly_->evaluation_at_point(
            this->projection_map_->evaluation_at_point(evalpoint));
    }

    std::vector<FieldT> evaluations_over_field_subset(
        const field_subset<FieldT> &eval_domain) const
    {
        if (eval_domain.type() != this->projection_vp_.type())
        {
            throw std::invalid_argument("evaluation domain type is not the same as bivariate embedding type");
        }
        enter_block("composed polynomial evaluated contents");
        /** The projection, evaluated over a domain is another algebraically structured domain,
         *  and in most cases of interest is actually smaller than the eval domain.
         *  So we first calculate that potentially smaller domain. */
        field_subset<FieldT> projected_domain =
            this->projection_vp_.associated_k_to_1_map_at_domain(eval_domain);

        std::vector<FieldT> projected_evals = this->poly_->evaluations_over_field_subset(projected_domain);
        // In this case the projection is a 1 to 1 map, so these evaluations are correct.
        if (projected_domain.num_elements() == eval_domain.num_elements())
        {
            leave_block("composed polynomial evaluated contents");
            return projected_evals;
        }
        /** Now we have to duplicate these evals according to how they get replicated in eval domain.
         *  TODO: I think it should be possible to unify the additive and multiplicative cases.
         */

        /** The number of cosets of the projection map is the number of cosets of the k to 1 map. */
        const size_t coset_size = eval_domain.num_elements() / projected_domain.num_elements();
        const size_t num_cosets = projected_domain.num_elements();
        std::vector<FieldT> evals(eval_domain.num_elements(), FieldT::zero());
        if (this->projection_vp_.type() == multiplicative_coset_type)
        {
            // TODO: See what loop bounds are for succinct Aurora, and determine which loop should go first
            // for cache efficiency
            for (size_t intra_coset_index = 0; intra_coset_index < coset_size; intra_coset_index++)
            {
                for (size_t coset_index = 0; coset_index < num_cosets; coset_index++)
                {
                    const size_t index = eval_domain.position_by_coset_indices(coset_index, intra_coset_index, coset_size);
                    evals[index] = projected_evals[coset_index];
                }
            }
        }
        leave_block("composed polynomial evaluated contents");
        return evals;
    }

    size_t degree() const
    {
        // TODO: implement
        throw std::logic_error("Not yet implemented");
    }
};

} // namespace libiop
