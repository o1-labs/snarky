#include <cstdint>

namespace libiop {

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> evaluate_next_f_i_over_entire_domain(
    const std::shared_ptr<std::vector<FieldT>> &f_i_evals,
    const field_subset<FieldT> &f_i_domain,
    const size_t coset_size,
    const FieldT x_i)
{
    /** The f_i_domain is partitioned into cosets by the localizer polynomial.
     *  This function computes the evaluations of f_{i + 1}, over its systematic domain.
     *  The systematic domain is bijective to the cosets of the localizer domain.
     *  What the actual elements of the systematic domain are
     *  don't matter for the purpose of this function.
     *
     *  f_{i + 1}'s evaluation on the jth element of the systematic domain is evaluated as follows:
     *    * Let C be the jth coset of the localizer polynomial in f_i's domain.
     *    * Interpolate P from (f_i |_C)
     *    * f_{i + 1}(j) = P(x_i)
     *
     *  Since we only need to evaluate P at a single point, we do lagrange interpolation at that point.
     *  This dispatches to domain-type dependent functions due to lagrange interpolation optimizations.
     */
    if (f_i_domain.type() == affine_subspace_type) {
        return additive_evaluate_next_f_i_over_entire_domain(
            f_i_evals, f_i_domain, coset_size, x_i);
    } else if (f_i_domain.type() == multiplicative_coset_type) {
        return multiplicative_evaluate_next_f_i_over_entire_domain(
            f_i_evals, f_i_domain, coset_size, x_i);
    }
    throw std::invalid_argument("f_i_domain is of unsupported domain type");
}

template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> additive_evaluate_next_f_i_over_entire_domain(
    const std::shared_ptr<std::vector<FieldT>> &f_i_evals,
    const field_subset<FieldT> &f_i_domain,
    const size_t coset_size,
    const FieldT x_i)
{
    const std::vector<FieldT> all_elements = f_i_domain.all_elements();
    const size_t num_cosets = all_elements.size() / coset_size;
    std::shared_ptr<std::vector<FieldT>> next_f_i = std::make_shared<std::vector<FieldT>>();
    next_f_i->reserve(num_cosets);

    /** Lagrange coefficient for coset element k is: vp_coset(x) / vp_coset[1] * (x - v[k])
     *
     *  The only change in vp_coset between cosets of the same domain is the constant term.
     *  As a consequence, we only need to calculate the vp_coset(x) and vp_coset[1] once.
     *  We then just adjust the vp_coset(x) by the constant term when processing each coset.
     *  TODO: Investigate if we can lower the number of inversions by taking advantage of
     *        v[k] = unshifted v[k % coset_size] + offset
     */

    const std::vector<FieldT> coset_basis = f_i_domain.get_subset_of_order(coset_size).basis();
    const affine_subspace<FieldT> unshifted_coset(coset_basis, FieldT::zero());
    const linearized_polynomial<FieldT> unshifted_vp =
        vanishing_polynomial_from_subspace(unshifted_coset);

    const FieldT unshifted_vp_x = unshifted_vp.evaluation_at_point(x_i);
    const FieldT inv_vp_linear_term = unshifted_vp.coefficients()[1].inverse();
    /** We process cosets one at a time.
     *  We should batch process them for fewer inversions,
     *  but at too large of a batch size we will lose out on cache efficiency.    */
    /* x - V[k] vector, defined outside the loop to avoid re-allocations. */
    std::vector<FieldT> shifted_coset_elements(coset_size);
    for (size_t j = 0; j < num_cosets; j++)
    {
        /** By definition of cosets,
         *  shifted vp = unshifted vp - unshifted_vp(shift) */
        const FieldT coset_offset = all_elements[coset_size * j];
        const FieldT shifted_vp_x = unshifted_vp_x -
            unshifted_vp.evaluation_at_point(coset_offset);

        const bool x_in_domain = shifted_vp_x == FieldT::zero();
        FieldT interpolation = FieldT::zero();
        for (std::size_t k = 0; k < coset_size; k++)
        {
            /** If x in coset, set the interpolation accordingly. */
            if (x_in_domain && x_i == all_elements[j*coset_size + k])
            {
                interpolation = f_i_evals->operator[](j*coset_size + k);
                break;
            }
            /* If it's not in the coset, set this element to x - V[k] */
            shifted_coset_elements[k] = x_i - all_elements[j*coset_size + k];
        }
        if (!x_in_domain)
        {
            const FieldT k = inv_vp_linear_term * shifted_vp_x;
            const std::vector<FieldT> lagrange_coefficients =
                batch_inverse_and_mul(shifted_coset_elements, k);
            for (std::size_t k = 0; k < coset_size; k++)
            {
                interpolation += f_i_evals->operator[](j*coset_size + k) * lagrange_coefficients[k];
            }
        }
        next_f_i->emplace_back(interpolation);
    }
    return next_f_i;
}


template<typename FieldT>
std::shared_ptr<std::vector<FieldT>> multiplicative_evaluate_next_f_i_over_entire_domain(
    const std::shared_ptr<std::vector<FieldT>> &f_i_evals,
    const field_subset<FieldT> &f_i_domain,
    const size_t coset_size,
    const FieldT x_i)
{
    const size_t num_cosets = f_i_domain.num_elements() / coset_size;
    std::shared_ptr<std::vector<FieldT>> next_f_i = std::make_shared<std::vector<FieldT>>();
    next_f_i->reserve(num_cosets);

    /** Let g be the generator for the coset, and h be the affine shift.
     *  Then the Lagrange coefficient for coset element k is:
     *  (vp_coset(x) * z_k) / (x - v[k]),
     *  where z_k = g^k / (|coset| * h^(|coset|-1)).
     *  See algebra/lagrange.tcc for the derivation of this equation.
     *
     *  We now optimize this for interpolating many equal sized cosets of a domain.
     *  To minimize inversions, we will do a single batch inversion for all cosets.
     *
     *  As cosets change, in these equations h changes,
     *  which also changes v[k] as v[k] = hg^k.
     *  Since g^k / (x - v[k]) = g^k / (x - hg^k) = 1 / (xg^{-k} - h),
     *  we cache the |coset| many values of xg^{-k}, to save 2L multiplications.
     *
     *  The coefficients that have to be computed are now:
     *    vp_coset(x) * z_k / (x - v[k])
     *  = vp_coset(x) * g^k / (|coset| * h^(|coset| - 1) (x - v[k]))
     *  = vp_coset(x) / (|coset| * h^(|coset| - 1) (xg^{-k} - h))
     *
     *  (xg^{-k} - h) is computed for all combinations of (k,h),
     *  and is computed with L additions.
     *  It is then batch inverted w/ 3L multiplications.
     *
     *  |coset|^{-1} is a constant for all coefficients,
     *  so we handle that in the batch inverse and mul, with just 1 multiplication.
     *
     *  vp_coset(x) / h^{|coset| - 1} is a constant for each coset.
     *  So we compute it for each coset,
     *  and since the prover does an interpolation for each coset,
     *  it gets multiplied against the interpolated value.
     *
     *  Recall that vp_coset(x) = x^|coset| - h^|coset|, consequently
     *  vp_coset(x) / h^{|coset| - 1} = x^{|coset|} h^{-|coset| + 1} - h
     *  Computing the per coset constant with the latter expression
     *  saves |L|/|coset| multiplications.
     */

    const FieldT h_inc = f_i_domain.generator();
    const FieldT h_inc_to_coset_inv_plus_one =
        libiop::power(h_inc, coset_size).inverse() * h_inc;
    const field_subset<FieldT> shiftless_coset(coset_size, FieldT::one());
    const FieldT g = shiftless_coset.generator();
    const FieldT g_inv = g.inverse();
    const FieldT x_to_order_coset = libiop::power(x_i, coset_size);
    /* xg^{-k} */
    std::vector<FieldT> shifted_x_elements(coset_size);
    shifted_x_elements[0] = x_i;
    for (size_t i = 1; i < coset_size; i++)
    {
        shifted_x_elements[i] = shifted_x_elements[i - 1] * g_inv;
    }

    FieldT cur_h = f_i_domain.shift();
    const FieldT first_h_to_coset_inv_plus_one = libiop::power(cur_h, coset_size).inverse() * cur_h;
    FieldT cur_coset_constant_plus_h = x_to_order_coset * first_h_to_coset_inv_plus_one;

    /* xg^{-k} - h, for all combinations of k, h.  */
    std::vector<FieldT> elements_to_invert;
    elements_to_invert.reserve(f_i_evals->size());
    /** constant for each coset, equal to
     *  vp_coset(x) / h^{|coset| - 1} = x^{|coset|} h^{-|coset| + 1} - h */
    std::vector<FieldT> constant_for_each_coset;
    constant_for_each_coset.reserve(num_cosets);

    const FieldT constant_for_all_cosets = FieldT(coset_size).inverse();
    bool x_ever_in_domain = false;
    size_t x_coset_index = 0;
    size_t x_index_in_domain = 0;

    /** First we create all the constants for each coset,
     *  and the entire vector of elements to invert, xg^{-k} - h.
     */
    for (size_t j = 0; j < num_cosets; j++)
    {
        /* coset constant = x^|coset| * h^{1 - |coset|} - h */
        const FieldT coset_constant = cur_coset_constant_plus_h - cur_h;
        constant_for_each_coset.emplace_back(coset_constant);
        /** coset_constant = vp_coset(x) * h^{-|coset| + 1},
         * since h is non-zero, coset_constant is zero iff vp_coset(x) is zero.
         * If vp_coset(x) is zero, then x is in the coset. */
        const bool x_in_coset = (coset_constant == FieldT::zero());
        /** if x is in the coset, we mark which position x is within f_i_domain,
         *  and we pad elements to invert to simplify inversion later. */
        if (x_in_coset)
        {
            x_ever_in_domain = true;
            x_coset_index = j;
            // find which element in the coset x belongs to.
            // also pad elements_to_invert to simplify indexing
            FieldT cur_elem = cur_h;
            for (size_t k = 0; k < coset_size; k++)
            {
                if (cur_elem == x_i)
                {
                    x_index_in_domain = k * num_cosets + j;
                }
                cur_elem *= g;
                elements_to_invert.emplace_back(FieldT::one());
            }
            continue;
        }
        /** Append all elements to invert, (xg^{-k} - h) */
        for (std::size_t k = 0; k < coset_size; k++)
        {
            elements_to_invert.emplace_back(shifted_x_elements[k] - cur_h);
        }

        cur_h *= h_inc;
        /** coset constant = x^|coset| * h^{1 - |coset|} - h
         *  So we can efficiently increment x^|coset| * h^{1 - |coset|} */
        cur_coset_constant_plus_h *= h_inc_to_coset_inv_plus_one;
    }
    /* Technically not lagrange coefficients, its missing the constant for each coset */
    const std::vector<FieldT> lagrange_coefficients =
        batch_inverse_and_mul(elements_to_invert, constant_for_all_cosets);
    for (size_t j = 0; j < num_cosets; j++)
    {
        FieldT interpolation = FieldT::zero();
        for (std::size_t k = 0; k < coset_size; k++) {
            interpolation += f_i_evals->operator[](k * num_cosets + j) *
                lagrange_coefficients[j*coset_size + k];
        }
        /* Multiply the constant for each coset, to get the correct interpolation */
        interpolation *= constant_for_each_coset[j];
        next_f_i->emplace_back(interpolation);
    }
    /* if x ever in domain, correct that evaluation. */
    if (x_ever_in_domain)
    {
        next_f_i->operator[](x_coset_index) = f_i_evals->operator[](x_index_in_domain);
    }
    return next_f_i;
}

template<typename FieldT>
FieldT evaluate_next_f_i_at_coset(
    const std::vector<FieldT> &f_i_evals_over_coset,
    const field_subset<FieldT> &unshifted_coset,
    const FieldT offset,
    const localizer_polynomial<FieldT> &unshifted_vp,
    const FieldT x_i)
{
    if (unshifted_coset.type() == affine_subspace_type) {
        return additive_evaluate_next_f_i_at_coset(
            f_i_evals_over_coset, unshifted_coset, offset, unshifted_vp, x_i);
    } else if (unshifted_coset.type() == multiplicative_coset_type) {
        const FieldT g = unshifted_coset.generator();
        return multiplicative_evaluate_next_f_i_at_coset(
            f_i_evals_over_coset, g, offset, x_i);
    }
    throw std::invalid_argument("unshifted_coset is of unsupported domain type");
}

template<typename FieldT>
FieldT additive_evaluate_next_f_i_at_coset(
    const std::vector<FieldT> &f_i_evals_over_coset,
    const field_subset<FieldT> &localizer_domain,
    const FieldT offset,
    const localizer_polynomial<FieldT> &unshifted_vp,
    const FieldT x_i)
{
    /** Other than vanishing polynomial calculation, this function is the same as
     *  the subspace lagrange coefficient generation, but with the interpolation being returned. */
    /* TODO: Cache unshifted_vp(x_i) and c */
    const FieldT vp_x = unshifted_vp.evaluation_at_point(x_i) -
        unshifted_vp.evaluation_at_point(offset);
    const FieldT c = unshifted_vp.get_linearized_polynomial().coefficients()[1].inverse();
    const bool x_in_domain = vp_x == FieldT::zero();
    /* In binary fields addition and subtraction are the same operation */
    const std::vector<FieldT> coset_elems =
        all_subset_sums<FieldT>(localizer_domain.basis(), x_i + offset);
    if (x_in_domain)
    {
        for (size_t k = 0; k < f_i_evals_over_coset.size(); k++)
        {
            if (coset_elems[k] == FieldT::zero()) {
                return f_i_evals_over_coset[k];
            }
        }
    }
    const std::vector<FieldT> lagrange_coefficients = batch_inverse_and_mul(coset_elems, vp_x * c);
    FieldT interpolation = FieldT::zero();
    for (size_t k = 0; k < coset_elems.size(); k++)
    {
        interpolation += lagrange_coefficients[k] * f_i_evals_over_coset[k];
    }
    return interpolation;
}

template<typename FieldT>
FieldT multiplicative_evaluate_next_f_i_at_coset(
    const std::vector<FieldT> &f_i_evals_over_coset,
    const FieldT g,
    const FieldT h,
    const FieldT x_i)
{
    /* TODO: Cache x_i^m, speed up transitions between different h's */
    /* vp(x) = x^m - h^m */
    const size_t coset_size = f_i_evals_over_coset.size();
    const FieldT m = FieldT(coset_size);
    const FieldT vp_x = libiop::power(x_i, coset_size) -
        libiop::power(h, coset_size);
    const bool x_in_domain = vp_x == FieldT::zero();
    const FieldT c = vp_x * (FieldT(m) * libiop::power(h, coset_size - 1)).inverse();

    // TODO: Add comments here for how coefficients are being generated.
    // (its the same as in lagrange.tcc, except we don't include l in the batch inversion,
    //  since we're making another pass to interpolate it anyway)

    std::vector<FieldT> shifted_coset_elems;
    shifted_coset_elems.reserve(coset_size);
    FieldT cur_coset_elem = h;

    for (size_t k = 0; k < coset_size; k++)
    {
        if (x_in_domain && cur_coset_elem == x_i) {
            return f_i_evals_over_coset[k];
        }
        shifted_coset_elems.emplace_back(x_i - cur_coset_elem);
        cur_coset_elem *= g;
    }

    const std::vector<FieldT> inverted_elems = batch_inverse_and_mul(shifted_coset_elems, c);
    FieldT interpolation = FieldT::zero();
    FieldT cur_unshifted_elem = FieldT::one();
    for (size_t k = 0; k < coset_size; k++)
    {
        const FieldT lagrange_coefficient = inverted_elems[k] * cur_unshifted_elem;
        interpolation += lagrange_coefficient * f_i_evals_over_coset[k];
        cur_unshifted_elem *= g;
    }
    return interpolation;
}

/** Given a query position handle for something in the previous coset,
 *  generate query position handles for every position in the next coset we localize to,
 *  with the handles ordered by position in coset.
 */
template<typename FieldT>
std::vector<query_position_handle> calculate_next_coset_query_positions(
    iop_protocol<FieldT> &IOP,
    const query_position_handle &non_localized_query_handle,
    const field_subset<FieldT> &non_localized_domain,
    const field_subset<FieldT> &localized_domain,
    const size_t prev_localization_parameter,
    const size_t cur_localization_parameter)
{
    /* "cur" refers to the domain after localization. */
    const size_t prev_coset_size = 1ull << prev_localization_parameter;
    const size_t cur_coset_size = 1ull << cur_localization_parameter;
    const size_t cur_domain_size = localized_domain.num_elements();
    std::vector<query_position_handle> query_pos(cur_coset_size);

    /** The previous query position gets localized to its coset index within the previous domain,
     *  for cosets of size (1ull << localization_parameter). Call this the localized position.
     *  Then we find the coset it is in, and query every position witin it. */
    for (size_t i = 0; i < cur_coset_size; i++)
    {
        query_pos[i] = IOP.register_deterministic_query_position(
            { non_localized_query_handle },
            [non_localized_domain, localized_domain, prev_coset_size, cur_coset_size, i]
            (const std::vector<std::size_t> &seed_positions)
            -> std::size_t {
                const std::size_t si_idx = seed_positions[0];
                const size_t localized_position = non_localized_domain.coset_index(si_idx, prev_coset_size);
                const size_t localized_coset_index = localized_domain.coset_index(localized_position, cur_coset_size);
                return localized_domain.position_by_coset_indices(localized_coset_index, i, cur_coset_size);
            });
    }
    return query_pos;
}

// -------------------------------------------------
// Optimizer utils

/** Generate all possible localization vectors that begin with starting,
 *  and reduce up to max_reducable_dimensions dimensions.
 */
std::vector<std::vector<size_t>> localization_vector_generator(
    size_t max_reducable_dimensions,
    std::vector<size_t> starting)
{
    std::vector<std::vector<size_t>> options;
    options.push_back(starting);
    if (max_reducable_dimensions == 0)
    {
        return options;
    }
    for (size_t i = 1; i <= max_reducable_dimensions; ++i)
    {
        std::vector<size_t> new_starting = starting;
        new_starting.push_back(i);
        std::vector<std::vector<size_t>> new_options =
            localization_vector_generator(max_reducable_dimensions - i, new_starting);
        options.insert(options.end(), new_options.begin(), new_options.end());
    }
    return options;
}

/* return all partitions of this number (that is, all possible FRI localization parameter vectors
   for this codeword domain dimension) */
std::vector<std::vector<size_t>> all_localization_vectors(size_t dimension_to_reduce)
{
    /* Fix the start as 1 */
    std::vector<size_t> starting({1});
    return localization_vector_generator(dimension_to_reduce - 1, starting);
}

} // namespace libiop
