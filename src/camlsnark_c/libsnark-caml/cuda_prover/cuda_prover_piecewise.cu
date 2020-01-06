#include <string>
#include <chrono>

#define NDEBUG 1

#include <prover_reference_functions.hpp>

#include "multiexp/reduce.cu"

// This is where all the FFTs happen

// template over the bundle of types and functions.
// Overwrites ca!
template <typename B>
typename B::vector_Fr *compute_H(size_t d, typename B::vector_Fr *ca,
                                 typename B::vector_Fr *cb,
                                 typename B::vector_Fr *cc) {
  auto domain = B::get_evaluation_domain(d + 1);

  B::domain_iFFT(domain, ca);
  B::domain_iFFT(domain, cb);

  B::domain_cosetFFT(domain, ca);
  B::domain_cosetFFT(domain, cb);

  // Use ca to store H
  auto H_tmp = ca;

  size_t m = B::domain_get_m(domain);
  // for i in 0 to m: H_tmp[i] *= cb[i]
  B::vector_Fr_muleq(H_tmp, cb, m);

  B::domain_iFFT(domain, cc);
  B::domain_cosetFFT(domain, cc);

  m = B::domain_get_m(domain);

  // for i in 0 to m: H_tmp[i] -= cc[i]
  B::vector_Fr_subeq(H_tmp, cc, m);

  B::domain_divide_by_Z_on_coset(domain, H_tmp);

  B::domain_icosetFFT(domain, H_tmp);

  m = B::domain_get_m(domain);
  typename B::vector_Fr *H_res = B::vector_Fr_zeros(m + 1);
  B::vector_Fr_copy_into(H_tmp, H_res, m);
  return H_res;
}

static size_t read_size_t(FILE* input) {
  size_t n;
  fread((void *) &n, sizeof(size_t), 1, input);
  return n;
}

template< typename B >
struct ec_type;

template<>
struct ec_type<mnt4753_libsnark> {
    typedef ECp_MNT4 ECp;
    typedef ECp2_MNT4 ECpe;
};

template<>
struct ec_type<mnt6753_libsnark> {
    typedef ECp_MNT6 ECp;
    typedef ECp3_MNT6 ECpe;
};


void
check_trailing(FILE *f, const char *name) {
    long bytes_remaining = 0;
    while (fgetc(f) != EOF)
        ++bytes_remaining;
    if (bytes_remaining > 0)
        fprintf(stderr, "!! Trailing characters in \"%s\": %ld\n", name, bytes_remaining);
}


static inline auto now() -> decltype(std::chrono::high_resolution_clock::now()) {
    return std::chrono::high_resolution_clock::now();
}

template<typename T>
void
print_time(T &t1, const char *str) {
    auto t2 = std::chrono::high_resolution_clock::now();
    auto tim = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    printf("%s: %ld ms\n", str, tim);
    t1 = t2;
}

template <const int R, const int C, typename B>
void prove_aux(
        size_t primary_input_size,
        size_t d,
        size_t m,
        const var *w,
        decltype(std::chrono::high_resolution_clock::now()) t,
        // const var *A_mults,
        // var *out_A,
        const var *B1_mults,
        var *out_B1,
        const var *B2_mults,
        var *out_B2,
        const var *L_mults,
        var *out_L,
        typename B::groth16_params *params,
        typename B::groth16_input *inputs,
        typename B::vector_Fr **coefficients_for_H,
        typename B::vector_G1 **H,
        typename B::G1 **evaluation_At,
        typename B::G1 **evaluation_Bt1,
        typename B::G2 **evaluation_Bt2,
        typename B::G1 **evaluation_Ht,
        typename B::G1 **evaluation_Lt,
        typename B::G1 **scaled_Bt1,
        typename B::G1 **Lt1_plus_scaled_Bt1,
        typename B::G1 **final_C,
        //cudaStream_t &sA,
        cudaStream_t &sB1,
        cudaStream_t &sB2,
        cudaStream_t &sL)
{
    auto t_gpu = t;

    typedef typename ec_type<B>::ECp ECp;
    typedef typename ec_type<B>::ECpe ECpe;

    //ec_reduce_straus<ECp, C, R>(sA, out_A, A_mults, w, m + 1);
    ec_reduce_straus<ECp, C, R>(sB1, out_B1, B1_mults, w, m + 1);
    ec_reduce_straus<ECpe, C, 2*R>(sB2, out_B2, B2_mults, w, m + 1);
    ec_reduce_straus<ECp, C, R>(sL, out_L, L_mults, w + (primary_input_size + 1) * ELT_LIMBS, m - primary_input_size);
    print_time(t, "gpu launch");

    (*evaluation_At) = B::multiexp_G1(B::input_w(inputs), B::params_A(params), m + 1);
    //(*evaluation_Bt1) = B::multiexp_G1(B::input_w(inputs), B::params_B1(params), m + 1);
    //(*evaluation_Bt2) = B::multiexp_G2(B::input_w(inputs), B::params_B2(params), m + 1);

    // Do calculations relating to H on CPU after having set the GPU in
    // motion
    (*H) = B::params_H(params);
    (*coefficients_for_H) =
        compute_H<B>(d, B::input_ca(inputs), B::input_cb(inputs), B::input_cc(inputs));
    (*evaluation_Ht) = B::multiexp_G1(*coefficients_for_H, *H, d);

    print_time(t, "cpu 1");

    cudaDeviceSynchronize();
    //cudaStreamSynchronize(sA);
    //G1 *evaluation_At = B::read_pt_ECp(out_A);

    cudaStreamSynchronize(sB1);
    *evaluation_Bt1 = B::read_pt_ECp(out_B1);

    cudaStreamSynchronize(sB2);
    *evaluation_Bt2 = B::read_pt_ECpe(out_B2);

    cudaStreamSynchronize(sL);
    *evaluation_Lt = B::read_pt_ECp(out_L);

    print_time(t_gpu, "gpu e2e");

    *scaled_Bt1 = B::G1_scale(B::input_r(inputs), *evaluation_Bt1);
    *Lt1_plus_scaled_Bt1 = B::G1_add(*evaluation_Lt, *scaled_Bt1);
    *final_C = B::G1_add(*evaluation_Ht, *Lt1_plus_scaled_Bt1);

    print_time(t, "cpu 2");
}

template< typename EC >
var_ptr copy_points_affine(size_t n, const void *src)
{
    typedef typename EC::field_type FF;

    static constexpr size_t coord_bytes = FF::DEGREE * ELT_BYTES;
    static constexpr size_t aff_pt_bytes = 2 * coord_bytes;

    size_t total_aff_bytes = n * aff_pt_bytes;

    auto mem = allocate_memory(total_aff_bytes);
    memcpy((void *)mem.get(), src, total_aff_bytes);
    return mem;
}

var_ptr copy_field_elts(size_t n, const void *src)
{
    static constexpr size_t field_elt_size = ELT_BYTES;

    size_t total_size = n * field_elt_size;

    auto mem = allocate_memory(total_size);
    memcpy((void *)mem.get(), src, total_size);
    return mem;
}

template <const int R, const int C, typename B>
void prove(
        typename B::G1 **A_out,
        typename B::G2 **B_out,
        typename B::G1 **C_out,
        size_t primary_input_size,
        size_t d,
        size_t m,
        const var *w,
        // const var *A_mults,
        const var *B1_mults,
        const var *B2_mults,
        const var *L_mults,
        typename B::groth16_params *params,
        typename B::groth16_input *inputs)
{
    typedef typename ec_type<B>::ECpe ECpe;

    size_t space = ((m + 1) + R - 1) / R;

    //auto out_A = allocate_memory(space * ECpe::NELTS * ELT_BYTES);
    // auto A_mults_dev = copy_points_affine<ec_type<mnt4753_libsnark>::ECp>(((1U << C) - 1)*(m + 1), (void *) A_mults);

    auto out_B1 = allocate_memory(space * ECpe::NELTS * ELT_BYTES);
    auto B1_mults_dev = copy_points_affine<ec_type<mnt4753_libsnark>::ECp>(((1U << C) - 1)*(m + 1), (void *) B1_mults);

    auto out_B2 = allocate_memory(space * ECpe::NELTS * ELT_BYTES);
    auto B2_mults_dev = copy_points_affine<ec_type<mnt4753_libsnark>::ECpe>(((1U << C) - 1)*(m + 1), (void *) B2_mults);

    auto out_L = allocate_memory(space * ECpe::NELTS * ELT_BYTES);
    auto L_mults_dev = copy_points_affine<ec_type<mnt4753_libsnark>::ECp>(((1U << C) - 1)*(m - 1), (void *) L_mults);

    auto w_dev = copy_field_elts(m + 1, (void *) w);

    cudaStream_t //sA,
                 sB1, sB2, sL;

    typename B::vector_Fr *coefficients_for_H = NULL;
    typename B::vector_G1 *H = NULL;

    typename B::G1 *evaluation_Bt1 = NULL,
                   *evaluation_Ht = NULL,
                   *evaluation_Lt = NULL,
                   *scaled_Bt1 = NULL,
                   *Lt1_plus_scaled_Bt1 = NULL;

    prove_aux<R, C, B>(
        primary_input_size,
        d,
        m,
        w_dev.get(),
        now(),
        // A_mults_dev.get(),
        // out_A.get(),
        B1_mults_dev.get(),
        out_B1.get(),
        B2_mults_dev.get(),
        out_B2.get(),
        L_mults_dev.get(),
        out_L.get(),
        params,
        inputs,
        &coefficients_for_H,
        &H,
        A_out,
        &evaluation_Bt1,
        B_out,
        &evaluation_Ht,
        &evaluation_Lt,
        &scaled_Bt1,
        &Lt1_plus_scaled_Bt1,
        C_out,
        //sA,
        sB1,
        sB2,
        sL);

    //cudaStreamDestroy(sA);
    cudaStreamDestroy(sB1);
    cudaStreamDestroy(sB2);
    cudaStreamDestroy(sL);

    B::delete_vector_G1(H);

    B::delete_G1(evaluation_Bt1);
    B::delete_G1(evaluation_Ht);
    B::delete_G1(evaluation_Lt);
    B::delete_G1(scaled_Bt1);
    B::delete_G1(Lt1_plus_scaled_Bt1);
    B::delete_vector_Fr(coefficients_for_H);

    //B::groth16_output_write(evaluation_At, evaluation_Bt2, final_C, output_path);
}

void mnt4753_cuda_prove(
        typename mnt4753_libsnark::G1 **A_out,
        typename mnt4753_libsnark::G2 **B_out,
        typename mnt4753_libsnark::G1 **C_out,
        size_t primary_input_size,
        size_t d,
        size_t m,
        const var *w,
        // const var *A_mults,
        const var *B1_mults,
        const var *B2_mults,
        const var *L_mults,
        typename mnt4753_libsnark::groth16_params *params,
        typename mnt4753_libsnark::groth16_input *inputs) {
  static constexpr int R = 32;
  static constexpr int C = 5;
  prove<R, C, mnt4753_libsnark>(
    A_out,
    B_out,
    C_out,
    primary_input_size,
    d,
    m,
    w,
    // A_mults,
    B1_mults,
    B2_mults,
    L_mults,
    params,
    inputs);
}

void mnt6753_cuda_prove(
        typename mnt6753_libsnark::G1 **A_out,
        typename mnt6753_libsnark::G2 **B_out,
        typename mnt6753_libsnark::G1 **C_out,
        size_t primary_input_size,
        size_t d,
        size_t m,
        const var *w,
        // const var *A_mults,
        const var *B1_mults,
        const var *B2_mults,
        const var *L_mults,
        typename mnt6753_libsnark::groth16_params *params,
        typename mnt6753_libsnark::groth16_input *inputs) {
  static constexpr int R = 32;
  static constexpr int C = 5;
  prove<R, C, mnt6753_libsnark>(
    A_out,
    B_out,
    C_out,
    primary_input_size,
    d,
    m,
    w,
    // A_mults,
    B1_mults,
    B2_mults,
    L_mults,
    params,
    inputs);
}

extern "C" {

var *mnt4753_cuda_load_points_affine(size_t n, FILE *inputs) {
  return load_points_affine<ec_type<mnt4753_libsnark>::ECp>(n, inputs).release();
}

var *mnt4753_cuda_load_extension_points_affine(size_t n, FILE *inputs) {
  return load_points_affine<ec_type<mnt4753_libsnark>::ECpe>(n, inputs).release();
}

var *mnt6753_cuda_load_points_affine(size_t n, FILE *inputs) {
  return load_points_affine<ec_type<mnt6753_libsnark>::ECp>(n, inputs).release();
}

var *mnt6753_cuda_load_extension_points_affine(size_t n, FILE *inputs) {
  return load_points_affine<ec_type<mnt6753_libsnark>::ECpe>(n, inputs).release();
}

}

template <typename B>
void run_prover(
        const char *params_path,
        const char *input_path,
        const char *output_path,
        const char *preprocessed_path)
{
    B::init_public_params();

    size_t primary_input_size = 1;

    auto beginning = now();
    auto t = beginning;

    FILE *params_file = fopen(params_path, "r");
    size_t d = read_size_t(params_file);
    size_t m = read_size_t(params_file);
    rewind(params_file);

    printf("d = %zu, m = %zu\n", d, m);

    typedef typename ec_type<B>::ECp ECp;
    typedef typename ec_type<B>::ECpe ECpe;

    typedef typename B::G1 G1;
    typedef typename B::G2 G2;

    typedef typename B::vector_Fr vector_Fr;
    typedef typename B::vector_G1 vector_G1;

    static constexpr int R = 32;
    static constexpr int C = 5;
    FILE *preprocessed_file = fopen(preprocessed_path, "r");

    size_t space = ((m + 1) + R - 1) / R;

    //auto A_mults = load_points_affine<ECp>(((1U << C) - 1)*(m + 1), preprocessed_file);
    //auto out_A = allocate_memory(space * ECpe::NELTS * ELT_BYTES);

    auto B1_mults = load_points_affine<ECp>(((1U << C) - 1)*(m + 1), preprocessed_file);
    auto out_B1 = allocate_memory(space * ECpe::NELTS * ELT_BYTES);

    auto B2_mults = load_points_affine<ECpe>(((1U << C) - 1)*(m + 1), preprocessed_file);
    auto out_B2 = allocate_memory(space * ECpe::NELTS * ELT_BYTES);

    auto L_mults = load_points_affine<ECp>(((1U << C) - 1)*(m - 1), preprocessed_file);
    auto out_L = allocate_memory(space * ECpe::NELTS * ELT_BYTES);

    fclose(preprocessed_file);

    print_time(t, "load preprocessing");

    auto params = B::read_params(params_file, d, m);
    fclose(params_file);
    print_time(t, "load params");

    auto t_main = t;

    FILE *inputs_file = fopen(input_path, "r");
    auto w_ = load_scalars(m + 1, inputs_file);
    rewind(inputs_file);
    auto inputs = B::read_input(inputs_file, d, m);
    fclose(inputs_file);
    print_time(t, "load inputs");

    const var *w = w_.get();

    cudaStream_t //sA,
                 sB1, sB2, sL;

    vector_Fr *coefficients_for_H = NULL;
    vector_G1 *H = NULL;

    G1 *evaluation_At = NULL,
       *evaluation_Bt1 = NULL,
       *evaluation_Ht = NULL,
       *evaluation_Lt = NULL,
       *scaled_Bt1 = NULL,
       *Lt1_plus_scaled_Bt1 = NULL,
       *final_C = NULL;
    G2 *evaluation_Bt2 = NULL;

    prove_aux<R, C, B>(
        primary_input_size,
        d,
        m,
        w,
        t,
        // A_mults.get(),
        // out_A.get(),
        B1_mults.get(),
        out_B1.get(),
        B2_mults.get(),
        out_B2.get(),
        L_mults.get(),
        out_L.get(),
        params,
        inputs,
        &coefficients_for_H,
        &H,
        &evaluation_At,
        &evaluation_Bt1,
        &evaluation_Bt2,
        &evaluation_Ht,
        &evaluation_Lt,
        &scaled_Bt1,
        &Lt1_plus_scaled_Bt1,
        &final_C,
        //sA,
        sB1,
        sB2,
        sL);

    B::groth16_output_write(evaluation_At, evaluation_Bt2, final_C, output_path);

    print_time(t, "store");

    print_time(t_main, "Total time from input to output: ");

    //cudaStreamDestroy(sA);
    cudaStreamDestroy(sB1);
    cudaStreamDestroy(sB2);
    cudaStreamDestroy(sL);

    B::delete_vector_G1(H);

    B::delete_G1(evaluation_At);
    B::delete_G1(evaluation_Bt1);
    B::delete_G2(evaluation_Bt2);
    B::delete_G1(evaluation_Ht);
    B::delete_G1(evaluation_Lt);
    B::delete_G1(scaled_Bt1);
    B::delete_G1(Lt1_plus_scaled_Bt1);
    B::delete_vector_Fr(coefficients_for_H);
    B::delete_groth16_input(inputs);
    B::delete_groth16_params(params);

    print_time(t, "cleanup");
    print_time(beginning, "Total runtime (incl. file reads)");
}
