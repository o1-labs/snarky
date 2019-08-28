#include <cstdio>
#include <cstring>
#include <cassert>
#include <vector>

#include "fixnum/warp_fixnum.cu"
#include "array/fixnum_array.h"
#include "functions/modexp.cu"
#include "functions/multi_modexp.cu"
#include "modnum/modnum_monty_redc.cu"
#include "modnum/modnum_monty_cios.cu"

const unsigned int bytes_per_elem = 128;
const unsigned int io_bytes_per_elem = 96;


using namespace std;
using namespace cuFIXNUM;

template< typename fixnum >
struct mul_and_convert {
  // redc may be worth trying over cios
  typedef modnum_monty_cios<fixnum> modnum;
  __device__ void operator()(fixnum &r, fixnum a, fixnum b, fixnum my_mod) {
      modnum mod = modnum(my_mod);

      fixnum sm;
      mod.mul(sm, a, b);

      fixnum s;
      mod.from_modnum(s, sm);

      r = s;
  }
};

template< int fn_bytes, typename fixnum_array >
void print_fixnum_array(fixnum_array* res, int nelts) {
    int lrl = fn_bytes*nelts;
    uint8_t local_results[lrl];
    int ret_nelts;
    for (int i = 0; i < lrl; i++) {
      local_results[i] = 0;
    }
    res->retrieve_all(local_results, fn_bytes*nelts, &ret_nelts);

    for (int i = 0; i < lrl; i++) {
      printf("%i ", local_results[i]);
    }
    printf("\n");
}

template< int fn_bytes, typename fixnum_array >
vector<uint8_t*> get_fixnum_array(fixnum_array* res, int nelts) {
    int lrl = fn_bytes*nelts;
    uint8_t local_results[lrl];
    int ret_nelts;
    for (int i = 0; i < lrl; i++) {
      local_results[i] = 0;
    }
    res->retrieve_all(local_results, fn_bytes*nelts, &ret_nelts);
    vector<uint8_t*> res_v;
    for (int n = 0; n < nelts; n++) {
      uint8_t* a = (uint8_t*)malloc(fn_bytes*sizeof(uint8_t));
      for (int i = 0; i < fn_bytes; i++) {
        a[i] = local_results[n*fn_bytes + i];
      }
      res_v.emplace_back(a);
    }
    return res_v;
}


template< int fn_bytes, typename word_fixnum, template <typename> class Func >
std::vector<uint8_t*> compute_product(std::vector<uint8_t*> a, std::vector<uint8_t*> b, uint8_t* input_m_base) {
    typedef warp_fixnum<fn_bytes, word_fixnum> fixnum;
    typedef fixnum_array<fixnum> fixnum_array;

    int nelts = a.size();

    uint8_t *input_a = new uint8_t[fn_bytes * nelts];
    for (int i = 0; i < fn_bytes * nelts; ++i) {
      input_a[i] = a[i/fn_bytes][i%fn_bytes];
    }

    uint8_t *input_b = new uint8_t[fn_bytes * nelts];
    for (int i = 0; i < fn_bytes * nelts; ++i) {
      input_b[i] = b[i/fn_bytes][i%fn_bytes];
    }

    uint8_t *input_m = new uint8_t[fn_bytes * nelts];
    for (int i = 0; i < fn_bytes * nelts; ++i) {
      input_m[i] = input_m_base[i%fn_bytes];
    }

    // TODO reuse modulus as a constant instead of passing in nelts times
    fixnum_array *res, *in_a, *in_b, *inM;
    in_a = fixnum_array::create(input_a, fn_bytes * nelts, fn_bytes);
    in_b = fixnum_array::create(input_b, fn_bytes * nelts, fn_bytes);
    inM = fixnum_array::create(input_m, fn_bytes * nelts, fn_bytes);
    res = fixnum_array::create(nelts);

    fixnum_array::template map<Func>(res, in_a, in_b, inM);

    vector<uint8_t*> v_res = get_fixnum_array<fn_bytes, fixnum_array>(res, nelts);

    //TODO to do stage 1 field arithmetic, instead of a map, do a reduce

    delete in_a;
    delete in_b;
    delete inM;
    delete res;
    delete[] input_a;
    delete[] input_b;
    delete[] input_m;
    return v_res;
}

uint8_t* read_mnt_fq(FILE* inputs) {
  uint8_t* buf = (uint8_t*)calloc(bytes_per_elem, sizeof(uint8_t));
  // the input is montgomery representation x * 2^768 whereas cuda-fixnum expects x * 2^1024 so we shift over by (1024-768)/8 bytes
  fread((void*)( buf + (bytes_per_elem - io_bytes_per_elem)), io_bytes_per_elem*sizeof(uint8_t), 1, inputs);
  return buf;
}

void write_mnt_fq(uint8_t* fq, FILE* outputs) {
  fwrite((void *) fq, io_bytes_per_elem * sizeof(uint8_t), 1, outputs);
}

int do_fixnum_example(const char *inputs_file, const char *outputs_file) {
  setbuf(stdout, NULL);

  // mnt4_q
  uint8_t mnt4_modulus[bytes_per_elem] = {1,128,94,36,222,99,144,94,159,17,221,44,82,84,157,227,240,37,196,154,113,16,136,99,164,84,114,118,233,204,90,104,56,126,83,203,165,13,15,184,157,5,24,242,118,231,23,177,157,247,90,161,217,36,209,153,141,237,160,232,37,185,253,7,115,216,151,108,249,232,183,94,237,175,143,91,80,151,249,183,173,205,226,238,34,144,34,16,17,196,146,45,198,196,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

  // mnt6_q
  uint8_t mnt6_modulus[bytes_per_elem] = {1,0,0,64,226,118,7,217,79,58,161,15,23,153,160,78,151,87,0,63,188,129,195,214,164,58,153,52,118,249,223,185,54,38,33,41,148,202,235,62,155,169,89,200,40,92,108,178,157,247,90,161,217,36,209,153,141,237,160,232,37,185,253,7,115,216,151,108,249,232,183,94,237,175,143,91,80,151,249,183,173,205,226,238,34,144,34,16,17,196,146,45,198,196,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

  auto inputs = fopen(inputs_file, "r");
  auto outputs = fopen(outputs_file, "w");

  size_t n;

   while (true) {
    size_t elts_read = fread((void *) &n, sizeof(size_t), 1, inputs);
    if (elts_read == 0) { break; }

    std::vector<uint8_t*> x0;
    for (size_t i = 0; i < n/2; ++i) {
      x0.emplace_back(read_mnt_fq(inputs));
    }

    std::vector<uint8_t*> x1;
    for (size_t i = 0; i < n/2; ++i) {
      x1.emplace_back(read_mnt_fq(inputs));
    }

    std::vector<uint8_t*> res_x = compute_product<bytes_per_elem, u64_fixnum, mul_and_convert>(x0, x1, mnt4_modulus);

    for (size_t i = 0; i < n/2; ++i) {
      write_mnt_fq(res_x[i], outputs);
    }

    std::vector<uint8_t*> y0;
    for (size_t i = 0; i < n/2; ++i) {
      y0.emplace_back(read_mnt_fq(inputs));
    }

    std::vector<uint8_t*> y1;
    for (size_t i = 0; i < n/2; ++i) {
      y1.emplace_back(read_mnt_fq(inputs));
    }

    std::vector<uint8_t*> res_y = compute_product<bytes_per_elem, u64_fixnum, mul_and_convert>(y0, y1, mnt6_modulus);

    for (size_t i = 0; i < n/2; ++i) {
      write_mnt_fq(res_y[i], outputs);
    }

    for (size_t i = 0; i < n/2; ++i) {
      free(x0[i]);
      free(x1[i]);
      free(y0[i]);
      free(y1[i]);
      free(res_x[i]);
      free(res_y[i]);
    }

  }

  return 0;
}

