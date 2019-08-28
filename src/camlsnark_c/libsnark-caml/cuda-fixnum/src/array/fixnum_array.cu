// for printing arrays
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
// for min
#include <algorithm>
// for assert
#include <cassert>

#include "util/cuda_wrap.h"
#include "fixnum_array.h"

namespace cuFIXNUM {

// TODO: The only device function in this file is the dispatch kernel
// mechanism, which could arguably be placed elsewhere, thereby
// allowing this file to be compiled completely for the host.

// Notes: Read programming guide Section K.3
// - Can prefetch unified memory
// - Can advise on location of unified memory

// TODO: Can I use smart pointers? unique_ptr?

// TODO: Clean this up
namespace {
    typedef std::uint8_t byte;

    template< typename T >
    static byte *as_byte_ptr(T *ptr) {
        return reinterpret_cast<byte *>(ptr);
    }

    template< typename T >
    static const byte *as_byte_ptr(const T *ptr) {
        return reinterpret_cast<const byte *>(ptr);
    }

    // TODO: refactor from word_fixnum.
    template< typename T >
    T ceilquo(T n, T d) {
        return (n + d - 1) / d;
    }
}

template< typename fixnum >
fixnum_array<fixnum> *
fixnum_array<fixnum>::create(size_t nelts) {
    fixnum_array *a = new fixnum_array;
    a->nelts = nelts;
    if (nelts > 0) {
        size_t nbytes = nelts * fixnum::BYTES;
        cuda_malloc_managed(&a->ptr, nbytes);
    }
    return a;
}

template< typename fixnum >
template< typename T >
fixnum_array<fixnum> *
fixnum_array<fixnum>::create(size_t nelts, T init) {
    fixnum_array *a = create(nelts);
    byte *p = as_byte_ptr(a->ptr);

    const byte *in = as_byte_ptr(&init);
    byte elt[fixnum::BYTES];
    memset(elt, 0, fixnum::BYTES);
    std::copy(in, in + sizeof(T), elt);

    for (uint32_t i = 0; i < nelts; ++i, p += fixnum::BYTES)
        fixnum::from_bytes(p, elt, fixnum::BYTES);
    return a;
}

template< typename fixnum >
fixnum_array<fixnum> *
fixnum_array<fixnum>::create(const byte *data, size_t total_bytes, size_t bytes_per_elt) {
    // FIXME: Should handle this error more appropriately
    if (total_bytes == 0 || bytes_per_elt == 0)
        return nullptr;

    size_t nelts = ceilquo(total_bytes, bytes_per_elt);
    fixnum_array *a = create(nelts);

    byte *p = as_byte_ptr(a->ptr);
    const byte *d = data;
    for (size_t i = 0; i < nelts; ++i) {
        fixnum::from_bytes(p, d, bytes_per_elt);
        p += fixnum::BYTES;
        d += bytes_per_elt;
    }
    return a;
}

// TODO: This doesn't belong here.
template< typename digit >
void
rotate_array(digit *out, const digit *in, int nelts, int words_per_elt, int i) {
    if (i < 0) {
        int j = -i;
        i += nelts * ceilquo(j, nelts);
        assert(i >= 0 && i < nelts);
        i = nelts - i;
    } else if (i >= nelts)
        i %= nelts;
    int pivot = i * words_per_elt;
    int nwords = nelts * words_per_elt;
    std::copy(in, in + nwords - pivot, out + pivot);
    std::copy(in + nwords - pivot, in + nwords, out);
}


// TODO: Find a way to return a wrapper that just modifies the requested indices
// on the fly, rather than copying the whole array. Hard part will be making it
// work with map/dispatch.
template< typename fixnum >
fixnum_array<fixnum> *
fixnum_array<fixnum>::rotate(int i) {
    fixnum_array *a = create(length());
    byte *p = as_byte_ptr(a->ptr);
    const byte *q = as_byte_ptr(ptr);
    rotate_array(p, q, nelts, fixnum::BYTES, i);
    return a;
}

template< typename fixnum >
fixnum_array<fixnum> *
fixnum_array<fixnum>::repeat(int ntimes) {
    fixnum_array *a = create(length() * ntimes);
    byte *p = as_byte_ptr(a->ptr);
    const byte *q = as_byte_ptr(ptr);
    int nbytes = nelts * fixnum::BYTES;
    for (int i = 0; i < ntimes; ++i, p += nbytes)
        std::copy(q, q + nbytes, p);
    return a;
}

template< typename fixnum >
fixnum_array<fixnum> *
fixnum_array<fixnum>::rotations(int ntimes) {
    fixnum_array *a = create(nelts * ntimes);
    byte *p = as_byte_ptr(a->ptr);
    const byte *q = as_byte_ptr(ptr);
    int nbytes = nelts * fixnum::BYTES;
    for (int i = 0; i < ntimes; ++i, p += nbytes)
        rotate_array(p, q, nelts, fixnum::BYTES, i);
    return a;
}


template< typename fixnum >
int
fixnum_array<fixnum>::set(int idx, const byte *data, size_t nbytes) {
    // FIXME: Better error handling
    if (idx < 0 || idx >= nelts)
        return -1;

    int off = idx * fixnum::BYTES;
    const byte *q = as_byte_ptr(ptr);
    return fixnum::from_bytes(q + off, data, nbytes);
}

template< typename fixnum >
fixnum_array<fixnum>::~fixnum_array() {
    if (nelts > 0)
        cuda_free(ptr);
}

template< typename fixnum >
int
fixnum_array<fixnum>::length() const {
    return nelts;
}

template< typename fixnum >
size_t
fixnum_array<fixnum>::retrieve_into(byte *dest, size_t dest_space, int idx) const {
    if (idx < 0 || idx > nelts) {
        // FIXME: This is not the right way to handle an "index out of
        // bounds" error.
        return 0;
    }
    const byte *q = as_byte_ptr(ptr);
    return fixnum::to_bytes(dest, dest_space, q + idx * fixnum::BYTES);
}

// FIXME: Can return fewer than nelts elements.
template< typename fixnum >
void
fixnum_array<fixnum>::retrieve_all(byte *dest, size_t dest_space, int *dest_nelts) const {
    const byte *p = as_byte_ptr(ptr);
    byte *d = dest;
    int max_dest_nelts = dest_space / fixnum::BYTES;
    *dest_nelts = std::min(nelts, max_dest_nelts);
    for (int i = 0; i < *dest_nelts; ++i) {
        fixnum::to_bytes(d, fixnum::BYTES, p);
        p += fixnum::BYTES;
        d += fixnum::BYTES;
    }
}

namespace {
    std::string
    fixnum_as_str(const uint8_t *fn, int nbytes) {
        std::ostringstream ss;

        for (int i = nbytes - 1; i >= 0; --i) {
            // These IO manipulators are forgotten after each use;
            // i.e. they don't apply to the next output operation (whether
            // it be in the next loop iteration or in the conditional
            // below.
            ss << std::setfill('0') << std::setw(2) << std::hex;
            ss << static_cast<int>(fn[i]);
            if (i && !(i & 3))
                ss << ' ';
        }
        return ss.str();
    }
}

template< typename fixnum >
std::ostream &
operator<<(std::ostream &os, const fixnum_array<fixnum> *fn_arr) {
    constexpr int fn_bytes = fixnum::BYTES;
    constexpr size_t bufsz = 4096;
    uint8_t arr[bufsz];
    int nelts;

    fn_arr->retrieve_all(arr, bufsz, &nelts);
    os << "( ";
    if (nelts < fn_arr->length()) {
        os << "insufficient space to retrieve array";
    } else if (nelts > 0) {
        os << fixnum_as_str(arr, fn_bytes);
        for (int i = 1; i < nelts; ++i)
            os << ", " << fixnum_as_str(arr + i*fn_bytes, fn_bytes);
    }
    os << " )" << std::flush;
    return os;
}


template< template <typename> class Func, typename fixnum, typename... Args >
__global__ void
dispatch(int nelts, Args... args) {
    // Get the slot index for the current thread.
    int blk_tid_offset = blockDim.x * blockIdx.x;
    int tid_in_blk = threadIdx.x;
    int idx = (blk_tid_offset + tid_in_blk) / fixnum::SLOT_WIDTH;

    if (idx < nelts) {
        // TODO: Find a way to load each argument into a register before passing
        // it to fn, and then unpack the return values where they belong. This
        // will guarantee that all operations happen on registers, rather than
        // inadvertently operating on memory.

        Func<fixnum> fn;
        // TODO: This offset calculation is entwined with fixnum layout and so
        // belongs somewhere else.
        int off = idx * fixnum::layout::WIDTH + fixnum::layout::laneIdx();
        // TODO: This is hiding a sin against memory aliasing / management /
        // type-safety.
        fn(args[off]...);
    }
}

template< typename fixnum >
template< template <typename> class Func, typename... Args >
void
fixnum_array<fixnum>::map(Args... args) {
    // TODO: Set this to the number of threads on a single SM on the host GPU.
    constexpr int BLOCK_SIZE = 192;

    // FIXME: WARPSIZE should come from slot_layout
    constexpr int WARPSIZE = 32;
    // BLOCK_SIZE must be a multiple of warpSize
    static_assert(!(BLOCK_SIZE % WARPSIZE),
            "block size must be a multiple of warpSize");

    int nelts = std::min( { args->length()... } );

    constexpr int fixnums_per_block = BLOCK_SIZE / fixnum::SLOT_WIDTH;

    // FIXME: nblocks could be too big for a single kernel call to handle
    int nblocks = ceilquo(nelts, fixnums_per_block);

    // nblocks > 0 iff nelts > 0
    if (nblocks > 0) {
        cudaStream_t stream;
        cuda_check(cudaStreamCreate(&stream), "create stream");
//         cuda_stream_attach_mem(stream, src->ptr);
//         cuda_stream_attach_mem(stream, ptr);
        cuda_check(cudaStreamSynchronize(stream), "stream sync");

        dispatch<Func, fixnum ><<< nblocks, BLOCK_SIZE, 0, stream >>>(nelts, args->ptr...);

        cuda_check(cudaPeekAtLastError(), "kernel invocation/run");
        cuda_check(cudaStreamSynchronize(stream), "stream sync");
        cuda_check(cudaStreamDestroy(stream), "stream destroy");

        // FIXME: Only synchronize when retrieving data from array
        cuda_device_synchronize();
    }
}

} // End namespace cuFIXNUM
