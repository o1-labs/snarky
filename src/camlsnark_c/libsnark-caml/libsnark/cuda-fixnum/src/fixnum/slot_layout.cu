#pragma once

#include <cstdint>

namespace cuFIXNUM {

// For some reason the warpSize value provided by CUDA is not
// considered a constant value, so cannot be used in constexprs or
// template parameters or static_asserts. Hence we must use WARPSIZE
// instead.
static constexpr int WARPSIZE = 32;

// TODO: Tidy up nomenclature: SUBWARP -> Slot
/*
 * SUBWARPS:
 *
 * Most of these functions operate on the level of a "subwarp" (NB:
 * this is not standard terminology).  A *warp* is a logical block of
 * 32 threads executed in lock-step by the GPU (thus obviating the
 * need for explicit synchronisation). For any w > 1 that divides 32,
 * a warp can be partitioned into 32/w subwarps of w threads.  The
 * struct below takes a parameter "width" which specifies the subwarp
 * size, and which thereby specifies the size of the numbers on which
 * its functions operate.
 *
 * The term "warp" should be reserved for subwarps of width 32
 * (=warpSize).
 *
 * TODO: Work out if using __forceinline__ in these definitions
 * actually achieves anything.
 */

template<typename T, int width = WARPSIZE>
struct slot_layout
{
    static_assert(width > 0 && !(WARPSIZE & (width - 1)),
        "slot width must be a positive divisor of warpSize (=32)");

    static constexpr int WIDTH = width;

    /*
     * Return the lane index within the slot.
     *
     * The lane index is the thread index modulo the width of the slot.
     */
    static __device__ __forceinline__
    int
    laneIdx() {
        // threadIdx.x % width = threadIdx.x & (width - 1) since width = 2^n
        return threadIdx.x & (width - 1);

        // TODO: Replace above with?
        // int L;
        // asm ("mov.b32 %0, %laneid;" : "=r"(L));
        // return L;
    }

    /*
     * Index of the top lane of the current slot.
     *
     * The top lane of a slot is the one with index width - 1.
     */
    static constexpr int toplaneIdx = width - 1;

    /*
     * Mask which selects the first width bits of a number.
     *
     * Useful in conjunction with offset() and __ballot().
     */
    static __device__ __forceinline__
    std::uint32_t
    mask() {
        return ((1UL << width) - 1UL) << offset();
    }

    /*
     * Return the thread index within the warp where the slot
     * containing this lane begins.  Examples:
     *
     * - width 16: slot offset is 0 for threads 0-15, and 16 for
     *    threads 16-31
     *
     * - width 8: slot offset is 0 for threads 0-7, 8 for threads 8-15,
     *    16 for threads 16-23, and 24 for threads 24-31.
     *
     * The slot offset at thread T in a slot of width w is given by
     * floor(T/w)*w.
     *
     * Useful in conjunction with mask() and __ballot().
     */
    static __device__ __forceinline__
    int
    offset() {
        // Thread index within the (full) warp.
        int tid = threadIdx.x & (WARPSIZE - 1);

        // Recall: x mod y = x - y*floor(x/y), so
        //
        //   slotOffset = width * floor(threadIdx/width)
        //                 = threadIdx - (threadIdx % width)
        //                 = threadIdx - (threadIdx & (width - 1))
        //                 // TODO: Do use this last formulation!
        //                 = set bottom log2(width) bits of threadIdx to zero
        //                 = T & ~mask ??  or "(T >> width) << width"
        //
        // since width = 2^n.
        return tid - (tid & (width - 1));
    }

    /*
     * Like ballot(tst) but restrict the result to the containing slot
     * of size width.
     */
    __device__ __forceinline__
    static uint32_t
    ballot(int tst) {
        uint32_t b = __ballot_sync(mask(), tst);
        return b >> offset();
    }

    /*
     * Wrappers for notation consistency.
     */
    __device__ __forceinline__
    static T
    shfl(T var, int srcLane) {
        return __shfl_sync(mask(), var, srcLane, width);
    }

    __device__ __forceinline__
    static T
    shfl_up(T var, unsigned int delta) {
        return __shfl_up_sync(mask(), var, delta, width);
    }

    __device__ __forceinline__
    static T
    shfl_down(T var, unsigned int delta) {
        return __shfl_down_sync(mask(), var, delta, width);
    }

    // NB: Assumes delta <= width + L. (There should be no reason for
    // it ever to be more than width.)
    __device__ __forceinline__
    static T
    rotate_up(T var, unsigned int delta) {
        int L = laneIdx();
        // Don't need to reduce srcLane modulo width; that is done by __shfl.
        int srcLane = L + width - delta; //  The +width is to ensure srcLane > 0
        return shfl(var, srcLane);
    }

    __device__ __forceinline__
    static T
    rotate_down(T var, unsigned int delta) {
        int L = laneIdx();
        // Don't need to reduce srcLane modulo width; that is done by __shfl.
        int srcLane = L + delta;
        return shfl(var, srcLane);
    }

    /*
     * Like shfl_up but set bottom delta variables to zero.
     */
    __device__ __forceinline__
    static T
    shfl_up0(T var, unsigned int delta) {
        T res = shfl_up(var, delta);
        //return res & -(T)(laneIdx() > 0);
        return laneIdx() < delta ? T(0) : res;
    }

    /*
     * Like shfl_down but set top delta variables to zero.
     */
    __device__ __forceinline__
    static T
    shfl_down0(T var, unsigned int delta) {
        T res = shfl_down(var, delta);
        //return res & -(T)(laneIdx() < toplaneIdx());
        return laneIdx() >= (width - delta) ? T(0) : res;
    }

private:
    slot_layout();
};

} // End namespace cuFIXNUM
