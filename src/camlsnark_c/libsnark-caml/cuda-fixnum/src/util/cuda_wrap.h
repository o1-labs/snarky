#pragma once

#include <cstdio>
#include <cstdlib>

namespace cuFIXNUM {

/*
 * Convenience wrappers around some CUDA library functions
 */
static inline void
cuda_print_errmsg(cudaError err, const char *msg, const char *file, const int line)
{
    if (err != cudaSuccess) {
        fprintf(stderr, "Fatal CUDA error at %s:%d : %s : %s\n",
                file, line, msg, cudaGetErrorString(err));
        if (cudaDeviceReset() != cudaSuccess)
            fprintf(stderr, "   ...and failed to reset the device!\n");
        exit(EXIT_FAILURE);
    }
}

} // End namespace cuFIXNUM

#define cuda_check(err, msg)                            \
    ::cuFIXNUM::cuda_print_errmsg(err, msg, __FILE__, __LINE__)

#define cuda_malloc(ptr, size)                                  \
    cuda_check(cudaMalloc(ptr, size), "memory allocation")
#define cuda_malloc_managed(ptr, size)                                  \
    cuda_check(cudaMallocManaged(ptr, size),                            \
            "unified memory allocation (default attach)")
#define cuda_malloc_managed_host(ptr, size)                             \
    cuda_check(cudaMallocManaged(ptr, size, cudaMemAttachHost),         \
            "unified memory allocation (host attach)")
#define cuda_stream_attach_mem(stream, ptr)                             \
    cuda_check(cudaStreamAttachMemAsync(stream, ptr), "attach unified memory to stream")
#define cuda_free(ptr)                                  \
    cuda_check(cudaFree(ptr), "memory deallocation")
#define cuda_memcpy_to_device(dest, src, size)                          \
    cuda_check(cudaMemcpy(dest, src, size, cudaMemcpyHostToDevice), "copy to device")
#define cuda_memcpy_from_device(dest, src, size)                        \
    cuda_check(cudaMemcpy(dest, src, size, cudaMemcpyDeviceToHost), "copy from device")
#define cuda_memcpy_on_device(dest, src, size)                        \
    cuda_check(cudaMemcpy(dest, src, size, cudaMemcpyDeviceToDevice), "copy on device")
#define cuda_memset(dest, val, size)                        \
    cuda_check(cudaMemset(dest, val, size), "memset on device")
#define cuda_device_synchronize() \
    cuda_check(cudaDeviceSynchronize(), "device synchronize")

