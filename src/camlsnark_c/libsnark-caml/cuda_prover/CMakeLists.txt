cmake_minimum_required(VERSION 3.8.0)
enable_language(CUDA)
project(cuda-fixnum CXX CUDA)

# Configure CCache if available
find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
        set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
        set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif(CCACHE_FOUND)

add_library (
    caml_cuda_prover

    ../libsnark/serialization.cpp
    ../prover_reference_functions/prover_reference_functions.cpp
    cuda_prover_piecewise.cu
    caml_cuda_prover.cpp
)

target_include_directories(
  caml_cuda_prover

  PUBLIC
  ../depends/libff
  ../depends/libfqfft
  ../prover_reference_functions
  ../libsnark
)

add_executable (
    cuda_prover_main
    EXCLUDE_FROM_ALL

    ../libsnark/serialization.cpp
    cuda_prover_main.cu
)
target_link_libraries(cuda_prover_main cuda_main prover_reference_functions)
