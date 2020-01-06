cmake_minimum_required(VERSION 2.8)

project (libsnark-caml)

add_subdirectory(libsnark)

add_subdirectory(prover_reference_functions)

add_subdirectory(caml-bindings)

find_package(CUDA)
if(CUDA_FOUND)
  add_subdirectory(cuda-fixnum)
  add_subdirectory(cuda_prover)
endif()
