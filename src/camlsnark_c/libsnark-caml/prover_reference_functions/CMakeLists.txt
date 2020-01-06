add_library (
    prover_reference_functions STATIC
    prover_reference_functions.cpp
    prover_reference_functions.hpp
)

set (CMAKE_CXX_STANDARD 11)

target_link_libraries (
    prover_reference_functions

    snark
)
