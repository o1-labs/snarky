project(cuda-fixnum CXX CUDA)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CUDA_STANDARD 11)
set(EXECUTABLE_OUTPUT_PATH "${PROJECT_BINARY_DIR}/bin")

# CUDA
find_package(CUDA REQUIRED)
set(CUDA_ARCH_LIST Auto CACHE LIST
    "List of CUDA architectures (e.g. Pascal, Volta, etc) or \
     compute capability versions (6.1, 7.0, etc) to generate code for. \
     Set to Auto for automatic detection (default)."
)
cuda_select_nvcc_arch_flags(CUDA_ARCH_FLAGS ${CUDA_ARCH_LIST})
string(REPLACE ";" " " CUDA_ARCH_FLAGS_SPACES "${CUDA_ARCH_FLAGS}")

string(APPEND CMAKE_CUDA_FLAGS " ${CUDA_ARCH_FLAGS_SPACES}")
string(APPEND CMAKE_CUDA_FLAGS " -Xcompiler -Wall,-Wextra")
set(CMAKE_CUDA_FLAGS_DEBUG "-g -G")

# source
include_directories(src)

# bench
add_executable(cuda_bench bench/bench.cu)
target_compile_definitions(cuda_bench PUBLIC -DNDEBUG)
target_link_libraries(cuda_bench -lstdc++)

add_library(cuda_main STATIC main.cu)
target_include_directories(cuda_main PUBLIC src)
set_property(TARGET cuda_main PROPERTY CUDA_SEPARABLE_COMPILATION ON)

# test-suite
find_package(GTest)
if(GTEST_FOUND)
  add_executable(cuda_check tests/test-suite.cu)
  target_link_libraries(cuda_check -lstdc++ -L${GTEST_INCLUDE_DIR} -lgtest)
  target_compile_options(cuda_check PUBLIC -lineinfo)
else()
  message(WARNING "GTest libraries not found - not building test-suite")
endif()

# FIXME: Add test runs
# enable_testing()
# add_test(NAME    mytest 
#         COMMAND  mytest)

# DEBUG CMAKE Variables
#get_cmake_property(_variableNames VARIABLES)
#list (SORT _variableNames)
#foreach (_variableName ${_variableNames})
#    message(STATUS "${_variableName}=${${_variableName}}")
#endforeach()
