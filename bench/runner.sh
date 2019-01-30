#!/usr/bin/env sh
export BENCHMARKS_RUNNER=TRUE
export BENCH_LIB=dune_bench
exec dune exec -- ./main.exe -run-without-cross-library-inlining "$@"
