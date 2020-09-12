open Core_kernel
open Camlsnark_c_bindings
open Cstubs

let with_formatter name ~f =
  Out_channel.with_file name ~f:(fun out ->
      let fmt = Format.formatter_of_out_channel out in
      f fmt )

let () =
  with_formatter "vector_ffi_bindings.c" ~f:(fun fmt ->
      Format.pp_print_string fmt {c|
#include "common.h"
#include "logging.h"
|c} ;
      write_c ~prefix:"snarky_vector" fmt (module Vector.Bindings) ) ;
  with_formatter "vector_ffi_bindings.ml" ~f:(fun fmt ->
      write_ml ~prefix:"snarky_vector" fmt (module Vector.Bindings) )
