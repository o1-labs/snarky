open Core_kernel
open Camlsnark_c_bindings
open Cstubs

module Common (F : Ctypes.FOREIGN) = struct
  open Bindings.Common

  module Bn128 =
    Bind (struct
        let prefix = "camlsnark_bn128"
      end)
      (F)

  module Mnt4 =
    Bind (struct
        let prefix = "camlsnark_mnt4"
      end)
      (F)

  module Mnt6 =
    Bind (struct
        let prefix = "camlsnark_mnt6"
      end)
      (F)

  module Mnt4753 =
    Bind (struct
        let prefix = "camlsnark_mnt4753"
      end)
      (F)

  module Mnt6753 =
    Bind (struct
        let prefix = "camlsnark_mnt6753"
      end)
      (F)
end

let with_formatter name ~f =
  Out_channel.with_file name ~f:(fun out ->
      let fmt = Format.formatter_of_out_channel out in
      f fmt )

let () =
  with_formatter "libsnark_ffi_bindings.c" ~f:(fun fmt ->
      Format.pp_print_string fmt
        {c|
#define CURVE_PREFIX(name) camlsnark_bn128_ ## name

#include "caml_curve.h.template"

#undef CURVE_PREFIX

#define CURVE_PREFIX(name) camlsnark_mnt4_ ## name

#include "caml_curve.h.template"

#undef CURVE_PREFIX

#define CURVE_PREFIX(name) camlsnark_mnt6_ ## name

#include "caml_curve.h.template"

#undef CURVE_PREFIX

#define CURVE_PREFIX(name) camlsnark_mnt4753_ ## name

#include "caml_curve.h.template"

#undef CURVE_PREFIX

#define CURVE_PREFIX(name) camlsnark_mnt6753_ ## name

#include "caml_curve.h.template"

#undef CURVE_PREFIX
|c} ;
      write_c ~prefix:"snarky_common" fmt (module Common) ) ;
  with_formatter "libsnark_ffi_bindings.ml" ~f:(fun fmt ->
      Format.pp_print_string fmt "[@@@warning \"-11\"]\n" ;
      write_ml ~prefix:"snarky_common" fmt (module Common) )
