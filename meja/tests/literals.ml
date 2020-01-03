module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module Int = struct
  let decimal_expr = 123456789

  let decimal_expr_neg = -123456789

  let decimal_expr_pos = 123456789

  let decimal_expr_sep = 1234567

  let binary_expr_neg = -170

  let binary_expr_pos = 170

  let binary_expr_upper = 170

  let binary_expr_sep = 85

  let octal_expr_neg = -2739128

  let octal_expr_pos = 2739128

  let octal_expr_upper = 2739128

  let octal_expr_sep = 342391

  let hex_expr_neg = -267242409

  let hex_expr_pos = 267242409

  let hex_expr_upper = 267242409

  let hex_expr_sep = 19088470

  let neg_pos_reduce = -1234

  let decimal_pat x = match x with 123456789 -> true | _ -> false

  let decimal_pat_neg x = match x with -123456789 -> true | _ -> false

  let decimal_pat_pos x = match x with 123456789 -> true | _ -> false

  let decimal_pat_sep x = match x with 1234567 -> true | _ -> false

  let binary_pat_neg x = match x with -170 -> true | _ -> false

  let binary_pat_pos x = match x with 170 -> true | _ -> false

  let binary_pat_upper x = match x with 170 -> true | _ -> false

  let binary_pat_sep x = match x with 85 -> true | _ -> false

  let octal_pat_neg x = match x with -2739128 -> true | _ -> false

  let octal_pat_pos x = match x with 2739128 -> true | _ -> false

  let octal_pat_upper x = match x with 2739128 -> true | _ -> false

  let octal_pat_sep x = match x with 342391 -> true | _ -> false

  let hex_pat_neg x = match x with -267242409 -> true | _ -> false

  let hex_pat_pos x = match x with 267242409 -> true | _ -> false

  let hex_pat_upper x = match x with 267242409 -> true | _ -> false

  let hex_pat_sep x = match x with 19088470 -> true | _ -> false
end

module Int32 = struct
  let decimal_expr = 123456789l

  let decimal_expr_neg = -123456789l

  let decimal_expr_pos = 123456789l

  let decimal_expr_sep = 1234567l

  let binary_expr_neg = -170l

  let binary_expr_pos = 170l

  let binary_expr_upper = 170l

  let binary_expr_sep = 85l

  let octal_expr_neg = -2739128l

  let octal_expr_pos = 2739128l

  let octal_expr_upper = 2739128l

  let octal_expr_sep = 342391l

  let hex_expr_neg = -267242409l

  let hex_expr_pos = 267242409l

  let hex_expr_upper = 267242409l

  let hex_expr_sep = 19088470l

  let neg_pos_reduce = -1234l

  let decimal_pat x = match x with 123456789l -> true | _ -> false

  let decimal_pat_neg x = match x with -123456789l -> true | _ -> false

  let decimal_pat_pos x = match x with 123456789l -> true | _ -> false

  let decimal_pat_sep x = match x with 1234567l -> true | _ -> false

  let binary_pat_neg x = match x with -170l -> true | _ -> false

  let binary_pat_pos x = match x with 170l -> true | _ -> false

  let binary_pat_upper x = match x with 170l -> true | _ -> false

  let binary_pat_sep x = match x with 85l -> true | _ -> false

  let octal_pat_neg x = match x with -2739128l -> true | _ -> false

  let octal_pat_pos x = match x with 2739128l -> true | _ -> false

  let octal_pat_upper x = match x with 2739128l -> true | _ -> false

  let octal_pat_sep x = match x with 342391l -> true | _ -> false

  let hex_pat_neg x = match x with -267242409l -> true | _ -> false

  let hex_pat_pos x = match x with 267242409l -> true | _ -> false

  let hex_pat_upper x = match x with 267242409l -> true | _ -> false

  let hex_pat_sep x = match x with 19088470l -> true | _ -> false
end

module Int64 = struct
  let decimal_expr = 123456789L

  let decimal_expr_neg = -123456789L

  let decimal_expr_pos = 123456789L

  let decimal_expr_sep = 1234567L

  let binary_expr_neg = -170L

  let binary_expr_pos = 170L

  let binary_expr_upper = 170L

  let binary_expr_sep = 85L

  let octal_expr_neg = -2739128L

  let octal_expr_pos = 2739128L

  let octal_expr_upper = 2739128L

  let octal_expr_sep = 342391L

  let hex_expr_neg = -267242409L

  let hex_expr_pos = 267242409L

  let hex_expr_upper = 267242409L

  let hex_expr_sep = 19088470L

  let neg_pos_reduce = -1234L

  let decimal_pat x = match x with 123456789L -> true | _ -> false

  let decimal_pat_neg x = match x with -123456789L -> true | _ -> false

  let decimal_pat_pos x = match x with 123456789L -> true | _ -> false

  let decimal_pat_sep x = match x with 1234567L -> true | _ -> false

  let binary_pat_neg x = match x with -170L -> true | _ -> false

  let binary_pat_pos x = match x with 170L -> true | _ -> false

  let binary_pat_upper x = match x with 170L -> true | _ -> false

  let binary_pat_sep x = match x with 85L -> true | _ -> false

  let octal_pat_neg x = match x with -2739128L -> true | _ -> false

  let octal_pat_pos x = match x with 2739128L -> true | _ -> false

  let octal_pat_upper x = match x with 2739128L -> true | _ -> false

  let octal_pat_sep x = match x with 342391L -> true | _ -> false

  let hex_pat_neg x = match x with -267242409L -> true | _ -> false

  let hex_pat_pos x = match x with 267242409L -> true | _ -> false

  let hex_pat_upper x = match x with 267242409L -> true | _ -> false

  let hex_pat_sep x = match x with 19088470L -> true | _ -> false
end

module Nativeint = struct
  let decimal_expr = 123456789n

  let decimal_expr_neg = -123456789n

  let decimal_expr_pos = 123456789n

  let decimal_expr_sep = 1234567n

  let binary_expr_neg = -170n

  let binary_expr_pos = 170n

  let binary_expr_upper = 170n

  let binary_expr_sep = 85n

  let octal_expr_neg = -2739128n

  let octal_expr_pos = 2739128n

  let octal_expr_upper = 2739128n

  let octal_expr_sep = 342391n

  let hex_expr_neg = -267242409n

  let hex_expr_pos = 267242409n

  let hex_expr_upper = 267242409n

  let hex_expr_sep = 19088470n

  let neg_pos_reduce = -1234n

  let decimal_pat x = match x with 123456789n -> true | _ -> false

  let decimal_pat_neg x = match x with -123456789n -> true | _ -> false

  let decimal_pat_pos x = match x with 123456789n -> true | _ -> false

  let decimal_pat_sep x = match x with 1234567n -> true | _ -> false

  let binary_pat_neg x = match x with -170n -> true | _ -> false

  let binary_pat_pos x = match x with 170n -> true | _ -> false

  let binary_pat_upper x = match x with 170n -> true | _ -> false

  let binary_pat_sep x = match x with 85n -> true | _ -> false

  let octal_pat_neg x = match x with -2739128n -> true | _ -> false

  let octal_pat_pos x = match x with 2739128n -> true | _ -> false

  let octal_pat_upper x = match x with 2739128n -> true | _ -> false

  let octal_pat_sep x = match x with 342391n -> true | _ -> false

  let hex_pat_neg x = match x with -267242409n -> true | _ -> false

  let hex_pat_pos x = match x with 267242409n -> true | _ -> false

  let hex_pat_upper x = match x with 267242409n -> true | _ -> false

  let hex_pat_sep x = match x with 19088470n -> true | _ -> false
end

module Float = struct
  let decimal_expr = 1e-05

  let decimal_expr_neg = -1000.

  let decimal_expr_pos = 100.2

  let decimal_expr_float_neg = -1000.

  let decimal_expr_float_pos = 100.2

  let decimal_expr_E = 1e-80

  let decimal_expr_E_neg = -1e-80

  let decimal_expr_E_pos = 1e-80

  let decimal_expr_E_float_neg = -1e-80

  let decimal_expr_E_float_pos = 1e-80

  let decimal_expr_sep = 1000000.

  let neg_pos_reduce = -1234.

  let decimal_pat x = match x with 1e-05 -> true | _ -> false

  let decimal_pat_neg x = match x with -1000. -> true | _ -> false

  let decimal_pat_pos x = match x with 100.2 -> true | _ -> false

  let decimal_pat_E x = match x with 1e-80 -> true | _ -> false

  let decimal_pat_E_neg x = match x with -1e-80 -> true | _ -> false

  let decimal_pat_E_pos x = match x with 1e-80 -> true | _ -> false

  let decimal_pat_sep x = match x with 1000000. -> true | _ -> false
end

module Field_checked = struct
  let zero = Field.constant (Field.constant.to_string "0")

  let one = Field.constant (Field.constant.to_string "1")

  let plus_one = Field.constant (Field.constant.to_string "1")

  let minus_one = Field.constant (Field.constant.to_string "-1")
end

module Field_prover = struct
  include struct
    let zero = Field.constant.to_string "0"

    let one = Field.constant.to_string "1"

    let plus_one = Field.constant.to_string "1"

    let minus_one = Field.constant.to_string "-1"
  end
end

module Boolean_checked = struct
  let false_ = Boolean.false_

  let true_ = Boolean.true_
end

module Boolean_prover = struct
  include struct
    let false_ = false

    let true_ = true
  end
end
