open Format

let comma_sep fmt () = fprintf fmt ",@ "

let bar_sep fmt () = fprintf fmt "@ | "

let arg_label fmt = function
  | Asttypes.Nolabel ->
      ()
  | Labelled label ->
      fprintf fmt "@[<hv2>%s:@," label
  | Optional label ->
      fprintf fmt "@[<hv2>?%s:@," label

let arg_label_box_end fmt = function
  | Asttypes.Nolabel ->
      ()
  | _ ->
      fprintf fmt "@]"
