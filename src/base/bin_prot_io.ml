open Core_kernel

let read m filename =
  let buffer =
    In_channel.with_file filename ~binary:true ~f:In_channel.input_all
  in
  Binable.of_string m buffer

let write m filename ~data =
  Out_channel.with_file filename ~binary:true ~f:(fun channel ->
      let buffer =
        Binable.to_string m data |> Bytes.unsafe_of_string_promise_no_mutation
      in
      Out_channel.output channel ~buf:buffer ~pos:0 ~len:(Bytes.length buffer)
  )
