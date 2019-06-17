open Core

let read (type t) (module R : Bin_prot.Binable.Minimal.S with type t = t)
    filename =
  let buffer =
    In_channel.with_file filename ~f:(fun channel ->
        let length =
          match Int64.to_int (In_channel.length channel) with
          | Some length ->
              length
          | None ->
              Format.(fprintf err_formatter "File %s is too big to read.")
                filename ;
              exit 2
        in
        let buffer = Bigstring.create length in
        Bigstring.really_input channel buffer ;
        buffer )
  in
  R.bin_read_t buffer ~pos_ref:(ref 0)

let write (type t) (module R : Bin_prot.Binable.Minimal.S with type t = t)
    filename ~data =
  Out_channel.with_file filename ~f:(fun channel ->
      let buffer = Bigstring.create (R.bin_size_t data) in
      ignore (R.bin_write_t buffer ~pos:0 data) ;
      Bigstring.really_output channel buffer )
