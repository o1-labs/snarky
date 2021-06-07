let encode (bytearray : bytes) : string =
  let dummy_char = '_' in
  let start_of_digit_0_in_ascii_table = 0x30 in
  let start_of_lower_case_a_in_ascii_table = 0x61 in
  let hex_digit_of_int (x : int) : char =
    assert (x >= 0);
    assert (x < 16);
    char_of_int
      (if x < 10 then x + start_of_digit_0_in_ascii_table
      else x - 10 + start_of_lower_case_a_in_ascii_table)
  in
  let rec aux bytearray len cur_pos buf =
    if cur_pos < len then (
      let x = int_of_char @@ Bytes.get bytearray cur_pos in
      let c1 = hex_digit_of_int (x lsr 4) in
      let c2 = hex_digit_of_int (x land 0x0F) in
      Bytes.set buf (cur_pos * 2) c1;
      Bytes.set buf ((cur_pos * 2) + 1) c2;
      aux bytearray len (succ cur_pos) buf)
  in
  let len = Bytes.length bytearray in
  let buf_len = 2 * len in
  let buf = Bytes.make buf_len dummy_char in
  aux bytearray len 0 buf;
  Bytes.to_string buf

let decode_1char = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | _ -> failwith "invalid character"

let decode_2chars c1 c2 : char =
  let fst = decode_1char c1 in
  let snd = decode_1char c2 in
  let res = (fst lsl 4) lxor snd in
  char_of_int res

let decode hexstring =
  let rec aux res_cur_pos res_len ss res : bytes =
    if res_cur_pos < res_len then (
      let pos = 2 * res_cur_pos in
      let c1 = ss.[pos] in
      let c2 = ss.[pos + 1] in
      let b = decode_2chars c1 c2 in
      Bytes.set res res_cur_pos b;
      aux (succ res_cur_pos) res_len ss res)
    else res
  in
  let len = String.length hexstring in
  if len mod 2 <> 0 then failwith "wrong hexstring length"
  else if len = 0 then Bytes.empty
  else
    let buf_len = len / 2 in
    let buf = Bytes.make buf_len '\x00' in
    aux 0 buf_len hexstring buf
      