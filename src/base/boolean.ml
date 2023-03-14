type 'v t = 'v

let to_field_var (t : 'v t) : 'v = t

module Unsafe = struct
  let create x = x
end
