module Location = struct
  include Location

  let error_of_printer ?(loc = none) fmt = error_of_printer loc fmt
end
