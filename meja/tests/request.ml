include struct
  type 'a Snarky.Request.t += Request of 'a list
end

include struct
  type 'a Snarky.Request.t += Request2 of 'a list

  let handle_Request2 = function
    | With {request= Request2 l; respond= ____respond____} ->
        ____respond____ (List.hd l)
    | _ -> unhandled
end
