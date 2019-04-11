include struct
  type 'x Snarky.Request.t += Request : 'x list -> 'x Snarky__Request.t
end

include struct
  type 'x Snarky.Request.t += Request2 : 'x Snarky__Request.t

  let handle_Request2 = function
    | With {request= Request2; respond} ->
        let unhandled = Snarky.Request.unhandled in
        let _ = () in
        unhandled
    | _ -> Snarky.Request.unhandled
end

include struct
  type 'x Snarky.Request.t += Request3 : 'x option -> 'x Snarky__Request.t

  let handle_Request3 = function
    | With {request= Request3 x; respond} -> (
        let unhandled = Snarky.Request.unhandled in
        match x with None -> unhandled | Some x -> respond x )
    | _ -> Snarky.Request.unhandled
end
