module type S = sig
  type whole

  type view

  val getter : whole -> view

  val setter : whole -> view -> whole
end

type ('whole, 'view) t =
  (module S with type whole = 'whole and type view = 'view)

let get (type whole view) ((module M) : (whole, view) t) = M.getter

let set (type whole view) ((module M) : (whole, view) t) = M.setter

let map (type whole view) ((module M) : (whole, view) t) ~f x =
  let y = M.getter x in
  M.setter x (f y)

let constant (type whole view) (x : view) : (whole, view) t =
  ( module struct
    type nonrec whole = whole

    type nonrec view = view

    let getter _ = x

    let setter w _ = w
  end )
