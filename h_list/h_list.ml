(** Polymorphic list type. *)
type (_, _) t = [] : ('r, 'r) t | ( :: ) : 'a * ('r, 'k) t -> ('r, 'a -> 'k) t
