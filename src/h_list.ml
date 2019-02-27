(** Polymorphic list type. *)
type _ t = [] : unit t | ( :: ) : 'a * 'xs t -> ('a * 'xs) t
