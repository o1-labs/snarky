(** Unique identifiers. *)
type t [@@deriving sexp]

type ident = t

val create : string -> t
(** Create a new unique name. *)

val name : t -> string
(** Retrieve the name passed to [create]. *)

val compare : t -> t -> int
(** Compare two names. This is 0 iff they originate from the same call to
    [create].
*)

module Table : sig
  (** An associative map from [ident]s, with direct name lookups. *)
  type 'a t

  val add : key:ident -> data:'a -> 'a t -> 'a t
  (** [add ~key:ident ~data tbl] returns the table [tbl] extended with [ident]
      associated to [data].
      Any previous association with [ident] is replaced, and the name
      [name ident] is also associated with [data].
  *)

  val remove : ident -> 'a t -> 'a t
  (** Returns the table with the binding to the given ident removed. *)

  val find : ident -> 'a t -> 'a option
  (** Returns the value bound to the given ident in the table if it exists, or
      [None] otherwise.
  *)

  val find_name : string -> 'a t -> (ident * 'a) option
  (** [find_name name tbl] returns the most recently bound [ident] and its
      associated value if it exists, or [None] otherwise.
  *)
end
