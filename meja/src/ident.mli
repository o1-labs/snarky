(** Unique identifiers. *)
type t [@@deriving sexp, equal, compare]

type ident = t

val create : mode:Ast_types.mode -> string -> t
(** Create a new unique name. *)

val name : t -> string
(** Retrieve the name passed to [create]. *)

val mode : t -> Ast_types.mode
(** Retrieve the mode passed to [create]. *)

val pprint : Format.formatter -> t -> unit
(** Pretty print. Identifiers that do not begin with a letter or underscore
    will be surrounded by parentheses.
*)

val debug_print : Format.formatter -> t -> unit
(** Debug print. Prints the identifier and its internal ID. *)

val fresh : Ast_types.mode -> t
(** HACK: Call a variable ["x___" ^ id] where [id] is the identifier's internal
   id.
*)

module Table : sig
  (** An associative map from [ident]s, with direct name lookups. *)
  type 'a t

  val empty : 'a t
  (** An empty table. *)

  val is_empty : 'a t -> bool
  (** Returns [true] if the table is empty, false otherwise. *)

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

  val find_name :
    string -> modes:(Ast_types.mode -> bool) -> 'a t -> (ident * 'a) option
  (** [find_name name ~modes tbl] returns the most recently bound [ident] and its
      associated value if it exists, or [None] otherwise. Any [ident]
      satisfying [modes (Ident.mode ident)] will be skipped.
  *)

  val first_exn : 'a t -> ident * 'a
  (** Returns the binding to the first name (under lexical ordering). *)

  val keys : 'a t -> ident list
  (** Returns a list of the bound identifiers. *)

  val fold_keys :
    'a t -> init:'accum -> f:('accum -> ident -> 'accum) -> 'accum

  val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  val foldi :
    'a t -> init:'accum -> f:(ident -> 'accum -> 'a -> 'accum) -> 'accum

  val fold2_names :
       'v1 t
    -> 'v2 t
    -> init:'a
    -> f:(   key:string
          -> data:[`Both of 'v1 * 'v2 | `Left of 'v1 | `Right of 'v2]
          -> 'a
          -> 'a)
    -> 'a
  (** Fold over names in the two tables, in order of increasing key. *)

  val merge_skewed_names :
       'v t
    -> 'v t
    -> combine:(key:string -> ident * 'v -> ident * 'v -> ident * 'v)
    -> 'v t
  (** Merge the bindings in the two tables. When the same name is bound in
      both, [combine] is used to decide the preferred binding.
      Throws [Failure] if the result of [combine] has a different name to
      [key].

      When an [ident] is bound in both tables but does not appear in
      [find_name] for its name, the bound value from the first table is chosen.
  *)

  val map : 'a t -> f:('a -> 'b) -> 'b t
  (** Map over each of the values in the table. *)

  val mapi : 'a t -> f:(ident -> 'a -> 'b) -> 'b t
  (** Like [map], but also pass the key to the function. *)
end
