(** Unique identifiers. *)
type t [@@deriving sexp, equal, compare]

type ident = t [@@deriving sexp]

(** Create a new unique name.

    If the [ocaml] argument is true, the identifier is associated with a
    modifiable 'OCaml name', accessible via [ocaml_name_ref].
*)
val create : mode:Ast_types.mode -> ?ocaml:bool -> string -> t

(** Create the name of a global (ie. external) module. *)
val create_global : string -> t

(** Create an identifier for a row constructor.

    This kind of identifier will alias with any other row identifier created
    with the same name.
*)
val create_row : string -> t

val is_global : t -> bool

val is_row : t -> bool

(** Retrieve the name passed to [create]. *)
val name : t -> string

(** Return the OCaml name for this identifier, which may or may not differ from
    the name given by [name].
*)
val ocaml_name : t -> string

(** Returns a handle to modify the OCaml name, or none if there is no
    associated OCaml name.
*)
val ocaml_name_ref : t -> string ref option

(** Retrieve the mode passed to [create]. *)
val mode : t -> Ast_types.mode

(** Pretty print. Identifiers that do not begin with a letter or underscore
    will be surrounded by parentheses.
*)
val pprint : Format.formatter -> t -> unit

(** Debug print. Prints the identifier and its internal ID. *)
val debug_print : Format.formatter -> t -> unit

(** HACK: Call a variable ["x___" ^ id] where [id] is the identifier's internal
   id.
*)
val fresh : Ast_types.mode -> t

module Table : sig
  (** An associative map from [ident]s, with direct name lookups. *)
  type 'a t

  (** An empty table. *)
  val empty : 'a t

  (** Returns [true] if the table is empty, false otherwise. *)
  val is_empty : 'a t -> bool

  (** [add ~key:ident ~data tbl] returns the table [tbl] extended with [ident]
      associated to [data].
      Any previous association with [ident] is replaced, and the name
      [name ident] is also associated with [data].
  *)
  val add : key:ident -> data:'a -> 'a t -> 'a t

  (** Returns the table with the binding to the given ident removed. *)
  val remove : ident -> 'a t -> 'a t

  (** Returns the value bound to the given ident in the table if it exists, or
      [None] otherwise.
  *)
  val find : ident -> 'a t -> 'a option

  (** [find_name name ~modes tbl] returns the most recently bound [ident] and its
      associated value if it exists, or [None] otherwise. Any [ident]
      satisfying [modes (Ident.mode ident)] will be skipped.
  *)
  val find_name :
    string -> modes:(Ast_types.mode -> bool) -> 'a t -> (ident * 'a) option

  (** Returns the binding to the first name (under lexical ordering). *)
  val first_exn : 'a t -> ident * 'a

  (** Returns a list of the bound identifiers. *)
  val keys : 'a t -> ident list

  val fold_keys : 'a t -> init:'accum -> f:('accum -> ident -> 'accum) -> 'accum

  val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  val foldi :
    'a t -> init:'accum -> f:(ident -> 'accum -> 'a -> 'accum) -> 'accum

  (** Fold over names in the two tables, in order of increasing key. *)
  val fold2_names :
       'v1 t
    -> 'v2 t
    -> init:'a
    -> f:
         (   key:string
          -> data:[ `Both of 'v1 * 'v2 | `Left of 'v1 | `Right of 'v2 ]
          -> 'a
          -> 'a )
    -> 'a

  (** Merge the bindings in the two tables. When the same name is bound in
      both, [combine] is used to decide the preferred binding.
      Throws [Failure] if the result of [combine] has a different name to
      [key].

      When an [ident] is bound in both tables but does not appear in
      [find_name] for its name, the bound value from the first table is chosen.
  *)
  val merge_skewed_names :
       'v t
    -> 'v t
    -> combine:(key:string -> ident * 'v -> ident * 'v -> ident * 'v)
    -> 'v t

  (** Map over each of the values in the table. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** Like [map], but also pass the key to the function. *)
  val mapi : 'a t -> f:(ident -> 'a -> 'b) -> 'b t
end

module Set : Core_kernel.Set.S with type Elt.t = t

module Map : Core_kernel.Map.S with type Key.t = t
