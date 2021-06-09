type ('hash, 'a) t [@@deriving sexp]

type ('hash, 'a) merkle_tree = ('hash, 'a) t

module Address : sig
  type t = int
end

module Free_hash : sig
  type 'a t = Hash_value of 'a | Hash_empty | Merge of 'a t * 'a t
  [@@deriving sexp]

  val diff : 'a t -> 'a t -> bool list option

  val run :
       'a t
    -> hash:('a option -> 'hash)
    -> merge:('hash -> 'hash -> 'hash)
    -> 'hash
end

val depth : (_, _) t -> int

val create :
     hash:('a option -> 'hash)
  -> merge:('hash -> 'hash -> 'hash)
  -> 'a
  -> ('hash, 'a) t

val add : ('hash, 'a) t -> 'a -> ('hash, 'a) t

val add_many : ('hash, 'a) t -> 'a list -> ('hash, 'a) t

val update : ('hash, 'a) t -> Address.t -> 'a -> ('hash, 'a) t

val get : (_, 'a) t -> Address.t -> 'a option

val get_exn : (_, 'a) t -> Address.t -> 'a

val get_path : ('hash, 'a) t -> Address.t -> 'hash list

val implied_root :
  merge:('hash -> 'hash -> 'hash) -> Address.t -> 'hash -> 'hash list -> 'hash

val get_free_path : (_, 'a) t -> Address.t -> 'a Free_hash.t list

val free_root : (_, 'a) t -> 'a Free_hash.t

val implied_free_root : Address.t -> 'a -> 'a Free_hash.t list -> 'a Free_hash.t

val root : ('hash, 'a) t -> 'hash

val to_list : ('hash, 'a) t -> 'a list

val check_exn : (_, _) t -> unit

module Checked
    (Impl : Snark_intf.S) (Hash : sig
      type var

      type value

      val typ : (var, value) Impl.Typ.t

      val merge : height:int -> var -> var -> (var, _) Impl.Checked.t

      val if_ :
        Impl.Boolean.var -> then_:var -> else_:var -> (var, _) Impl.Checked.t

      val assert_equal : var -> var -> (unit, _) Impl.Checked.t
    end) (Elt : sig
      type var

      type value

      val typ : (var, value) Impl.Typ.t

      val hash : var -> (Hash.var, _) Impl.Checked.t
    end) : sig
  open Impl

  module Address : sig
    type var = Boolean.var list

    type value = int

    val typ : depth:int -> (var, value) Typ.t
  end

  module Path : sig
    type value = Hash.value list

    type var = Hash.var list

    val typ : depth:int -> (var, value) Typ.t
  end

  type _ Request.t +=
    | Get_element : Address.value -> (Elt.value * Path.value) Request.t
    | Get_path : Address.value -> Path.value Request.t
    | Set : Address.value * Elt.value -> unit Request.t

  val implied_root :
    Hash.var -> Address.var -> Path.var -> (Hash.var, _) Checked.t

  (* TODO: Change [prev] to be [prev_hash : Hash.var] since there may be no need
     to certify that the hash of the element is a particular value. *)
  val modify_req :
       depth:int
    -> Hash.var
    -> Address.var
    -> f:(Elt.var -> (Elt.var, 's) Checked.t)
    -> (Hash.var, 's) Checked.t

  (* This function does the modification and also returns the old and the new value *)
  val fetch_and_update_req :
       depth:int
    -> Hash.var
    -> Address.var
    -> f:(Elt.var -> (Elt.var, 's) Checked.t)
    -> (Hash.var * [ `Old of Elt.var ] * [ `New of Elt.var ], 's) Checked.t

  val get_req : depth:int -> Hash.var -> Address.var -> (Elt.var, 's) Checked.t

  (* TODO: Change [prev] to be [prev_hash : Hash.var] since there may be no need
     to certify that the hash of the element is a particular value. *)

  val update_req :
       depth:int
    -> root:Hash.var
    -> prev:Elt.var
    -> next:Elt.var
    -> Address.var
    -> (Hash.var, _) Checked.t

  val update :
       depth:int
    -> root:Hash.var
    -> prev:Elt.var
    -> next:Elt.var
    -> Address.var
    -> (Hash.var, (Hash.value, Elt.value) merkle_tree) Checked.t
end

module Run : sig
  module Make
      (Impl : Snark_intf.Run_basic) (Hash : sig
        type var

        type value

        val typ : (var, value) Impl.Typ.t

        val merge : height:int -> var -> var -> var

        val if_ : Impl.Boolean.var -> then_:var -> else_:var -> var

        val assert_equal : var -> var -> unit

        (** The prover state to run the checked computations above with.
              This state will *always* be passed to the above unchanged.

              NOTE: This is equivalent to the condition on the monadic
                    interface that the computations are not constrained in
                    their prover-state type: the type is abstract from the
                    perspective of the functions, and so they cannot have any
                    effect on the state.
          *)
        val prover_state : Impl.prover_state
      end) (Elt : sig
        type var

        type value

        val typ : (var, value) Impl.Typ.t

        val hash : var -> Hash.var

        (** The prover state to run the checked computations above with.
              This state will *always* be passed to the above unchanged.

              NOTE: This is equivalent to the condition on the monadic
                    interface that the computations are not constrained in
                    their prover-state type: the type is abstract from the
                    perspective of the functions, and so they cannot have any
                    effect on the state.
          *)
        val prover_state : Impl.prover_state

        (** A lens to give access to the [(Hash.value, Elt.value) merkle_tree]
            state that [update] uses. *)
        val lens : (Impl.prover_state, (Hash.value, value) merkle_tree) Lens.t
      end) : sig
    open Impl

    module Address : sig
      type var = Boolean.var list

      type value = int

      val typ : depth:int -> (var, value) Typ.t
    end

    module Path : sig
      type value = Hash.value list

      type var = Hash.var list

      val typ : depth:int -> (var, value) Typ.t
    end

    type _ Request.t +=
      | Get_element : Address.value -> (Elt.value * Path.value) Request.t
      | Get_path : Address.value -> Path.value Request.t
      | Set : Address.value * Elt.value -> unit Request.t

    val implied_root : Hash.var -> Address.var -> Path.var -> Hash.var

    (* TODO: Change [prev] to be [prev_hash : Hash.var] since there may be no need
       to certify that the hash of the element is a particular value. *)

    val modify_req :
      depth:int -> Hash.var -> Address.var -> f:(Elt.var -> Elt.var) -> Hash.var

    val get_req : depth:int -> Hash.var -> Address.var -> Elt.var

    (* TODO: Change [prev] to be [prev_hash : Hash.var] since there may be no need
       to certify that the hash of the element is a particular value. *)

    val update_req :
         depth:int
      -> root:Hash.var
      -> prev:Elt.var
      -> next:Elt.var
      -> Address.var
      -> Hash.var

    val update :
         depth:int
      -> root:Hash.var
      -> prev:Elt.var
      -> next:Elt.var
      -> Address.var
      -> Hash.var
  end
end
