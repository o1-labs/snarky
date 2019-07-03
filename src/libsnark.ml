module Bignum_bigint = Bigint
open Core
open Backend_types
open Ctypes
open Foreign

module type Foreign_intf = sig
  type t

  val typ : t Ctypes.typ
end

module type Deletable_intf = sig
  include Foreign_intf

  val delete : t -> unit
end

let set_no_profiling =
  foreign "camlsnark_set_profiling" (bool @-> returning void)

let set_printing_off =
  foreign "camlsnark_set_printing_off" (void @-> returning void)

let set_printing_stdout =
  foreign "camlsnark_set_printing_stdout" (void @-> returning void)

let set_printing_file =
  foreign "camlsnark_set_printing_file" (string @-> returning void)

module Print_func = struct
  (** Internal: The reference to the user-defined function passed to
      {!val:set_printing_fun}.
      OCaml may relocate the function in memory if it is heap-allocated (e.g.
      using a closure) during its GC cycle, so we store a reference here and
      call it from the statically-allocated OCaml function {!val:dispatch}
      below.
  *)
  let print = ref (fun str -> str)

  (** A reference to the C [puts] function.

      The OCaml stdlib functions use thread-unsafe primitives that may cause a
      crash if calls from multiple threads overlap, so we use this to avoid
      their thread-unsafe blocking behaviour.
  *)
  let puts = foreign "camlsnark_puts" (string @-> returning void)

  (** The dispatcher passed to the C++ interface in {!val:set_printing_fun}.
      We cannot pass the user-provided function directly to the C++ side in
      case of GC relocation, so this provides a statically-allocated wrapper.

      The call to {!val:puts} is made from here instead of the C++ side so that
      OCaml's GC behaviour is mitigated by the the Ctypes API.
  *)
  let dispatch str = puts (!print str)
end

let set_printing_fun =
  let stub =
    foreign "camlsnark_set_printing_fun"
      (funptr (string @-> returning void) @-> returning void)
  in
  fun f ->
    Print_func.print := f ;
    stub Print_func.dispatch

let () = set_no_profiling true

module Group_coefficients (Fq : Foreign_intf) = struct
  module type Bound = sig
    include Foreign_types

    val a : (unit -> Fq.t return) result

    val b : (unit -> Fq.t return) result
  end

  module type S = sig
    val a : Fq.t

    val b : Fq.t
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    let mk_coeff name = foreign name (void @-> returning Fq.typ)

    let a = mk_coeff (with_prefix P.prefix "coeff_a")

    let b = mk_coeff (with_prefix P.prefix "coeff_b")
  end

  module Make
      (Bindings : Bound with type 'a return = 'a and type 'a result = 'a) : S =
  struct
    open Bindings

    let a = a ()

    let b = b ()
  end
end

module Window_table
    (G : Foreign_intf)
    (Scalar_field : Foreign_intf)
    (Scalar : Foreign_intf)
    (V : Foreign_intf) =
struct
  module type Bound = sig
    include Foreign_types

    val create : (G.t -> V.t return) result

    val scale : (V.t -> Scalar.t -> G.t return) result
  end

  module type S = sig
    type t [@@deriving bin_io]

    val create : G.t -> t

    val scale : t -> Scalar.t -> G.t

    val scale_field : t -> Scalar_field.t -> G.t
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    let func_name = with_prefix P.prefix

    let create =
      foreign (func_name "create_window_table") (G.typ @-> returning V.typ)

    let scale =
      foreign
        (func_name "window_scalar_mul")
        (V.typ @-> Scalar.typ @-> returning G.typ)
  end

  module Make
      (Bindings : Bound with type 'a return = 'a and type 'a result = 'a)
                                                                        (G : sig
          val delete : G.t -> unit
      end) (Scalar : sig
        val of_field : Scalar_field.t -> Scalar.t
      end) (V : sig
        val delete : V.t -> unit

        include Binable.S with type t = V.t
      end) : S = struct
    open Bindings
    include V

    let create g =
      let t = create g in
      Caml.Gc.finalise delete t ; t

    let scale tbl s =
      let x = scale tbl s in
      Caml.Gc.finalise G.delete x ;
      x

    let scale_field t (x : Scalar_field.t) = scale t (Scalar.of_field x)
  end
end

module Group (Field : sig
  type t

  val typ : t Ctypes.typ
end) (Bigint_r : sig
  type t

  val typ : t Ctypes.typ
end) (Fq : sig
  type t [@@deriving bin_io]

  val typ : t Ctypes.typ

  val delete : t -> unit
end) =
struct
  module type Bound = sig
    include Foreign_types

    type t

    val typ : t Ctypes.typ

    val zero : (unit -> t return) result

    val one : (unit -> t return) result

    val delete : (t -> unit return) result

    val print : (t -> unit return) result

    val random : (unit -> t return) result

    val double : (t -> t return) result

    val negate : (t -> t return) result

    val add : (t -> t -> t return) result

    val scale : (Bigint_r.t -> t -> t return) result

    val scale_field : (Field.t -> t -> t return) result

    val equal : (t -> t -> bool return) result

    val of_affine : (Fq.t -> Fq.t -> t return) result

    val is_zero : (t -> bool return) result

    val to_affine' : (t -> unit return) result

    val x : (t -> Fq.t return) result

    val y : (t -> Fq.t return) result

    val prefix : string

    module Vector :
      Vector.Bound
      with type 'a result = 'a result
       and type 'a return = 'a return
       and type elt = t
  end

  module type S = sig
    type t [@@deriving bin_io]

    val typ : t Ctypes.typ

    val add : t -> t -> t

    val ( + ) : t -> t -> t

    val negate : t -> t

    val double : t -> t

    val scale : t -> Bigint_r.t -> t

    val scale_field : t -> Field.t -> t

    val zero : t

    val one : t

    module Affine : sig
      type t = Fq.t * Fq.t [@@deriving bin_io]
    end

    val to_affine_exn : t -> Affine.t

    val to_affine : t -> Affine.t option

    val of_affine : Affine.t -> t

    val equal : t -> t -> bool

    val random : unit -> t

    val delete : t -> unit

    val print : t -> unit

    module Vector : Vector.S_binable with type elt := t
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F
    include P
    include Make_foreign (F) (P)

    let zero = foreign (func_name "zero") (void @-> returning typ)

    let one = foreign (func_name "one") (void @-> returning typ)

    let delete = foreign (func_name "delete") (typ @-> returning void)

    let print = foreign (func_name "print") (typ @-> returning void)

    let random = foreign (func_name "random") (void @-> returning typ)

    let double = foreign (func_name "double") (typ @-> returning typ)

    let negate = foreign (func_name "negate") (typ @-> returning typ)

    let add = foreign (func_name "add") (typ @-> typ @-> returning typ)

    let scale =
      foreign (func_name "scale") (Bigint_r.typ @-> typ @-> returning typ)

    let scale_field =
      foreign (func_name "scale_field") (Field.typ @-> typ @-> returning typ)

    let equal = foreign (func_name "equal") (typ @-> typ @-> returning bool)

    let of_affine =
      foreign (func_name "of_coords") (Fq.typ @-> Fq.typ @-> returning typ)

    let is_zero = foreign (func_name "is_zero") (typ @-> returning bool)

    let to_affine' =
      foreign (func_name "to_affine_coordinates") (typ @-> returning void)

    let x = foreign (func_name "x") (typ @-> returning Fq.typ)

    let y = foreign (func_name "y") (typ @-> returning Fq.typ)

    module Vector =
      Vector.Bind
        (F)
        (struct
          type nonrec t = t

          let typ = typ

          let prefix = with_prefix prefix "vector"
        end)
  end

  module Make
      (Bindings : Bound with type 'a return = 'a and type 'a result = 'a) : S =
  struct
    include (
      Bindings :
        Bound
        with type 'a return = 'a
         and type 'a result = 'a
         and type t = Bindings.t
        with module Vector := Bindings.Vector )

    let zero = zero ()

    let one = one ()

    let schedule_delete t = Caml.Gc.finalise delete t

    let random () =
      let x = random () in
      schedule_delete x ; x

    let double x =
      let z = double x in
      schedule_delete z ; z

    let negate x =
      let z = negate x in
      schedule_delete z ; z

    let add x y =
      let z = add x y in
      schedule_delete z ; z

    let ( + ) = add

    let scale y x =
      let z = scale x y in
      schedule_delete z ; z

    let scale_field y x =
      let z = scale_field x y in
      schedule_delete z ; z

    module Affine = struct
      type t = Fq.t * Fq.t [@@deriving bin_io]
    end

    let of_affine (x, y) =
      let t = of_affine x y in
      schedule_delete t ; t

    (* This function hits an ugly C++ assertion failure in the zero case, so
   we wrap it with to_affine_exn *)
    let to_affine' t =
      let () = to_affine' t in
      let x = x t in
      Caml.Gc.finalise Fq.delete x ;
      let y = y t in
      Caml.Gc.finalise Fq.delete y ;
      (x, y)

    let to_affine t = if is_zero t then None else Some (to_affine' t)

    let to_affine_exn t =
      if is_zero t then
        failwithf "to_affine_exn (%s): Got a zero curve point" Bindings.prefix
          ()
      else to_affine' t

    module Repr = struct
      type t = Zero | Non_zero of Affine.t [@@deriving bin_io]
    end

    let to_repr t : Repr.t =
      match to_affine t with None -> Zero | Some t -> Non_zero t

    let of_repr (r : Repr.t) =
      match r with Zero -> zero | Non_zero t -> of_affine t

    module B =
      Binable.Of_binable
        (Repr)
        (struct
          type nonrec t = t

          let to_binable = to_repr

          let of_binable = of_repr
        end)

    include B

    module Vector =
      Vector.Make_binable (struct
          type nonrec t = t

          include B

          let schedule_delete = schedule_delete
        end)
        (Bindings.Vector)
  end
end

module Field = struct
  module type Bound = sig
    include Foreign_types

    type t

    val typ : t typ

    val size_in_bits : (unit -> int return) result

    val delete : (t -> unit return) result

    val print : (t -> unit return) result

    val random : (unit -> t return) result

    val square : (t -> t return) result

    val is_square : (t -> bool return) result

    val sqrt : (t -> t return) result

    val of_int : (Long_vector.elt -> t return) result

    val add : (t -> t -> t return) result

    val inv : (t -> t return) result

    val mul : (t -> t -> t return) result

    val sub : (t -> t -> t return) result

    module Mutable : sig
      val add : (t -> t -> unit return) result

      val sub : (t -> t -> unit return) result

      val mul : (t -> t -> unit return) result

      val copy : (t -> t -> unit return) result
    end

    val equal : (t -> t -> bool return) result

    module Vector :
      Vector.Bound
      with type 'a result = 'a result
       and type 'a return = 'a return
       and type elt = t
  end

  module type S = sig
    type t [@@deriving sexp, bin_io]

    val typ : t Ctypes.typ

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val inv : t -> t

    val is_square : t -> bool

    val sqrt : t -> t

    val square : t -> t

    val of_int : int -> t

    val one : t

    val zero : t

    val equal : t -> t -> bool

    val size_in_bits : int

    val random : unit -> t

    val delete : t -> unit

    val schedule_delete : t -> unit

    val print : t -> unit

    module Mutable : sig
      val add : t -> other:t -> unit

      val mul : t -> other:t -> unit

      val sub : t -> other:t -> unit

      val copy : over:t -> t -> unit
    end

    val ( += ) : t -> t -> unit

    val ( -= ) : t -> t -> unit

    val ( *= ) : t -> t -> unit

    module Vector : Vector.S_binable with type elt = t
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          include Foreign_intf

          val func_name : string -> string

          val outer_prefix : string
      end) :
    Bound
    with type 'a return = 'a F.return
     and type 'a result = 'a F.result
     and type t = P.t = struct
    include F
    include P

    let size_in_bits =
      foreign (func_name "size_in_bits") (void @-> returning int)

    let delete = foreign (func_name "delete") (typ @-> returning void)

    let print = foreign (func_name "print") (typ @-> returning void)

    let random = foreign (func_name "random") (void @-> returning typ)

    let square = foreign (func_name "square") (typ @-> returning typ)

    let is_square = foreign (func_name "is_square") (typ @-> returning bool)

    let sqrt = foreign (func_name "sqrt") (typ @-> returning typ)

    let of_int = foreign (func_name "of_int") (long @-> returning typ)

    let add = foreign (func_name "add") (typ @-> typ @-> returning typ)

    let inv = foreign (func_name "inv") (typ @-> returning typ)

    let mul = foreign (func_name "mul") (typ @-> typ @-> returning typ)

    let sub = foreign (func_name "sub") (typ @-> typ @-> returning typ)

    module Mutable = struct
      let make name =
        foreign (func_name ("mut_" ^ name)) (typ @-> typ @-> returning void)

      let add = make "add"

      let sub = make "sub"

      let mul = make "mul"

      let copy = foreign (func_name "copy") (typ @-> typ @-> returning void)
    end

    let equal = foreign (func_name "equal") (typ @-> typ @-> returning bool)

    module Vector =
      Vector.Bind
        (F)
        (struct
          type nonrec t = t

          let typ = typ

          let prefix = with_prefix outer_prefix "field_vector"
        end)
  end

  module Make (Field0 : sig
    type t [@@deriving sexp]

    include Deletable_intf with type t := t
  end) (R : sig
    include Binable.Minimal.S

    val to_field : t -> Field0.t

    val of_field : Field0.t -> t
  end)
  (Bindings : Bound
              with type 'a return = 'a
               and type 'a result = 'a
               and type t = Field0.t) : S with type t = Bindings.t = struct
    module T = struct
      include Field0

      include (
        Bindings :
          Bound
          with type 'a return = 'a
           and type 'a result = 'a
           and type t := Field0.t
          with module Vector := Bindings.Vector
          with module Mutable := Bindings.Mutable )

      let size_in_bits = size_in_bits ()

      let schedule_delete t = Caml.Gc.finalise delete t

      let random () =
        let x = random () in
        schedule_delete x ; x

      let square x =
        let y = square x in
        schedule_delete y ; y

      let sqrt x =
        let y = sqrt x in
        schedule_delete y ; y

      let of_int n =
        let x = of_int (Signed.Long.of_int n) in
        schedule_delete x ; x

      let add x y =
        let z = add x y in
        schedule_delete z ; z

      let inv x =
        let y = inv x in
        schedule_delete y ; y

      let mul x y =
        let z = mul x y in
        schedule_delete z ; z

      let sub x y =
        let z = sub x y in
        schedule_delete z ; z

      module Mutable = struct
        open Bindings.Mutable

        let make stub x ~other = stub x other

        let add = make add

        let sub = make sub

        let mul = make mul

        let copy ~over x = copy over x
      end

      let ( += ) t other = Mutable.add t ~other

      let ( -= ) t other = Mutable.sub t ~other

      let ( *= ) t other = Mutable.mul t ~other

      let one = of_int 1

      let zero = of_int 0
    end

    module B =
      Binable.Of_binable
        (R)
        (struct
          type t = T.t

          let to_binable = R.of_field

          let of_binable = R.to_field
        end)

    include B

    module Vector =
      Vector.Make_binable (struct
          type t = T.t

          include B

          let schedule_delete = Caml.Gc.finalise T.delete
        end)
        (Bindings.Vector)

    include T
  end
end

(* NOTE: This isn't functored over Field0 because Common is field-agnostic.
   Probably the relationship should be inverted, and Field should get
   to/of_bigint instead.
*)
module Bigint = struct
  module Common = struct
    module type Bound = sig
      include Foreign_types

      type t

      val typ : t typ

      val func_name : string -> string

      val delete : (t -> unit return) result

      val test_bit : (t -> int -> bool return) result

      val find_wnaf : (Unsigned.size_t -> t -> Long_vector.t return) result
    end

    module type S = sig
      type t

      val typ : t Ctypes.typ

      val test_bit : t -> int -> bool

      val find_wnaf : Unsigned.Size_t.t -> t -> Long_vector.t
    end

    module Bind
        (F : Ctypes.FOREIGN) (P : sig
            val prefix : string

            val outer_prefix : string
        end) :
      Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
    struct
      include F

      include Make_foreign
                (F)
                (struct
                  let prefix =
                    with_prefix (with_prefix P.outer_prefix "bigint") P.prefix
                end)

      let test_bit =
        foreign (func_name "test_bit") (typ @-> int @-> returning bool)

      let find_wnaf =
        foreign (func_name "find_wnaf")
          (size_t @-> typ @-> returning Long_vector.typ)
    end

    module Make
        (Bindings : Bound with type 'a return = 'a and type 'a result = 'a) :
      S with type t = Bindings.t = struct
      include Bindings

      let find_wnaf x y =
        let v = find_wnaf x y in
        Caml.Gc.finalise Long_vector.delete v ;
        v
    end
  end

  module R = struct
    module type Bound = sig
      include Common.Bound

      type field

      val div : (t -> t -> t return) result

      val of_numeral : (string -> int -> int -> t return) result

      val of_decimal_string : (string -> t return) result

      val compare : (t -> t -> int return) result

      val of_field : (field -> t return) result

      val num_limbs : (unit -> int return) result

      val bytes_per_limb : (unit -> int return) result

      val to_data : (t -> char Ctypes_static.ptr return) result

      val of_data : (char Ctypes_static.ptr -> t return) result

      val to_field : (t -> field return) result
    end

    module type S = sig
      type t [@@deriving bin_io]

      type field

      val typ : t Ctypes.typ

      val of_decimal_string : string -> t

      val of_numeral : string -> base:int -> t

      val of_field : field -> t

      val of_data : Bigstring.t -> bitcount:int -> t

      val length_in_bytes : int

      val div : t -> t -> t

      val to_field : t -> field

      val to_bigstring : t -> Bigstring.t

      val compare : t -> t -> int

      val test_bit : t -> int -> bool

      val find_wnaf : Unsigned.Size_t.t -> t -> Long_vector.t
    end

    module Bind
        (F : Ctypes.FOREIGN) (P : sig
            val prefix : string
        end)
        (Field0 : Foreign_intf) :
      Bound
      with type 'a return = 'a F.return
       and type 'a result = 'a F.result
       and type field = Field0.t = struct
      open F

      include Common.Bind
                (F)
                (struct
                  let outer_prefix = P.prefix

                  let prefix = "r"
                end)

      type field = Field0.t

      let div = foreign (func_name "div") (typ @-> typ @-> returning typ)

      let of_numeral =
        foreign (func_name "of_numeral")
          (string @-> int @-> int @-> returning typ)

      let of_decimal_string =
        foreign (func_name "of_decimal_string") (string @-> returning typ)

      let compare =
        foreign (func_name "compare") (typ @-> typ @-> returning int)

      let of_field =
        foreign (func_name "of_field") (Field0.typ @-> returning typ)

      let num_limbs = foreign (func_name "num_limbs") (void @-> returning int)

      let bytes_per_limb =
        foreign (func_name "bytes_per_limb") (void @-> returning int)

      let to_data = foreign (func_name "to_data") (typ @-> returning (ptr char))

      let of_data = foreign (func_name "of_data") (ptr char @-> returning typ)

      let to_field =
        foreign (func_name "to_field") (typ @-> returning Field0.typ)
    end

    module Make
        (Field0 : Deletable_intf)
        (Bindings : Bound
                    with type 'a return = 'a
                     and type 'a result = 'a
                     and type field = Field0.t) :
      S with type field = Field0.t = struct
      include Bindings

      include (Common.Make (Bindings) : Common.S with type t := t)

      let div x y =
        let z = div x y in
        Caml.Gc.finalise delete z ; z

      let of_numeral s ~base =
        let n = of_numeral s (String.length s) base in
        Caml.Gc.finalise delete n ; n

      let of_decimal_string s =
        let n = of_decimal_string s in
        Caml.Gc.finalise delete n ; n

      let of_field x =
        let n = of_field x in
        Caml.Gc.finalise delete n ; n

      let num_limbs = num_limbs ()

      let bytes_per_limb =
        let res = bytes_per_limb () in
        assert (res = 8) ;
        res

      let length_in_bytes = num_limbs * bytes_per_limb

      let to_bigstring t =
        let limbs = to_data t in
        Bigstring.init length_in_bytes ~f:(fun i -> Ctypes.(!@(limbs +@ i)))

      let of_bigstring s =
        let ptr = Ctypes.bigarray_start Ctypes.array1 s in
        let t = of_data ptr in
        Caml.Gc.finalise delete t ; t

      let of_data bs ~bitcount =
        assert (bitcount <= length_in_bytes * 8) ;
        of_bigstring bs

      include Binable.Of_binable
                (Bigstring.Stable.V1)
                (struct
                  type nonrec t = t

                  let to_binable = to_bigstring

                  let of_binable = of_bigstring
                end)

      let to_field n =
        let x = to_field n in
        Caml.Gc.finalise Field0.delete x ;
        x
    end
  end

  module type Bound = sig
    include Foreign_types

    type field

    module R :
      R.Bound
      with type 'a return = 'a return
       and type 'a result = 'a result
       and type field = field

    module Q :
      Common.Bound
      with type 'a return = 'a return
       and type 'a result = 'a result
  end

  module type S = sig
    type field

    module R : R.S with type field = field

    module Q : Common.S
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end)
      (Field0 : Foreign_intf) :
    Bound
    with type 'a return = 'a F.return
     and type 'a result = 'a F.result
     and type field = Field0.t = struct
    include F

    type field = Field0.t

    module R = R.Bind (F) (P) (Field0)

    module Q =
      Common.Bind
        (F)
        (struct
          let prefix = "q"

          let outer_prefix = P.prefix
        end)
  end

  module Make
      (Field0 : Deletable_intf)
      (Bindings : Bound
                  with type 'a return = 'a
                   and type 'a result = 'a
                   and type field = Field0.t) : S with type field = Field0.t =
  struct
    type field = Field0.t

    module R = R.Make (Field0) (Bindings.R)
    module Q = Common.Make (Bindings.Q)
  end
end

module Var (Field0 : Deletable_intf) = struct
  module type Bound = sig
    include Foreign_types

    type t = Field0.t Backend_types.Var.t

    val typ : t Ctypes.typ

    val delete : (t -> unit return) result

    val index : (t -> Unsigned.size_t return) result

    val create : (int -> t return) result
  end

  module type S = sig
    type t = Field0.t Backend_types.Var.t

    val typ : t Ctypes.typ

    val index : t -> int

    val create : int -> t
  end

  module Bind
      (F : Ctypes.FOREIGN) (P : sig
          val prefix : string
      end) :
    Bound with type 'a return = 'a F.return and type 'a result = 'a F.result =
  struct
    include F

    include Var.Make
              (F)
              (struct
                let prefix = with_prefix P.prefix "var"

                type field = Field0.t
              end)

    let create = foreign (func_name "create") (int @-> returning typ)

    let index = foreign (func_name "index") (typ @-> returning size_t)
  end

  module Make
      (Bindings : Bound with type 'a return = 'a and type 'a result = 'a) : S =
  struct
    include Bindings

    let create n =
      let v = create n in
      Caml.Gc.finalise delete v ; v

    let index v =
      let r = index v in
      (* TODO: Use size_t *)
      Unsigned.Size_t.to_int r
  end
end

module Make_common (M : sig
  val prefix : string
end) =
struct
  let prefix = M.prefix

  let init =
    foreign
      (with_prefix M.prefix "init_public_params")
      (void @-> returning void)

  let () = init ()

  module Field0 : sig
    type t [@@deriving sexp]

    include Deletable_intf with type t := t

    val func_name : string -> string
  end = struct
    module F =
      Make_foreign
        (Ctypes_foreign)
        (struct
          let prefix = with_prefix prefix "field"
        end)

    type t = F.t sexp_opaque [@@deriving sexp]

    include (F : module type of F with type t := t)
  end

  module Bigint =
    Bigint.Make (Field0) (Bigint.Bind (Ctypes_foreign) (M) (Field0))

  module Field = struct
    include Field.Make (Field0) (Bigint.R)
              (Field.Bind
                 (Ctypes_foreign)
                 (struct
                   include Field0

                   let outer_prefix = M.prefix
                 end))
  end

  module Var = struct
    module T = Var (Field0)
    include T.Make (T.Bind (Ctypes_foreign) (M))
  end

  module Linear_combination : sig
    type t = Field0.t Backend_types.Linear_combination.t

    val typ : t Ctypes.typ

    val create : unit -> t

    val of_var : Var.t -> t

    val of_int : int -> t

    val of_field : Field.t -> t

    val print : t -> unit

    module Term : sig
      type t = Field0.t Backend_types.Linear_combination.Term.t

      val create : Field.t -> Var.t -> t

      val coeff : t -> Field.t

      val var : t -> Var.t

      module Vector : Vector.S with type elt = t
    end

    val terms : t -> Term.Vector.t

    module Vector : Vector.S with type elt = t

    val add_term : t -> Field.t -> Var.t -> unit
  end = struct
    let prefix = with_prefix M.prefix "linear_combination"

    include Linear_combination.Make
              (Ctypes_foreign)
              (struct
                let prefix = prefix

                type field = Field0.t
              end)

    module Term = struct
      let prefix = with_prefix prefix "term"

      include Linear_combination.Term.Make
                (Ctypes_foreign)
                (struct
                  let prefix = prefix

                  type field = Field0.t
                end)

      let create =
        let stub =
          foreign (func_name "create") (Field.typ @-> Var.typ @-> returning typ)
        in
        fun x v ->
          let t = stub x v in
          Caml.Gc.finalise delete t ; t

      let coeff =
        let stub = foreign (func_name "coeff") (typ @-> returning Field.typ) in
        fun t ->
          let x = stub t in
          Caml.Gc.finalise Field.delete x ;
          x

      let var =
        let stub = foreign (func_name "index") (typ @-> returning int) in
        fun t -> Var.create (stub t)

      module Vector = struct
        module Bindings =
          Vector.Bind
            (Ctypes_foreign)
            (struct
              type nonrec t = t

              let typ = typ

              let prefix = with_prefix prefix "vector"
            end)

        include Vector.Make (struct
                    type nonrec t = t

                    let schedule_delete = Caml.Gc.finalise delete
                  end)
                  (Bindings)
      end
    end

    let schedule_delete t = Caml.Gc.finalise delete t

    module Vector = struct
      module Bindings =
        Vector.Bind
          (Ctypes_foreign)
          (struct
            type nonrec t = t

            let typ = typ

            let prefix = with_prefix prefix "vector"
          end)

      include Vector.Make (struct
                  type nonrec t = t

                  let schedule_delete = schedule_delete
                end)
                (Bindings)
    end

    let print = foreign (func_name "print") (typ @-> returning void)

    (*
    let substitute =
      foreign (func_name "substitute")
        (typ @-> Var.typ @-> Term.Vector.typ @-> returning void)
    ;; *)

    let create : unit -> t =
      let stub = foreign (func_name "create") (void @-> returning typ) in
      fun () ->
        let t = stub () in
        schedule_delete t ; t

    let of_var : Var.t -> t =
      let stub = foreign (func_name "of_var") (Var.typ @-> returning typ) in
      fun v ->
        let t = stub v in
        schedule_delete t ; t

    let of_int : int -> t =
      let stub = foreign (func_name "of_int") (int @-> returning typ) in
      fun n ->
        let t = stub n in
        schedule_delete t ; t

    let add_term =
      foreign (func_name "add_term")
        (typ @-> Field.typ @-> Var.typ @-> returning void)

    let terms =
      let stub =
        foreign (func_name "terms") (typ @-> returning Term.Vector.typ)
      in
      fun t ->
        let v = stub t in
        Caml.Gc.finalise Term.Vector.delete v ;
        v

    let of_field : Field.t -> t =
      let stub =
        foreign (func_name "of_field") (Field.typ @-> returning typ)
      in
      fun n ->
        let t = stub n in
        schedule_delete t ; t
  end

  module R1CS_constraint : sig
    type t = Field0.t Backend_types.R1CS_constraint.t

    val typ : t Ctypes.typ

    val create :
      Linear_combination.t -> Linear_combination.t -> Linear_combination.t -> t

    val set_is_square : t -> bool -> unit

    val a : t -> Linear_combination.t

    val b : t -> Linear_combination.t

    val c : t -> Linear_combination.t
  end = struct
    include R1CS_constraint.Make
              (Ctypes_foreign)
              (struct
                let prefix = with_prefix M.prefix "r1cs_constraint"

                type field = Field0.t
              end)

    let create =
      let stub =
        foreign (func_name "create")
          ( Linear_combination.typ @-> Linear_combination.typ
          @-> Linear_combination.typ @-> returning typ )
      in
      fun a b c ->
        let t = stub a b c in
        Caml.Gc.finalise delete t ; t

    let set_is_square =
      foreign (func_name "set_is_square") (typ @-> bool @-> returning void)

    let a =
      let stub =
        foreign (func_name "a") (typ @-> returning Linear_combination.typ)
      in
      fun t -> stub t

    let b =
      let stub =
        foreign (func_name "b") (typ @-> returning Linear_combination.typ)
      in
      fun t -> stub t

    let c =
      let stub =
        foreign (func_name "c") (typ @-> returning Linear_combination.typ)
      in
      fun t -> stub t
  end

  module R1CS_constraint_system : sig
    type t = Field0.t Backend_types.R1CS_constraint_system.t

    val typ : t Ctypes.typ

    val create : unit -> t

    val clear : t -> unit

    val delete : t -> unit

    val report_statistics : t -> unit

    val swap_AB_if_beneficial : t -> unit

    val add_constraint : t -> R1CS_constraint.t -> unit

    val add_constraint_with_annotation :
      t -> R1CS_constraint.t -> string -> unit

    val set_primary_input_size : t -> int -> unit

    val set_auxiliary_input_size : t -> int -> unit

    val get_primary_input_size : t -> int

    val get_auxiliary_input_size : t -> int

    val check_exn : t -> unit

    val is_satisfied :
         t
      -> primary_input:Field.Vector.t
      -> auxiliary_input:Field.Vector.t
      -> bool

    val digest : t -> Md5.t

    val iter_constraints : f:(R1CS_constraint.t -> unit) -> t -> unit

    val fold_constraints :
      f:('a -> R1CS_constraint.t -> 'a) -> init:'a -> t -> 'a
  end = struct
    include R1CS_constraint_system.Make
              (Ctypes_foreign)
              (struct
                let prefix = with_prefix M.prefix "r1cs_constraint_system"

                type field = Field0.t
              end)

    let report_statistics =
      foreign (func_name "report_statistics") (typ @-> returning void)

    let swap_AB_if_beneficial =
      foreign (func_name "swap_AB_if_beneficial") (typ @-> returning void)

    let check_exn =
      let stub = foreign (func_name "check") (typ @-> returning bool) in
      fun sys ->
        if not (stub sys) then failwith "R1CS_constraint_system.check_exn"

    let create =
      let stub = foreign (func_name "create") (void @-> returning typ) in
      fun () ->
        let t = stub () in
        Caml.Gc.finalise delete t ; t

    let clear = foreign (func_name "clear") (typ @-> returning void)

    let add_constraint =
      foreign
        (func_name "add_constraint")
        (typ @-> R1CS_constraint.typ @-> returning void)

    let add_constraint_with_annotation =
      foreign
        (func_name "add_constraint_with_annotation")
        (typ @-> R1CS_constraint.typ @-> string @-> returning void)

    let set_primary_input_size =
      foreign
        (func_name "set_primary_input_size")
        (typ @-> int @-> returning void)

    let set_auxiliary_input_size =
      foreign
        (func_name "set_auxiliary_input_size")
        (typ @-> int @-> returning void)

    let get_primary_input_size =
      foreign (func_name "get_primary_input_size") (typ @-> returning int)

    let get_auxiliary_input_size =
      foreign (func_name "get_auxiliary_input_size") (typ @-> returning int)

    let is_satisfied =
      let stub =
        foreign (func_name "is_satisfied")
          ( typ @-> Field.Vector.typ @-> Field.Vector.typ
          @-> returning Ctypes.bool )
      in
      fun t ~primary_input ~auxiliary_input ->
        stub t primary_input auxiliary_input

    let digest =
      let stub =
        foreign (func_name "digest") (typ @-> returning Cpp_string.typ)
      in
      fun t ->
        let s = stub t in
        let r = Cpp_string.to_string s in
        Cpp_string.delete s ; Md5.of_binary_exn r

    let iter_constraints =
      let stub =
        foreign (func_name "iter")
          ( typ
          @-> funptr (R1CS_constraint.typ @-> returning void)
          @-> returning void )
      in
      fun ~f t -> stub t f

    let fold_constraints ~f ~init t =
      let a = ref init in
      let f c = a := f !a c in
      iter_constraints ~f t ; !a
  end

  module Protoboard : sig
    type t

    val typ : t Ctypes.typ

    val create : unit -> t

    val auxiliary_input : t -> Field.Vector.t

    val num_variables : t -> int

    val set_input_sizes : t -> int -> unit

    val renumber_and_append_constraints :
         t
      -> R1CS_constraint_system.t
      -> Linear_combination.Vector.t
      -> int
      -> unit

    module Variable : sig
      type t

      val typ : t Ctypes.typ

      val delete : t -> unit

      val index : t -> int

      val of_int : int -> t
    end

    module Variable_array : sig
      type t

      val typ : t Ctypes.typ

      val emplace_back : t -> Variable.t -> unit

      val create : unit -> t

      val delete : t -> unit

      val get : t -> int -> Variable.t
    end

    val set_variable : t -> Variable.t -> Field.t -> unit

    val get_variable : t -> Variable.t -> Field.t

    val allocate_variable : t -> Variable.t

    val allocate_variable_array : t -> int -> Variable_array.t

    val augment_variable_annotation : t -> Variable.t -> string -> unit
  end = struct
    include Make_foreign
              (Ctypes_foreign)
              (struct
                let prefix = with_prefix M.prefix "protoboard"
              end)

    module Variable : sig
      type t

      val typ : t Ctypes.typ

      val delete : t -> unit

      val of_int : int -> t

      val index : t -> int
    end = struct
      include Make_foreign
                (Ctypes_foreign)
                (struct
                  let prefix = with_prefix M.prefix "protoboard_variable"
                end)

      let of_int =
        let stub = foreign (func_name "of_int") (int @-> returning typ) in
        fun i ->
          let t = stub i in
          Caml.Gc.finalise delete t ; t

      let index = foreign (func_name "index") (typ @-> returning int)
    end

    module Variable_array = struct
      include Make_foreign
                (Ctypes_foreign)
                (struct
                  let prefix = with_prefix M.prefix "protoboard_variable_array"
                end)

      let create =
        let stub = foreign (func_name "create") (void @-> returning typ) in
        fun () ->
          let t = stub () in
          Caml.Gc.finalise delete t ; t

      let emplace_back =
        foreign (func_name "emplace_back")
          (typ @-> Variable.typ @-> returning void)

      let get =
        let stub =
          foreign (func_name "get") (typ @-> int @-> returning Variable.typ)
        in
        fun t i ->
          let v = stub t i in
          Caml.Gc.finalise Variable.delete v ;
          v
    end

    let renumber_and_append_constraints =
      foreign
        (func_name "renumber_and_append_constraints")
        ( typ @-> R1CS_constraint_system.typ @-> Linear_combination.Vector.typ
        @-> int @-> returning void )

    let augment_variable_annotation =
      foreign
        (func_name "augment_variable_annotation")
        (typ @-> Variable.typ @-> string @-> returning void)

    let create =
      let stub = foreign (func_name "create") (void @-> returning typ) in
      fun () ->
        let t = stub () in
        Caml.Gc.finalise delete t ; t

    let num_variables =
      foreign (func_name "num_variables") (typ @-> returning int)

    let set_input_sizes =
      foreign (func_name "set_input_sizes") (typ @-> int @-> returning void)

    let set_variable =
      foreign (func_name "set_variable")
        (typ @-> Variable.typ @-> Field.typ @-> returning void)

    let get_variable =
      let stub =
        foreign (func_name "get_variable")
          (typ @-> Variable.typ @-> returning Field.typ)
      in
      fun t v ->
        let x = stub t v in
        Caml.Gc.finalise Field.delete x ;
        x

    let allocate_variable =
      let stub =
        foreign (func_name "allocate_variable") (typ @-> returning Variable.typ)
      in
      fun t ->
        let v = stub t in
        Caml.Gc.finalise Variable.delete v ;
        v

    let allocate_variable_array =
      let stub =
        foreign
          (func_name "allocate_variable_array")
          (typ @-> int @-> returning Variable_array.typ)
      in
      fun t n ->
        let v = stub t n in
        Caml.Gc.finalise Variable_array.delete v ;
        v

    let auxiliary_input =
      let stub =
        foreign
          (func_name "auxiliary_input")
          (typ @-> returning Field.Vector.typ)
      in
      fun t ->
        let v = stub t in
        Caml.Gc.finalise Field.Vector.delete v ;
        v
  end

  let field_size =
    let stub =
      foreign
        (with_prefix M.prefix "field_size")
        (void @-> returning Bigint.R.typ)
    in
    stub ()
end

module type Proof_system_inputs_intf = sig
  val prefix : string

  module R1CS_constraint_system : sig
    type t

    val typ : t Ctypes.typ

    val clear : t -> unit
  end

  module Field : sig
    type t

    module Vector : sig
      type t

      val typ : t Ctypes.typ
    end
  end
end

module Make_proof_system_keys (M : Proof_system_inputs_intf) = struct
  module Proving_key : sig
    type t [@@deriving bin_io]

    val func_name : string -> string

    val typ : t Ctypes.typ

    val r1cs_constraint_system : t -> M.R1CS_constraint_system.t

    val delete : t -> unit

    val to_string : t -> string

    val of_string : string -> t

    val to_bigstring : t -> Bigstring.t

    val of_bigstring : Bigstring.t -> t
  end = struct
    include Proving_key.Make
              (Ctypes_foreign)
              (struct
                let prefix = with_prefix M.prefix "proving_key"
              end)

    let r1cs_constraint_system =
      foreign
        (func_name "r1cs_constraint_system")
        (typ @-> returning M.R1CS_constraint_system.typ)

    let to_cpp_string_stub : t -> Cpp_string.t =
      let stub =
        foreign (func_name "to_string") (typ @-> returning Cpp_string.typ)
      in
      fun t ->
        M.R1CS_constraint_system.clear (r1cs_constraint_system t) ;
        stub t

    let to_string : t -> string =
     fun t ->
      let s = to_cpp_string_stub t in
      let r = Cpp_string.to_string s in
      Cpp_string.delete s ; r

    let of_cpp_string_stub =
      foreign (func_name "of_string") (Cpp_string.typ @-> returning typ)

    let of_string : string -> t =
     fun s ->
      let str = Cpp_string.of_string_don't_delete s in
      let t = of_cpp_string_stub str in
      Cpp_string.delete str ; t

    include Bin_prot.Utils.Of_minimal (struct
      type nonrec t = t

      let bin_shape_t = String.bin_shape_t

      let bin_size_t t =
        let s = to_cpp_string_stub t in
        let len = Cpp_string.length s in
        let plen = Bin_prot.Nat0.of_int len in
        let size_len = Bin_prot.Size.bin_size_nat0 plen in
        let res = size_len + len in
        Cpp_string.delete s ; res

      let bin_write_t buf ~pos t =
        let s = to_cpp_string_stub t in
        let len = Cpp_string.length s in
        let plen = Bin_prot.Nat0.unsafe_of_int len in
        let new_pos = Bin_prot.Write.bin_write_nat0 buf ~pos plen in
        let next = new_pos + len in
        Bin_prot.Common.check_next buf next ;
        let bs = Cpp_string.to_bigstring s in
        Bigstring.blit ~src:bs ~dst:buf ~src_pos:0 ~dst_pos:new_pos ~len ;
        Cpp_string.delete s ;
        next

      let bin_read_t buf ~pos_ref =
        let len = (Bin_prot.Read.bin_read_nat0 buf ~pos_ref :> int) in
        let pos = !pos_ref in
        let next = pos + len in
        Bin_prot.Common.check_next buf next ;
        pos_ref := next ;
        let cpp_str =
          let pointer =
            Ctypes.( +@ ) (Ctypes.bigarray_start Ctypes.array1 buf) pos
          in
          Cpp_string.of_char_pointer_don't_delete pointer len
        in
        let result = of_cpp_string_stub cpp_str in
        Cpp_string.delete cpp_str ; result

      let __bin_read_t__ _buf ~pos_ref _vint =
        Bin_prot.Common.raise_variant_wrong_type "Proving_key.t" !pos_ref
    end)

    let to_bigstring : t -> Bigstring.t =
      let stub =
        foreign (func_name "to_string") (typ @-> returning Cpp_string.typ)
      in
      fun t ->
        let str = stub t in
        let length = Cpp_string.length str in
        let char_star = Cpp_string.to_char_pointer str in
        let bs =
          Ctypes.bigarray_of_ptr Ctypes.array1 length Bigarray.Char char_star
        in
        Caml.Gc.finalise (fun _ -> Cpp_string.delete str) bs ;
        bs

    let of_bigstring : Bigstring.t -> t =
      let stub =
        foreign (func_name "of_string") (Cpp_string.typ @-> returning typ)
      in
      fun bs ->
        let char_star = Ctypes.bigarray_start Ctypes.array1 bs in
        let str =
          Cpp_string.of_char_pointer_don't_delete char_star
            (Bigstring.length bs)
        in
        let t = stub str in
        Caml.Gc.finalise (fun _ -> delete t) t ;
        t
  end

  module Verification_key : sig
    type t

    val typ : t Ctypes.typ

    val delete : t -> unit

    val to_string : t -> string

    val of_string : string -> t

    val to_bigstring : t -> Bigstring.t

    val of_bigstring : Bigstring.t -> t

    val size_in_bits : t -> int
  end = struct
    include Verification_key.Make
              (Ctypes_foreign)
              (struct
                let prefix = with_prefix M.prefix "verification_key"
              end)

    let size_in_bits =
      foreign (func_name "size_in_bits") (typ @-> returning int)

    let to_string : t -> string =
      let stub =
        foreign (func_name "to_string") (typ @-> returning Cpp_string.typ)
      in
      fun t ->
        let s = stub t in
        let r = Cpp_string.to_string s in
        Cpp_string.delete s ; r

    let of_string : string -> t =
      let stub =
        foreign (func_name "of_string") (Cpp_string.typ @-> returning typ)
      in
      fun s ->
        let str = Cpp_string.of_string_don't_delete s in
        let t = stub str in
        Cpp_string.delete str ; t

    let to_bigstring : t -> Bigstring.t =
      let stub =
        foreign (func_name "to_string") (typ @-> returning Cpp_string.typ)
      in
      fun t ->
        let str = stub t in
        let length = Cpp_string.length str in
        let char_star = Cpp_string.to_char_pointer str in
        let bs =
          Ctypes.bigarray_of_ptr Ctypes.array1 length Bigarray.Char char_star
        in
        Caml.Gc.finalise (fun _ -> Cpp_string.delete str) bs ;
        bs

    let of_bigstring : Bigstring.t -> t =
      let stub =
        foreign (func_name "of_string") (Cpp_string.typ @-> returning typ)
      in
      fun bs ->
        let char_star = Ctypes.bigarray_start Ctypes.array1 bs in
        let str =
          Cpp_string.of_char_pointer_don't_delete char_star
            (Bigstring.length bs)
        in
        let t = stub str in
        Caml.Gc.finalise (fun _ -> delete t) t ;
        t
  end

  module Keypair : sig
    type t

    val typ : t Ctypes.typ

    val delete : t -> unit

    val pk : t -> Proving_key.t

    val vk : t -> Verification_key.t

    val create : M.R1CS_constraint_system.t -> t
  end = struct
    include Keypair.Make
              (Ctypes_foreign)
              (struct
                let prefix = with_prefix M.prefix "keypair"
              end)

    let pk =
      let stub =
        foreign (func_name "pk") (typ @-> returning Proving_key.typ)
      in
      fun t ->
        let k = stub t in
        Caml.Gc.finalise Proving_key.delete k ;
        k

    let vk =
      let stub =
        foreign (func_name "vk") (typ @-> returning Verification_key.typ)
      in
      fun t ->
        let k = stub t in
        Caml.Gc.finalise Verification_key.delete k ;
        k

    let create =
      let stub =
        foreign (func_name "create")
          (M.R1CS_constraint_system.typ @-> returning typ)
      in
      fun sys ->
        let t = stub sys in
        Caml.Gc.finalise delete t ; t
  end
end

module Make_proof_system (M : sig
  val prefix : string

  module R1CS_constraint_system : sig
    type t

    val typ : t Ctypes.typ

    val clear : t -> unit
  end

  module Field : sig
    type t

    module Vector : sig
      type t

      val typ : t Ctypes.typ
    end
  end
end) =
struct
  include Make_proof_system_keys (M)

  module Proof : sig
    type message = unit

    type t

    val typ : t Ctypes.typ

    val create :
         ?message:message
      -> Proving_key.t
      -> primary:M.Field.Vector.t
      -> auxiliary:M.Field.Vector.t
      -> t

    val verify :
      ?message:message -> t -> Verification_key.t -> M.Field.Vector.t -> bool

    include Binable.S with type t := t
  end = struct
    include Proof.Make
              (Ctypes_foreign)
              (struct
                let prefix = with_prefix M.prefix "proof"
              end)

    type message = unit

    include Binable.Of_stringable (struct
      type nonrec t = t

      let to_string : t -> string =
        let stub =
          foreign (func_name "to_string") (typ @-> returning Cpp_string.typ)
        in
        fun t ->
          let s = stub t in
          let r = Cpp_string.to_string s in
          Cpp_string.delete s ; r

      let of_string : string -> t =
        let stub =
          foreign (func_name "of_string") (Cpp_string.typ @-> returning typ)
        in
        fun s ->
          let str = Cpp_string.of_string_don't_delete s in
          let t = stub str in
          Cpp_string.delete str ; t
    end)

    let create_ =
      let stub =
        foreign (func_name "create")
          ( Proving_key.typ @-> M.Field.Vector.typ @-> M.Field.Vector.typ
          @-> returning typ )
      in
      fun k primary auxiliary ->
        let t = stub k primary auxiliary in
        Caml.Gc.finalise delete t ; t

    let create ?message:_ key ~primary ~auxiliary =
      create_ key primary auxiliary

    let verify =
      let stub =
        foreign (func_name "verify")
          ( typ @-> Verification_key.typ @-> M.Field.Vector.typ
          @-> returning bool )
      in
      fun ?message:_ t k primary -> stub t k primary
  end
end

module Make_full (M : sig
  val prefix : string
end) =
struct
  module Common = Make_common (struct
    let prefix = M.prefix
  end)

  module Prefix = M

  module type Common_intf = module type of Common

  module Default = struct
    module R1CS_constraint_system = struct
      include Common.R1CS_constraint_system

      let finalize = Common.R1CS_constraint_system.swap_AB_if_beneficial
    end

    include (
      Common :
        module type of Common
        with module Field0 := Common.Field0
         and module R1CS_constraint_system := Common.R1CS_constraint_system )

    include Make_proof_system (Common)
  end

  module GM = struct
    module R1CS_constraint_system = struct
      include Common.R1CS_constraint_system

      let finalize = ignore
    end

    include (
      Common :
        module type of Common
        with module Field0 := Common.Field0
         and module R1CS_constraint_system := Common.R1CS_constraint_system )

    include Make_proof_system (struct
      include Common

      let prefix = with_prefix M.prefix "gm"
    end)
  end

  include Common
end

module Bn128 = Make_full (struct
  let prefix = "camlsnark_bn128"
end)

module Make_Groth16_verification_key_accessors (Prefix : sig
  val prefix : string
end)
(Verification_key : Foreign_intf) (G1 : sig
    include Deletable_intf

    module Vector : Deletable_intf
end)
(G2 : Deletable_intf)
(Fqk : Deletable_intf) =
struct
  open Prefix

  let prefix = with_prefix prefix "verification_key"

  let func_name = with_prefix prefix

  let func name ret delete =
    let stub =
      foreign (func_name name) (Verification_key.typ @-> returning ret)
    in
    fun vk ->
      let r = stub vk in
      Caml.Gc.finalise delete r ; r

  let delta = func "delta" G2.typ G2.delete

  let query = func "query" G1.Vector.typ G1.Vector.delete

  let alpha_beta = func "alpha_beta" Fqk.typ Fqk.delete
end

module Make_proof_accessors (Prefix : sig
  val prefix : string
end)
(Proof : Foreign_intf) (G1 : sig
    include Deletable_intf

    module Vector : Deletable_intf
end)
(G2 : Deletable_intf) =
struct
  open Prefix

  let func_name = with_prefix prefix

  let func name ret delete =
    let stub = foreign (func_name name) (Proof.typ @-> returning ret) in
    fun vk ->
      let r = stub vk in
      Caml.Gc.finalise delete r ; r

  let a = func "a" G1.typ G1.delete

  let b = func "b" G2.typ G2.delete

  let c = func "c" G1.typ G1.delete
end

module Make_bowe_gabizon (M : sig
  val prefix : string

  module R1CS_constraint_system : sig
    type t

    val typ : t Ctypes.typ

    val clear : t -> unit
  end

  module Field : sig
    include Deletable_intf

    val random : unit -> t

    module Vector : sig
      type t

      val typ : t Ctypes.typ
    end
  end

  module Fqk : Deletable_intf

  module G1 : sig
    include Deletable_intf

    include Binable.S with type t := t

    module Vector : Deletable_intf

    val scale_field : t -> Field.t -> t
  end

  module G2 : sig
    include Deletable_intf

    include Binable.S with type t := t
  end
end) (H : sig
  open M

  val hash :
       ?message:bool array
    -> a:G1.t
    -> b:G2.t
    -> c:G1.t
    -> delta_prime:G2.t
    -> G1.t
end) =
struct
  open M

  let bg_prefix = with_prefix M.prefix "bg"

  module Keys = Make_proof_system_keys (struct
    include M

    let prefix = bg_prefix
  end)

  module Proving_key = Keys.Proving_key
  module Keypair = Keys.Keypair

  module Verification_key = struct
    include Keys.Verification_key

    include Make_Groth16_verification_key_accessors (struct
                let prefix = bg_prefix
              end)
              (Keys.Verification_key)
              (G1)
              (G2)
              (Fqk)
  end

  module Proof = struct
    module Pre = struct
      module Prefix = struct
        let prefix = with_prefix M.prefix "bg_proof"
      end

      module T = struct
        include Make_foreign (Ctypes_foreign) (Prefix)
      end

      include T
      include Make_proof_accessors (Prefix) (T) (G1) (G2)

      let delta_prime = func "delta_prime" G2.typ G2.delete

      let verify_components ~a ~b ~c ~delta_prime ~z ~y_s key input =
        let stub =
          foreign
            (func_name "verify_components")
            ( G1.typ @-> G2.typ @-> G1.typ @-> G2.typ @-> G1.typ @-> G1.typ
            @-> Verification_key.typ @-> Field.Vector.typ @-> returning bool )
        in
        stub a b c delta_prime z y_s key input

      let create proving_key ~primary ~auxiliary ~d =
        let stub =
          foreign (func_name "create")
            ( Proving_key.typ @-> Field.typ @-> Field.Vector.typ
            @-> Field.Vector.typ @-> returning typ )
        in
        let t = stub proving_key d primary auxiliary in
        Caml.Gc.finalise delete t ; t
    end

    type message = bool array

    type t = {a: G1.t; b: G2.t; c: G1.t; delta_prime: G2.t; z: G1.t}
    [@@deriving bin_io]

    let create ?message proving_key ~primary ~auxiliary =
      let d = Field.random () in
      let pre = Pre.create proving_key ~primary ~auxiliary ~d in
      let a = Pre.a pre in
      let b = Pre.b pre in
      let c = Pre.c pre in
      let delta_prime = Pre.delta_prime pre in
      let y_s = H.hash ?message ~a ~b ~c ~delta_prime in
      let z = G1.scale_field y_s d in
      {a= Pre.a pre; b= Pre.b pre; c= Pre.c pre; z; delta_prime}

    let verify ?message {a; b; c; z; delta_prime} vk input =
      let y_s = H.hash ?message ~a ~b ~c ~delta_prime in
      Pre.verify_components ~a ~b ~c ~delta_prime ~y_s ~z vk input
  end
end

module Make_mnt_cycle (Security_level : sig
  val modulus_size : [`Bits298 | `Bits753]
end) =
struct
  let suffix =
    match Security_level.modulus_size with `Bits298 -> "" | `Bits753 -> "753"

  module Mnt4_0 = Make_full (struct
    let prefix = "camlsnark_mnt4" ^ suffix
  end)

  module Mnt6_0 = Make_full (struct
    let prefix = "camlsnark_mnt6" ^ suffix
  end)

  module Make_GM_verification_key_accessors (Prefix : sig
    val prefix : string
  end)
  (Gm_verification_key : Foreign_intf) (G1 : sig
      include Deletable_intf

      module Vector : Deletable_intf
  end)
  (G2 : Deletable_intf)
  (Fqk : Deletable_intf) =
  struct
    open Prefix

    let prefix = with_prefix prefix "gm_verification_key"

    let func_name = with_prefix prefix

    let func name ret delete =
      let stub =
        foreign (func_name name) (Gm_verification_key.typ @-> returning ret)
      in
      fun vk ->
        let r = stub vk in
        Caml.Gc.finalise delete r ; r

    let h = func "h" G2.typ G2.delete

    let g_alpha = func "g_alpha" G1.typ G1.delete

    let h_beta = func "h_beta" G2.typ G2.delete

    let g_gamma = func "g_gamma" G1.typ G1.delete

    let h_gamma = func "h_gamma" G2.typ G2.delete

    let query = func "query" G1.Vector.typ G1.Vector.delete

    let g_alpha_h_beta = func "g_alpha_h_beta" Fqk.typ Fqk.delete
  end

  module Make_fqk
      (Prefix : Prefix_intf) (Fq : sig
          include Deletable_intf

          module Vector : Deletable_intf
      end) =
  struct
    include Make_foreign
              (Ctypes_foreign)
              (struct
                let prefix = with_prefix Prefix.prefix "fqk"
              end)

    let one =
      let stub = foreign (func_name "one") (void @-> returning typ) in
      let x = stub () in
      Caml.Gc.finalise delete x ; x

    let to_elts =
      let stub =
        foreign (func_name "to_elts") (typ @-> returning Fq.Vector.typ)
      in
      fun t ->
        let v = stub t in
        Caml.Gc.finalise Fq.Vector.delete v ;
        v
  end

  module Mnt4 = struct
    include Mnt4_0
    module Fqk = Make_fqk (Prefix) (Mnt6_0.Field)

    let%test "fqk4" =
      let v = Fqk.to_elts Fqk.one in
      Mnt6_0.Field.Vector.length v = 4

    module G2 = struct
      module T = Group (Mnt4_0.Field) (Mnt4_0.Bigint.R) (Mnt6_0.Field.Vector)

      include T.Make
                (T.Bind
                   (Ctypes_foreign)
                   (struct
                     let prefix = with_prefix Mnt4_0.prefix "g2"
                   end))
    end

    module G1 = struct
      module T = struct
        module T' = Group (Mnt4_0.Field) (Mnt4_0.Bigint.R) (Mnt6_0.Field)

        include T'.Make
                  (T'.Bind
                     (Ctypes_foreign)
                     (struct
                       let prefix = with_prefix Mnt4_0.prefix "g1"
                     end))
      end

      include T

      let%test "scalar_mul" =
        let g = one in
        equal (g + g + g + g + g) (scale_field g (Field.of_int 5))

      module Coefficients = struct
        module T = Group_coefficients (Mnt6_0.Field)

        include T.Make
                  (T.Bind
                     (Ctypes_foreign)
                     (struct
                       let prefix = with_prefix Mnt4_0.prefix "g1"
                     end))
      end

      module Window_table = struct
        module T' = Window_table (T) (Field) (Bigint.R) (Vector)

        include T'.Make
                  (T'.Bind
                     (Ctypes_foreign)
                     (struct
                       let prefix = with_prefix Mnt4_0.prefix "g1"
                     end))
                     (T)
                  (Bigint.R)
                  (Vector)
      end

      let%test "window-scale" =
        let table = Window_table.create one in
        let s = Bigint.R.of_field (Field.random ()) in
        equal (Window_table.scale table s) (scale one s)

      let%test "window-base" =
        let rec random_curve_point () =
          let module Field = Mnt6_0.Field in
          let ( + ) = Field.add in
          let ( * ) = Field.mul in
          let x = Field.random () in
          let f = (x * x * x) + (Coefficients.a * x) + Coefficients.b in
          if Field.is_square f then of_affine (x, Field.sqrt f)
          else random_curve_point ()
        in
        let g = random_curve_point () in
        let table = Window_table.create g in
        let s = Bigint.R.of_field Field.one in
        equal (Window_table.scale table s) g

      let%test "coefficients correct" =
        let x, y = to_affine_exn one in
        let open Mnt6_0.Field in
        let ( + ) = add in
        let ( * ) = mul in
        equal (square y) ((x * x * x) + (Coefficients.a * x) + Coefficients.b)
    end

    module GM_proof_accessors =
      Make_proof_accessors (struct
          let prefix = with_prefix Prefix.prefix "gm_proof"
        end)
        (GM.Proof)
        (G1)
        (G2)

    module GM_verification_key_accessors =
      Make_GM_verification_key_accessors (Prefix) (GM.Verification_key) (G1)
        (G2)
        (Fqk)

    module Groth16_proof_accessors =
      Make_proof_accessors (struct
          let prefix = with_prefix Prefix.prefix "proof"
        end)
        (Default.Proof)
        (G1)
        (G2)

    module Groth16_verification_key_accessors =
      Make_Groth16_verification_key_accessors
        (Prefix)
        (Default.Verification_key)
        (G1)
        (G2)
        (Fqk)
  end

  module Mnt6 = struct
    include Mnt6_0
    module Fqk = Make_fqk (Prefix) (Mnt4_0.Field)

    let%test "fqk6" =
      let v = Fqk.to_elts Fqk.one in
      Mnt4_0.Field.Vector.length v = 6

    module G2 = struct
      module T = Group (Mnt6_0.Field) (Mnt6_0.Bigint.R) (Mnt4_0.Field.Vector)

      include T.Make
                (T.Bind
                   (Ctypes_foreign)
                   (struct
                     let prefix = with_prefix Mnt6_0.prefix "g2"
                   end))
    end

    module G1 = struct
      module T = struct
        module T' = Group (Mnt6_0.Field) (Mnt6_0.Bigint.R) (Mnt4_0.Field)

        include T'.Make
                  (T'.Bind
                     (Ctypes_foreign)
                     (struct
                       let prefix = with_prefix Mnt6_0.prefix "g1"
                     end))
      end

      include T

      let%test "scalar_mul" =
        let g = one in
        equal (g + g + g + g + g) (scale_field g (Field.of_int 5))

      module Coefficients = struct
        module T = Group_coefficients (Mnt4_0.Field)

        include T.Make
                  (T.Bind
                     (Ctypes_foreign)
                     (struct
                       let prefix = with_prefix Mnt6_0.prefix "g1"
                     end))
      end

      module Window_table = struct
        module T' = Window_table (T) (Field) (Bigint.R) (Vector)

        include T'.Make
                  (T'.Bind
                     (Ctypes_foreign)
                     (struct
                       let prefix = with_prefix Mnt6_0.prefix "g1"
                     end))
                     (T)
                  (Bigint.R)
                  (Vector)
      end

      let%test "window-scale" =
        let table = Window_table.create one in
        let s = Bigint.R.of_field (Field.random ()) in
        equal (Window_table.scale table s) (scale one s)

      let%test "coefficients correct" =
        let x, y = to_affine_exn one in
        let open Mnt4_0.Field in
        let ( + ) = add in
        let ( * ) = mul in
        equal (square y) ((x * x * x) + (Coefficients.a * x) + Coefficients.b)
    end

    module GM_proof_accessors =
      Make_proof_accessors (struct
          let prefix = with_prefix Prefix.prefix "gm_proof"
        end)
        (GM.Proof)
        (G1)
        (G2)

    module GM_verification_key_accessors =
      Make_GM_verification_key_accessors (Prefix) (GM.Verification_key) (G1)
        (G2)
        (Fqk)

    module Groth16_proof_accessors =
      Make_proof_accessors (struct
          let prefix = with_prefix Prefix.prefix "proof"
        end)
        (Default.Proof)
        (G1)
        (G2)

    module Groth16_verification_key_accessors =
      Make_Groth16_verification_key_accessors
        (Prefix)
        (Default.Verification_key)
        (G1)
        (G2)
        (Fqk)
  end
end

module Mnt298 = Make_mnt_cycle (struct
  let modulus_size = `Bits298
end)

module Mnt4 = Mnt298.Mnt4
module Mnt6 = Mnt298.Mnt6

module Mnt753 = Make_mnt_cycle (struct
  let modulus_size = `Bits753
end)

module Mnt4753 = Mnt753.Mnt4
module Mnt6753 = Mnt753.Mnt6

module type S = sig
  val prefix : string

  val init : unit -> unit

  module Field : sig
    type t

    val typ : t Ctypes.typ

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val inv : t -> t

    val is_square : t -> bool

    val sqrt : t -> t

    val square : t -> t

    val of_int : int -> t

    val one : t

    val zero : t

    val equal : t -> t -> bool

    val size_in_bits : int

    val random : unit -> t

    val delete : t -> unit

    val print : t -> unit

    module Vector : sig
      type elt = t

      type t

      val typ : t Ctypes.typ

      val delete : t -> unit

      val create : unit -> t

      val get : t -> int -> elt

      val emplace_back : t -> elt -> unit

      val length : t -> int
    end
  end

  module Var : sig
    type t

    val typ : t Ctypes.typ

    val index : t -> int

    val create : int -> t
  end

  module Linear_combination : sig
    type t

    val typ : t Ctypes.typ

    val create : unit -> t

    val of_var : Var.t -> t

    val of_int : int -> t

    val of_field : Field.t -> t

    val print : t -> unit

    module Term : sig
      type t

      val create : Field.t -> Var.t -> t

      val coeff : t -> Field.t

      val var : t -> Var.t

      module Vector : sig
        type elt = t

        type t

        val typ : t Ctypes.typ

        val delete : t -> unit

        val create : unit -> t

        val get : t -> int -> elt

        val emplace_back : t -> elt -> unit

        val length : t -> int
      end
    end

    val terms : t -> Term.Vector.t

    module Vector : sig
      type elt = t

      type t

      val typ : t Ctypes.typ

      val delete : t -> unit

      val create : unit -> t

      val get : t -> int -> elt

      val emplace_back : t -> elt -> unit

      val length : t -> int
    end

    val add_term : t -> Field.t -> Var.t -> unit
  end

  module R1CS_constraint : sig
    type t

    val typ : t Ctypes.typ

    val create :
      Linear_combination.t -> Linear_combination.t -> Linear_combination.t -> t
  end

  module R1CS_constraint_system : sig
    type t

    val typ : t Ctypes.typ

    val create : unit -> t

    val delete : t -> unit

    val report_statistics : t -> unit

    val add_constraint : t -> R1CS_constraint.t -> unit

    val add_constraint_with_annotation :
      t -> R1CS_constraint.t -> string -> unit

    val set_primary_input_size : t -> int -> unit

    val set_auxiliary_input_size : t -> int -> unit

    val get_primary_input_size : t -> int

    val get_auxiliary_input_size : t -> int

    val check_exn : t -> unit

    val is_satisfied :
         t
      -> primary_input:Field.Vector.t
      -> auxiliary_input:Field.Vector.t
      -> bool

    val digest : t -> Core.Md5.t
  end

  module Protoboard : sig
    type t

    val typ : t Ctypes.typ

    val create : unit -> t

    val auxiliary_input : t -> Field.Vector.t

    val num_variables : t -> int

    val set_input_sizes : t -> int -> unit

    val renumber_and_append_constraints :
         t
      -> R1CS_constraint_system.t
      -> Linear_combination.Vector.t
      -> int
      -> unit

    module Variable : sig
      type t

      val typ : t Ctypes.typ

      val delete : t -> unit

      val index : t -> int

      val of_int : int -> t
    end

    module Variable_array : sig
      type t

      val typ : t Ctypes.typ

      val emplace_back : t -> Variable.t -> unit

      val create : unit -> t

      val delete : t -> unit

      val get : t -> int -> Variable.t
    end

    val set_variable : t -> Variable.t -> Field.t -> unit

    val get_variable : t -> Variable.t -> Field.t

    val allocate_variable : t -> Variable.t

    val allocate_variable_array : t -> int -> Variable_array.t

    val augment_variable_annotation : t -> Variable.t -> string -> unit
  end

  module Bigint : sig
    module R : sig
      type t

      val typ : t Ctypes.typ

      val of_decimal_string : string -> t

      val of_numeral : string -> base:int -> t

      val of_field : Field.t -> t

      val div : t -> t -> t

      val to_field : t -> Field.t

      val compare : t -> t -> int

      val test_bit : t -> int -> bool

      val find_wnaf : Unsigned.Size_t.t -> t -> Long_vector.t
    end

    module Q : sig
      type t

      val typ : t Ctypes.typ

      val test_bit : t -> int -> bool

      val find_wnaf : Unsigned.Size_t.t -> t -> Long_vector.t
    end
  end

  val field_size : Bigint.R.t

  module Proving_key : sig
    type t [@@deriving bin_io]

    val typ : t Ctypes.typ

    val r1cs_constraint_system : t -> R1CS_constraint_system.t

    val delete : t -> unit

    val to_string : t -> string

    val of_string : string -> t

    val to_bigstring : t -> Core.Bigstring.t

    val of_bigstring : Core.Bigstring.t -> t
  end

  module Verification_key : sig
    type t

    val typ : t Ctypes.typ

    val delete : t -> unit

    val to_string : t -> string

    val of_string : string -> t

    val to_bigstring : t -> Core.Bigstring.t

    val of_bigstring : Core.Bigstring.t -> t

    val size_in_bits : t -> int
  end

  module Keypair : sig
    type t

    val typ : t Ctypes.typ

    val delete : t -> unit

    val pk : t -> Proving_key.t

    val vk : t -> Verification_key.t

    val create : R1CS_constraint_system.t -> t
  end

  module Proof : sig
    type t

    type message

    val typ : t Ctypes.typ

    val create :
         ?message:message
      -> Proving_key.t
      -> primary:Field.Vector.t
      -> auxiliary:Field.Vector.t
      -> t

    val verify :
      ?message:message -> t -> Verification_key.t -> Field.Vector.t -> bool

    include Binable.S with type t := t
  end
end
