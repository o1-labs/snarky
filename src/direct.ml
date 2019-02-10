module Field_intf = struct
  module type S = sig
    type t
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val one : t
    val zero : t
    val of_bigint : Bigint.t -> t
  end

  module type As_prover = sig
    include S
    val to_bigint : t -> Bigint.t
  end
end

module type S = sig
  module Field : sig 
    include Field_intf.S
    module As_prover : Field_intf.As_prover
  end


  val exists
      : 
        ?request:('value Request.t, ( Field.t -> Field.As_prover.t), 's) As_prover.t
      -> ?compute:('value, (Field.t -> Field.As_prover.t), 's) As_prover.t
      -> ('var, 'value, Field.As_prover.t, Field.t, unit) Typ.t -> 'var

  val assert_ : Field.t Constraint.t -> unit 
end

type 'f m = (module S with type Field.t = 'f)

let div (type f) ((module I) : f m) x y =
  let open I in
  let z =
    exists (Typ.field ())
      ~compute:As_prover.(Let_syntax.(
        let%map x = read (Typ.field ()) x and y = read (Typ.field ()) y in
        Field.As_prover.(x / y)))
  in
  assert_ (Constraint.r1cs z y x);
  z
