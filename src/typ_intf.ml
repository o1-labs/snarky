module type S' = sig
  type 'f field

  module Types : Types.Types

  module Data_spec : sig
    val size : ('a, 'b, 'c, 'd, 'f) Types.Data_spec.t -> int
  end

  val store :
       ('var, 'value, 'f field) Types.Typ.t
    -> 'value
    -> ('var, 'f field) Typ_monads.Store.t

  val read :
       ('var, 'value, 'f field) Types.Typ.t
    -> 'var
    -> ('value, 'f field) Typ_monads.Read.t

  val alloc :
    ('var, 'value, 'f field) Types.Typ.t -> ('var, 'f field) Typ_monads.Alloc.t

  val check :
       ('var, 'value, 'f field) Types.Typ.t
    -> 'var
    -> (unit, 's, 'f field) Types.Checked.t

  val unit : unit -> (unit, unit, 'f field) Types.Typ.t

  val field : unit -> ('f field Cvar.t, 'f field, 'f field) Types.Typ.t

  val transport :
       ('var1, 'value1, 'f field) Types.Typ.t
    -> there:('value2 -> 'value1)
    -> back:('value1 -> 'value2)
    -> ('var1, 'value2, 'f field) Types.Typ.t

  val transport_var :
       ('var1, 'value, 'f field) Types.Typ.t
    -> there:('var2 -> 'var1)
    -> back:('var1 -> 'var2)
    -> ('var2, 'value, 'f field) Types.Typ.t

  val list :
       length:int
    -> ('elt_var, 'elt_value, 'f field) Types.Typ.t
    -> ('elt_var list, 'elt_value list, 'f field) Types.Typ.t

  val array :
       length:int
    -> ('elt_var, 'elt_value, 'f field) Types.Typ.t
    -> ('elt_var array, 'elt_value array, 'f field) Types.Typ.t

  val tuple2 :
       ('var1, 'value1, 'f field) Types.Typ.t
    -> ('var2, 'value2, 'f field) Types.Typ.t
    -> ('var1 * 'var2, 'value1 * 'value2, 'f field) Types.Typ.t

  val ( * ) :
       ('var1, 'value1, 'f field) Types.Typ.t
    -> ('var2, 'value2, 'f field) Types.Typ.t
    -> ('var1 * 'var2, 'value1 * 'value2, 'f field) Types.Typ.t

  val tuple3 :
       ('var1, 'value1, 'f field) Types.Typ.t
    -> ('var2, 'value2, 'f field) Types.Typ.t
    -> ('var3, 'value3, 'f field) Types.Typ.t
    -> ( 'var1 * 'var2 * 'var3
       , 'value1 * 'value2 * 'value3
       , 'f field )
       Types.Typ.t

  val hlist :
       (unit, unit, 'vars, 'values, 'f field) Types.Data_spec.t
    -> ((unit, 'vars) H_list.t, (unit, 'values) H_list.t, 'f field) Types.Typ.t

  val of_hlistable :
       (unit, unit, 'k_var, 'k_value, 'a field) Types.Data_spec.t
    -> var_to_hlist:('var -> (unit, 'k_var) H_list.t)
    -> var_of_hlist:((unit, 'k_var) H_list.t -> 'var)
    -> value_to_hlist:('value -> (unit, 'k_value) H_list.t)
    -> value_of_hlist:((unit, 'k_value) H_list.t -> 'value)
    -> ('var, 'value, 'a field) Types.Typ.t
end

module type S = sig
  module Types : Types.Types

  module Data_spec : sig
    include module type of Types.Data_spec

    val size : ('a, 'b, 'c, 'd, 'f) Types.Data_spec.t -> int
  end

  include S' with module Types := Types and module Data_spec := Data_spec

  type ('var, 'value, 'field) t = ('var, 'value, 'field) Types.Typ.t
end
