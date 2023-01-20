open Core_kernel
open Types.Typ

module Data_spec0 = struct
  (** A list of {!type:Type.Typ.t} values, describing the inputs to a checked
      computation. The type [('r_var, 'r_value, 'k_var, 'k_value, 'field) t]
      represents
      - ['k_value] is the OCaml type of the computation
      - ['r_value] is the OCaml type of the result
      - ['k_var] is the type of the computation within the R1CS
      - ['k_value] is the type of the result within the R1CS
      - ['field] is the field over which the R1CS operates
      - ['checked] is the type of checked computation that verifies the stored
        contents as R1CS variables.

      This functions the same as OCaml's default list type:
{[
  Data_spec.[typ1; typ2; typ3]

  Data_spec.(typ1 :: typs)

  let open Data_spec in
  [typ1; typ2; typ3; typ4; typ5]

  let open Data_spec in
  typ1 :: typ2 :: typs

]}
      all function as you would expect.
  *)
  type ('r_var, 'r_value, 'k_var, 'k_value, 'f, 'checked) data_spec =
    | ( :: ) :
        ('var, 'value, 'f, 'checked) Types.Typ.t
        * ('r_var, 'r_value, 'k_var, 'k_value, 'f, 'checked) data_spec
        -> ( 'r_var
           , 'r_value
           , 'var -> 'k_var
           , 'value -> 'k_value
           , 'f
           , 'checked )
           data_spec
    | [] : ('r_var, 'r_value, 'r_var, 'r_value, 'f, 'checked) data_spec
end

module type Checked_monad = sig
  type ('a, 'f) t

  type 'f field

  include Monad_let.S2 with type ('a, 'e) t := ('a, 'e) t

  module Types : Types.Types
end

module Make (Checked : Checked_monad) = struct
  type ('var, 'value, 'field) t =
    ('var, 'value, 'field, (unit, 'field) Checked.t) Types.Typ.t

  type ('var, 'value, 'field) typ = ('var, 'value, 'field) t

  module type S = sig
    type field

    module Var : sig
      type t

      val size_in_field_elements : int

      val to_field_elements : t -> field Cvar.t array

      val of_field_elements : field Cvar.t array -> t

      val check : t -> (unit, field) Checked.t
    end

    module Value : sig
      type t

      val size_in_field_elements : int

      val to_field_elements : t -> field array

      val of_field_elements : field array -> t
    end
  end

  module Data_spec = struct
    include Data_spec0

    type ('r_var, 'r_value, 'k_var, 'k_value, 'f) t =
      ('r_var, 'r_value, 'k_var, 'k_value, 'f, (unit, 'f) Checked.t) data_spec

    let size t =
      let rec go :
          type r_var r_value k_var k_value.
          int -> (r_var, r_value, k_var, k_value, 'f) t -> int =
       fun acc t ->
        match t with
        | [] ->
            acc
        | Typ { size_in_field_elements; _ } :: t' ->
            go (acc + size_in_field_elements) t'
      in
      go 0 t
  end

  module T = struct
    let unit () : (unit, unit, 'field) t =
      Typ
        { var_to_fields = (fun () -> ([||], ()))
        ; var_of_fields = (fun _ -> ())
        ; value_to_fields = (fun () -> ([||], ()))
        ; value_of_fields = (fun _ -> ())
        ; size_in_field_elements = 0
        ; constraint_system_auxiliary = (fun () -> ())
        ; check = (fun () -> Checked.return ())
        }

    let field () : ('field Cvar.t, 'field, 'field) t =
      Typ
        { var_to_fields = (fun f -> ([| f |], ()))
        ; var_of_fields = (fun (fields, _) -> fields.(0))
        ; value_to_fields = (fun f -> ([| f |], ()))
        ; value_of_fields = (fun (fields, _) -> fields.(0))
        ; size_in_field_elements = 1
        ; constraint_system_auxiliary = (fun () -> ())
        ; check = (fun _ -> Checked.return ())
        }

    module Internal = struct
      let snarkless value : _ t =
        Typ
          { var_to_fields = (fun _ -> ([||], ()))
          ; var_of_fields = (fun _ -> value)
          ; value_to_fields =
              (fun value' ->
                assert (phys_equal value value') ;
                ([||], ()) )
          ; value_of_fields = (fun _ -> value)
          ; size_in_field_elements = 0
          ; constraint_system_auxiliary = (fun () -> ())
          ; check = (fun _ -> Checked.return ())
          }

      module Ref_typ = As_prover_ref.Make_ref_typ (Checked)

      let ref () = Ref_typ.typ
    end

    let transport (type var value1 value2 field)
        (Typ
           { var_to_fields
           ; var_of_fields
           ; value_to_fields
           ; value_of_fields
           ; size_in_field_elements
           ; constraint_system_auxiliary
           ; check
           } :
          (var, value1, field) t ) ~(there : value2 -> value1)
        ~(back : value1 -> value2) : (var, value2, field) t =
      Typ
        { var_to_fields
        ; var_of_fields
        ; value_to_fields = (fun x -> value_to_fields (there x))
        ; value_of_fields = (fun x -> back (value_of_fields x))
        ; size_in_field_elements
        ; constraint_system_auxiliary
        ; check
        }

    let transport_var (type var1 var2 value field)
        (Typ
           { var_to_fields
           ; var_of_fields
           ; value_to_fields
           ; value_of_fields
           ; size_in_field_elements
           ; constraint_system_auxiliary
           ; check
           } :
          (var1, value, field) t ) ~(there : var2 -> var1) ~(back : var1 -> var2)
        : (var2, value, field) t =
      Typ
        { var_to_fields = (fun x -> var_to_fields (there x))
        ; var_of_fields = (fun x -> back (var_of_fields x))
        ; value_to_fields
        ; value_of_fields
        ; size_in_field_elements
        ; constraint_system_auxiliary
        ; check = (fun x -> check (there x))
        }

    let list ~length
        (Typ
           { var_to_fields
           ; var_of_fields
           ; value_to_fields
           ; value_of_fields
           ; size_in_field_elements
           ; constraint_system_auxiliary
           ; check
           } :
          ('elt_var, 'elt_value, 'field) t ) :
        ('elt_var list, 'elt_value list, 'field) t =
      (* NB: We store the size_in_field_elements of each in the auxiliary
         data, to allow for 'reads' of e.g. list of lists of different
         lengths.
      *)
      Typ
        { var_to_fields =
            (fun ts ->
              let rec go ts ((fieldss, auxes) as acc) =
                match ts with
                | [] ->
                    acc
                | t :: tl ->
                    let fields, aux = var_to_fields t in
                    let acc =
                      ( Array.append fieldss fields
                      , (aux, Array.length fields) :: auxes )
                    in
                    go tl acc
              in
              go ts ([||], []) )
        ; var_of_fields =
            (fun (fields, auxes) ->
              let vars, _ =
                List.fold
                  ~init:([], Array.length fields)
                  auxes
                  ~f:(fun (vars, end_pos) (aux, num_fields) ->
                    let end_pos = end_pos - num_fields in
                    let var =
                      var_of_fields
                        (Array.sub ~pos:end_pos ~len:num_fields fields, aux)
                    in
                    (var :: vars, end_pos) )
              in
              vars )
        ; value_to_fields =
            (fun ts ->
              let rec go ts ((fieldss, auxes) as acc) =
                match ts with
                | [] ->
                    acc
                | t :: tl ->
                    let fields, aux = value_to_fields t in
                    let acc =
                      ( Array.append fieldss fields
                      , (aux, Array.length fields) :: auxes )
                    in
                    go tl acc
              in
              go ts ([||], []) )
        ; value_of_fields =
            (fun (fields, auxes) ->
              let vars, _ =
                List.fold
                  ~init:([], Array.length fields)
                  auxes
                  ~f:(fun (vars, end_pos) (aux, num_fields) ->
                    let end_pos = end_pos - num_fields in
                    let var =
                      value_of_fields
                        (Array.sub ~pos:end_pos ~len:num_fields fields, aux)
                    in
                    (var :: vars, end_pos) )
              in
              vars )
        ; size_in_field_elements = length * size_in_field_elements
        ; constraint_system_auxiliary =
            (fun () ->
              List.init length ~f:(fun _ ->
                  (constraint_system_auxiliary (), size_in_field_elements) ) )
        ; check = (fun ts -> Checked.all_unit (List.map ts ~f:check))
        }

    let array ~length typ =
      list ~length typ
      |> transport ~there:Array.to_list ~back:Array.of_list
      |> transport_var ~there:Array.to_list ~back:Array.of_list

    let hlist (type k_var k_value)
        (spec0 : (unit, unit, k_var, k_value, 'f) Data_spec.t) :
        ((unit, k_var) H_list.t, (unit, k_value) H_list.t, 'f) t =
      let rec go :
          type k_var k_value.
             (unit, unit, k_var, k_value, 'f) Data_spec.t
          -> ((unit, k_var) H_list.t, (unit, k_value) H_list.t, 'f) t =
       fun spec0 ->
        let open H_list in
        match spec0 with
        | [] ->
            Typ
              { var_to_fields = (fun [] -> ([||], ()))
              ; var_of_fields = (fun _ -> [])
              ; value_to_fields = (fun [] -> ([||], ()))
              ; value_of_fields = (fun _ -> [])
              ; size_in_field_elements = 0
              ; constraint_system_auxiliary = (fun () -> ())
              ; check = (fun [] -> Checked.return ())
              }
        | Typ
            { var_to_fields
            ; var_of_fields
            ; value_to_fields
            ; value_of_fields
            ; size_in_field_elements
            ; constraint_system_auxiliary
            ; check
            }
          :: spec0 ->
            let (Typ typ) = go spec0 in
            let open H_list in
            Typ
              { var_to_fields =
                  (fun (x :: tl) ->
                    let fields, aux = var_to_fields x in
                    let fieldss, auxes = typ.var_to_fields tl in
                    ( Array.append fields fieldss
                    , (aux, Array.length fields, auxes) ) )
              ; var_of_fields =
                  (fun (fields, (hd, len, tl)) ->
                    let var =
                      var_of_fields (Array.sub ~pos:0 ~len fields, hd)
                    in
                    let tl =
                      typ.var_of_fields
                        ( Array.sub ~pos:len
                            ~len:(Array.length fields - len)
                            fields
                        , tl )
                    in
                    var :: tl )
              ; value_to_fields =
                  (fun (x :: tl) ->
                    let fields, aux = value_to_fields x in
                    let fieldss, auxes = typ.value_to_fields tl in
                    ( Array.append fields fieldss
                    , (aux, Array.length fields, auxes) ) )
              ; value_of_fields =
                  (fun (fields, (hd, len, tl)) ->
                    let value =
                      value_of_fields (Array.sub ~pos:0 ~len fields, hd)
                    in
                    let tl =
                      typ.value_of_fields
                        ( Array.sub ~pos:len
                            ~len:(Array.length fields - len)
                            fields
                        , tl )
                    in
                    value :: tl )
              ; size_in_field_elements =
                  size_in_field_elements + typ.size_in_field_elements
              ; constraint_system_auxiliary =
                  (fun () ->
                    let hd = constraint_system_auxiliary () in
                    let auxes = typ.constraint_system_auxiliary () in
                    (hd, size_in_field_elements, auxes) )
              ; check =
                  (fun (x :: tl) ->
                    Checked.bind (check x) ~f:(fun () -> typ.check tl) )
              }
      in
      go spec0

    let tuple2 typ1 typ2 =
      let open H_list in
      hlist [ typ1; typ2 ]
      |> transport
           ~there:(fun (a, b) -> [ a; b ])
           ~back:(fun ([ a; b ] : (unit, _ -> _ -> unit) H_list.t) -> (a, b))
      |> transport_var
           ~there:(fun (a, b) -> [ a; b ])
           ~back:(fun ([ a; b ] : (unit, _ -> _ -> unit) H_list.t) -> (a, b))

    let ( * ) = tuple2

    let tuple3 typ1 typ2 typ3 =
      let open H_list in
      hlist [ typ1; typ2; typ3 ]
      |> transport
           ~there:(fun (a, b, c) -> [ a; b; c ])
           ~back:(fun ([ a; b; c ] : (unit, _ -> _ -> _ -> unit) H_list.t) ->
             (a, b, c) )
      |> transport_var
           ~there:(fun (a, b, c) -> [ a; b; c ])
           ~back:(fun ([ a; b; c ] : (unit, _ -> _ -> _ -> unit) H_list.t) ->
             (a, b, c) )

    let tuple4 typ1 typ2 typ3 typ4 =
      let open H_list in
      hlist [ typ1; typ2; typ3; typ4 ]
      |> transport
           ~there:(fun (a, b, c, d) -> [ a; b; c; d ])
           ~back:(fun ([ a; b; c; d ] :
                        (unit, _ -> _ -> _ -> _ -> unit) H_list.t ) ->
             (a, b, c, d) )
      |> transport_var
           ~there:(fun (a, b, c, d) -> [ a; b; c; d ])
           ~back:(fun ([ a; b; c; d ] :
                        (unit, _ -> _ -> _ -> _ -> unit) H_list.t ) ->
             (a, b, c, d) )

    let tuple5 typ1 typ2 typ3 typ4 typ5 =
      let open H_list in
      hlist [ typ1; typ2; typ3; typ4; typ5 ]
      |> transport
           ~there:(fun (a, b, c, d, e) -> [ a; b; c; d; e ])
           ~back:(fun ([ a; b; c; d; e ] :
                        (unit, _ -> _ -> _ -> _ -> _ -> unit) H_list.t ) ->
             (a, b, c, d, e) )
      |> transport_var
           ~there:(fun (a, b, c, d, e) -> [ a; b; c; d; e ])
           ~back:(fun ([ a; b; c; d; e ] :
                        (unit, _ -> _ -> _ -> _ -> _ -> unit) H_list.t ) ->
             (a, b, c, d, e) )

    let tuple6 typ1 typ2 typ3 typ4 typ5 typ6 =
      let open H_list in
      hlist [ typ1; typ2; typ3; typ4; typ5; typ6 ]
      |> transport
           ~there:(fun (a, b, c, d, e, f) -> [ a; b; c; d; e; f ])
           ~back:(fun ([ a; b; c; d; e; f ] :
                        (unit, _ -> _ -> _ -> _ -> _ -> _ -> unit) H_list.t ) ->
             (a, b, c, d, e, f) )
      |> transport_var
           ~there:(fun (a, b, c, d, e, f) -> [ a; b; c; d; e; f ])
           ~back:(fun ([ a; b; c; d; e; f ] :
                        (unit, _ -> _ -> _ -> _ -> _ -> _ -> unit) H_list.t ) ->
             (a, b, c, d, e, f) )

    let of_hlistable (spec : (unit, unit, 'k_var, 'k_value, 'f) Data_spec.t)
        ~(var_to_hlist : 'var -> (unit, 'k_var) H_list.t)
        ~(var_of_hlist : (unit, 'k_var) H_list.t -> 'var)
        ~(value_to_hlist : 'value -> (unit, 'k_value) H_list.t)
        ~(value_of_hlist : (unit, 'k_value) H_list.t -> 'value) :
        ('var, 'value, 'f) t =
      hlist spec
      |> transport ~there:value_to_hlist ~back:value_of_hlist
      |> transport_var ~there:var_to_hlist ~back:var_of_hlist
  end

  include T
end
