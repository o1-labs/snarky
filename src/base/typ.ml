open Core_kernel

module Intf = struct
  module type S = sig
    type field

    type field_var

    type 'field checked

    module Var : sig
      type t

      val size_in_field_elements : int

      val to_field_elements : t -> field_var array

      val of_field_elements : field_var array -> t

      val check : t -> field checked
    end

    module Value : sig
      type t

      val size_in_field_elements : int

      val to_field_elements : t -> field array

      val of_field_elements : field array -> t
    end
  end
end

module type Checked_monad = sig
  module Types : Types.Types

  type 'a t = 'a Types.Checked.t

  include Snarky_monad_lib.Monad_let.S with type 'a t := 'a t
end

module Make
    (Types : Types.Types)
    (Checked : Checked_monad with module Types := Types) =
struct
  type ('var, 'value) t = ('var, 'value) Types.Typ.t

  type ('var, 'value) typ = ('var, 'value) t

  module type S = sig
    type field

    include
      Intf.S
        with type 'field checked := unit Checked.t
         and type field := field
         and type field_var := field Cvar.t
  end

  module T = struct
    module Data_spec = struct
      type ('r_var, 'r_value, 'k_var, 'k_value) t =
        | ( :: ) :
            ('var, 'value) typ * ('r_var, 'r_value, 'k_var, 'k_value) t
            -> ('r_var, 'r_value, 'var -> 'k_var, 'value -> 'k_value) t
        | [] : ('r_var, 'r_value, 'r_var, 'r_value) t
    end

    let unit () : (unit, unit) t =
      Typ
        { var_to_fields = (fun () -> ([||], ()))
        ; var_of_fields = (fun _ -> ())
        ; value_to_fields = (fun () -> ([||], ()))
        ; value_of_fields = (fun _ -> ())
        ; size_in_field_elements = 0
        ; constraint_system_auxiliary = (fun () -> ())
        ; check = (fun () -> Checked.return ())
        }

    let field () : ('field_var, 'field) t =
      Typ
        { var_to_fields = (fun f -> ([| f |], ()))
        ; var_of_fields = (fun (fields, _) -> fields.(0))
        ; value_to_fields = (fun f -> ([| f |], ()))
        ; value_of_fields = (fun (fields, _) -> fields.(0))
        ; size_in_field_elements = 1
        ; constraint_system_auxiliary = (fun () -> ())
        ; check = (fun _ -> Checked.return ())
        }

    include struct
      type 'a prover_value = 'a option

      let prover_value () : _ t =
        Typ
          { var_to_fields = (fun x -> ([||], x))
          ; var_of_fields = (fun (_, x) -> x)
          ; value_to_fields = (fun x -> ([||], Some x))
          ; value_of_fields = (fun (_, x) -> Option.value_exn x)
          ; size_in_field_elements = 0
          ; constraint_system_auxiliary = (fun () -> None)
          ; check = (fun _ -> Checked.return ())
          }
    end

    let transport (type var value1 value2)
        (Typ
           { var_to_fields
           ; var_of_fields
           ; value_to_fields
           ; value_of_fields
           ; size_in_field_elements
           ; constraint_system_auxiliary
           ; check
           } :
          (var, value1) t ) ~(there : value2 -> value1)
        ~(back : value1 -> value2) : (var, value2) t =
      Typ
        { var_to_fields
        ; var_of_fields
        ; value_to_fields = (fun x -> value_to_fields (there x))
        ; value_of_fields = (fun x -> back (value_of_fields x))
        ; size_in_field_elements
        ; constraint_system_auxiliary
        ; check
        }

    let transport_var (type var1 var2 value)
        (Typ
           { var_to_fields
           ; var_of_fields
           ; value_to_fields
           ; value_of_fields
           ; size_in_field_elements
           ; constraint_system_auxiliary
           ; check
           } :
          (var1, value) t ) ~(there : var2 -> var1) ~(back : var1 -> var2) :
        (var2, value) t =
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
          ('elt_var, 'elt_value) t ) : ('elt_var list, 'elt_value list) t =
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
        (spec0 : (unit, unit, k_var, k_value) Data_spec.t) :
        ((unit, k_var) H_list.t, (unit, k_value) H_list.t) t =
      let rec go :
          type k_var k_value.
             (unit, unit, k_var, k_value) Data_spec.t
          -> ((unit, k_var) H_list.t, (unit, k_value) H_list.t) t =
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

    let of_hlistable (spec : (unit, unit, 'k_var, 'k_value) Data_spec.t)
        ~(var_to_hlist : 'var -> (unit, 'k_var) H_list.t)
        ~(var_of_hlist : (unit, 'k_var) H_list.t -> 'var)
        ~(value_to_hlist : 'value -> (unit, 'k_value) H_list.t)
        ~(value_of_hlist : (unit, 'k_value) H_list.t -> 'value) :
        ('var, 'value) t =
      hlist spec
      |> transport ~there:value_to_hlist ~back:value_of_hlist
      |> transport_var ~there:var_to_hlist ~back:var_of_hlist
  end

  include T
end
