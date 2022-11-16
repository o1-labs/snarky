open Core_kernel

exception Runtime_error of string list * exn * string

let stack_to_string = String.concat ~sep:"\n"

(* Register a printer for [Runtime_error], so that the user sees a useful,
   well-formatted message. This will contain all of the information that
   raising the original error would have raised, along with the extra
   information added to [Runtime_error].

   NOTE: The message in its entirety is included in [Runtime_error], so that
         all of the information will be visible to the user in some form even
         if they don't use the [Print_exc] pretty-printer.
*)
let () =
  Stdlib.Printexc.register_printer (fun exn ->
      match exn with
      | Runtime_error (stack, exn, bt) ->
          Some
            (Printf.sprintf
               "Snarky.Checked_runner.Runtime_error(_, _, _, _)\n\n\
                Encountered an error while evaluating the checked computation:\n\
               \  %s\n\n\
                Label stack trace:\n\
                %s\n\n\n\
                %s"
               (Exn.to_string exn) (stack_to_string stack) bt )
      | _ ->
          None )

module Make_runner
    (Checked : Checked_intf.Basic
                 with type ('a, 'f) Types.As_prover.t =
                   ('a, 'f) Types.As_prover.t
                  and type ('a, 'f) Types.Provider.provider =
                   ('a, 'f) Types.Provider.provider) =
struct
  let handle_error label f =
    try f () with
    | Runtime_error (stack, exn, bt) ->
        let bt_new = Printexc.get_backtrace () in
        raise (Runtime_error (label :: stack, exn, bt ^ "\n\n" ^ bt_new))
    | exn ->
        let bt = Printexc.get_backtrace () in
        raise (Runtime_error ([ label ], exn, bt))

  let rec run_ast :
      type a.
      (a, 'f Checked.field) Checked_ast.t -> (a, 'f Checked.field) Checked.t =
   fun t ->
    match t with
    | As_prover (x, k) ->
        Checked.bind (Checked.as_prover x) ~f:(fun () -> run_ast k)
    | Pure x ->
        Checked.return x
    | Direct (d, k) ->
        Checked.bind (Checked.direct d) ~f:(fun y -> run_ast (k y))
    | Lazy (x, k) ->
        Checked.bind
          (Checked.mk_lazy (fun () -> run_ast x))
          ~f:(fun y -> run_ast (k y))
    | With_label (lab, t, k) ->
        Checked.bind
          (Checked.with_label lab (fun () ->
               handle_error lab (fun () -> run_ast t) ) )
          ~f:(fun y -> run_ast (k y))
    | Add_constraint (c, t) ->
        Checked.bind (Checked.add_constraint c) ~f:(fun () -> run_ast t)
    | With_handler (h, t, k) ->
        Checked.bind
          (Checked.with_handler h (fun () -> run_ast t))
          ~f:(fun y -> run_ast (k y))
    | Exists
        ( Typ
            { var_to_fields
            ; var_of_fields
            ; value_to_fields
            ; value_of_fields
            ; size_in_field_elements
            ; constraint_system_auxiliary
            ; check
            }
        , p
        , k ) ->
        let typ =
          Types.Typ.Typ
            { var_to_fields
            ; var_of_fields
            ; value_to_fields
            ; value_of_fields
            ; size_in_field_elements
            ; constraint_system_auxiliary
            ; check = (fun var -> run_ast (check var))
            }
        in
        Checked.bind (Checked.exists typ p) ~f:(fun y -> run_ast (k y))
    | Next_auxiliary k ->
        Checked.bind (Checked.next_auxiliary ()) ~f:(fun y -> run_ast (k y))
end
