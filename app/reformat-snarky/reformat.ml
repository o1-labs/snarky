open Core
open Async

(* If OCamlformat ever breaks on any files add their paths here *)
let whitelist = []

let rec fold_over_files ~path ~process_path ~init ~f =
  let%bind all = Sys.ls_dir path in
  Deferred.List.fold all ~init ~f:(fun acc x ->
      match%bind Sys.is_directory (path ^/ x) with
      | `Yes when process_path `Dir (path ^/ x) ->
          fold_over_files ~path:(path ^/ x) ~process_path ~init:acc ~f
      | `Yes -> return acc
      | _ when process_path `File (path ^/ x) -> f acc (path ^/ x)
      | _ -> return acc )

let main dry_run check path =
  let%bind all =
    fold_over_files ~path ~init:()
      ~process_path:(fun kind path ->
        match kind with
        | `Dir ->
            (not (String.is_suffix ~suffix:".git" path))
            && (not (String.is_suffix ~suffix:"_build" path))
            && (not (String.is_suffix ~suffix:"stationary" path))
            && (not (String.is_suffix ~suffix:".un~" path))
            && (not (String.is_suffix ~suffix:"external" path))
            && (not (String.is_suffix ~suffix:"ocamlformat" path))
            && not (String.is_suffix ~suffix:"node_modules" path)
        | `File ->
            (not
               (List.exists whitelist ~f:(fun s ->
                    String.is_suffix ~suffix:s path )))
            && ( String.is_suffix ~suffix:".ml" path
               || String.is_suffix ~suffix:".mli" path ) )
      ~f:(fun () file ->
        let dump prog args =
          printf !"%s %{sexp: string List.t}\n" prog args ;
          return ()
        in
        if check then
          let prog, args = ("ocamlformat", [file]) in
          let%bind formatted = Process.run_exn ~prog ~args () in
          let%bind raw = Reader.file_contents file in
          if formatted <> raw then (
            eprintf "File: %s has needs to be ocamlformat-ed\n" file ;
            exit 1 )
          else return ()
        else
          let prog, args = ("ocamlformat", ["-i"; file]) in
          if dry_run then dump prog args
          else
            let%map _stdout = Process.run_exn ~prog ~args () in
            () )
  in
  exit 0

let cli =
  let open Command.Let_syntax in
  Command.async ~summary:"Format all ml and mli files"
    (let%map_open path = flag "path" ~doc:"Path to traverse" (required file)
     and dry_run = flag "dry-run" no_arg ~doc:"Dry run"
     and check =
       flag "check" no_arg
         ~doc:
           "Return with an error code if there exists an ml file that was \
            formatted improperly"
     in
     fun () -> main dry_run check path)
  |> Command.run

let () = never_returns (Scheduler.go ())
