Require Snarky.types.
Require Snarky.as_prover.
Require Snarky.request.

Include Snarky.types.Provider.

Definition run {F V S request result} (t : t F V S request result) tbl s
  (handler : request.Handler.t request) :=
  match t with
  | Request rc =>
    let (s', r) := as_prover.run rc tbl s in
    (s', request.Handler.run handler r)
  | Compute c =>
    let (s', r) := as_prover.run c tbl s in
    (s', Some r)
  | Both rc c =>
    let (s', r) := as_prover.run rc tbl s in
    match request.Handler.run handler r with
    | Some x => (s', Some x)
    | None =>
      let (s', r) := as_prover.run c tbl s in
      (s', Some r)
    end
  end.