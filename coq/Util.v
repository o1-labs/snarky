Module List.

  Fixpoint filter_map {A B} (f : A -> option B) (l : list A) :=
  match l with
  | nil => nil
  | (a :: l)%list =>
      match f a with
      | Some b => (b :: filter_map f l)%list
      | None => filter_map f l
      end
  end.

End List.