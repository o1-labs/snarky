Module List.

  Fixpoint filter_map {A B} (f : A -> option B) (l : list A) :=
    match l with
    | nil => nil
    | cons a l =>
      match f a with
      | Some b => cons b (filter_map f l)
      | None => filter_map f l
      end
    end.

End List.