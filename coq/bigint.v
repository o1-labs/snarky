Module T.
  Section T.
    Class Of_field t F :={of_field : F -> t}.
    Arguments of_field {t} {F}.

    Class Test_bit t :={test_bit : t -> nat -> bool}.
    Arguments test_bit {t}.
  End T.

End T.

Include T.

Module Class.
  Class Bigint t F :={bigint_of_field :> Of_field t F;
                    bigint_test_bit :> Test_bit t}.
End Class.
Include Class.

Module Extended.
  Include T.

  Section Extended.
    Context {t F : Type}.

    Class To_field :={to_field : t -> F}.

  End Extended.
End Extended.