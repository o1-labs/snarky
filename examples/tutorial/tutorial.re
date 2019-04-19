open Core;
open Snarky;

/* Welcome!

      Snarky is a library for constructing R1CS SNARKs.

      TODO: Explanation of R1CSs, how it makes addition and scalar mult 'free' but
      multiplication of variables costs 1.
   */
/* 0. First we instantiate Snarky with a 'backend' */
module Impl = Snark.Run.Make(Backends.Bn128.Default);
open Impl;

/* The point of snarky is to describe "checked computations"
      which are computations that may "request" values from their environment,
      and make assertions about values that arise in the computation.

      You "run" snarky programs in two ways:
      1. To generate a constraint system for the SNARK
      2. To generate proofs.

      We'll see exactly how this works later.

      First let's understand "field var"s which are the main primitive type we
      have access to in snarky.
   */
/* A [Field.Constant.t] represents an element of the finite field of order [Field.size]
      It is a prime order field so you can think about it as "integers mod Field.size".
   */

let () = {
  let x = Field.Constant.of_int(23);
  let x_cubed = Field.Constant.mul(x, Field.Constant.square(x));
  let z = Field.Constant.(x_cubed / x);
  assert(Field.Constant.equal(z, Field.Constant.square(x)));
};

/* Try seeing what operations there are in the [Field] module by using
      your editor's auto-completion feature
   */
/* Inside snarky we also work with "Field.t"s. These are sort of like
      Field.Constant.t's but we can make assertions about them.
   */
/* [Field.Assert.equal : Field.t -> Field.t -> unit] lets us
      make an assertion that two field elements are equal.

      Here we assert that [x] is a square root of 9.
   */
let assert_is_square_root_of_9 = (x: Field.t):unit => {
  let x_squared = Field.(x * x);
  Field.Assert.equal(
    x_squared,
    Field.of_int(9)
  );
};

/* Exercise 1:
      Write a function
      [assert_is_cube_root_of_1 : Field.t -> unit]
      that asserts its argument is a cube root of 1.

      Aside:
      In finite fields there may be either 1 or 3 cube roots of 1.
      This is because

      x^3 - 1 = (x - 1)(x^2 + x + 1)

      so if [x^2 + x + 1] has a root in the field, then we will get
      another cube root of 1. By quadratic formula,

      x^2 + x + 1 = 0 iff
      x = ( -1 +/- sqrt (1 - 4) ) / 2

      so if sqrt(1 - 4) = sqrt(-3) exists in the field then there will
      be two additional cube roots of 1.
   */
/* In this field, it happens to be the case that -3 is a square. */
let () = assert(Field.Constant.is_square(Field.Constant.of_int(-3)));

let assert_is_cube_root_of_1 = (x: Field.t) => failwith("Exercise 1");

let cube_root_of_1 = Field.Constant.((of_int(-1) + sqrt(of_int(-3))) / of_int(2));

let exercise1 = () => {
  /* Before we generate a constraint system or a proof for our checked
       computation we must first specify the "data spec" of the input.

       This is actually an HList which we can represent in OCaml by overriding the
       list type constructors. We also need to make input a function over unit due
       to value restriction reasons.

       Here our function `assert_is_cube_root_of_1` takes a single Field.var as
       input. The type of that var is `Field.typ`.
     */
  let input = () => Data_spec.[Field.typ];
  /* Now we generate a keypair that we can use produce and verify proofs */
  let keypair = generate_keypair(~exposing=input(), assert_is_cube_root_of_1);

  /* Now we prove: Here is an input to `assert_is_cube_root_of_1` such that the
       checked computation terminates without failing any assertions. In other
       words, there exists some cube_root_of_1.
     */
  let proof =
    prove(
      Keypair.pk(keypair),
      input(),
      assert_is_cube_root_of_1,
      cube_root_of_1,
    );

  /* We can verify a proof as follows */
  let is_valid = verify(proof, Keypair.vk(keypair), input(), cube_root_of_1);
  printf(
    "is %{sexp:Field.Constant.t} a cube root of 1? %b\n%!"^,
    cube_root_of_1,
    is_valid,
  );
};

/* Exercise 1: Comment this out when you're ready to test it! */
/* let () = exercise1 () */

let exercise2 = () => {
  /* Now let's prove that there are two cube roots of 1. */
  let distinct_cube_roots_of_1 = (x, y) => {
    assert_is_cube_root_of_1(x);
    assert_is_cube_root_of_1(y);
    Field.Assert.not_equal(x, y);
  };

  /* Exercise 2:
       Now you try: Creating a data spec, keypair, proof, and verifying that proof
       for `distinct_cube_roots_of_1`.
     */
  let another_cube_root_of_1 = failwith("x^3 = 1, find x");
  let input = () => failwith("Exercise 2: Data_spec here");
  let keypair = failwith("Exercise 2: Keypair here");
  let proof = failwith("Exercise 2: Proof");
  let is_valid = failwith("Exercise 2: Verify");
  printf(
    "Are %{sexp:Field.Constant.t} and %{sexp:Field.Constant.t} two distinct cube roots of 1? %b\n%!"
      ^,
    cube_root_of_1,
    another_cube_root_of_1,
    is_valid,
  );
};

/* Exercise 2: Comment this out when you're ready to test it! */
/* let () = exercise2 () */

/* We can encode other data types in terms of the underlying fields. An
      extremely useful one is boolean values -- true and false.

      Within our field, a Boolean is a Field element that is either zero or one.
      With this simple concept, we can start writing checked programs that make
      decisions!

      For example, `ifeqxy_x_else_z` checks if x and y are equal and if so
      returns x. If not, we return z.

      This is also an example of a Checked computation that doesn't return unit!
   */
let ifeqxy_x_else_z = (x, y, z) => {
  let b = Field.equal(x, y);
  Field.if_(b, ~then_=x, ~else_=z);
};

/* We can combine booleans in the usual ways: `a && b` for 'and', `a || b`
      for 'or'.
   */
let if_both = (x, y, a, b) => {
  let x_and_y = Boolean.(x && y);
  Field.if_(x_and_y, ~then_=a, ~else_=b);
};

/* Exercise 3:
      Write a function
      [zero_or_inverse : Field.var -> (Field.var, _) Checked.t]
      that returns zero if the input is zero, or the inverse of the input
      otherwise.
   */
let zero_or_inverse = (x: Field.t) => failwith("Exercise 3");

let exercise3 = () => {
  /* Unchecked reference implementation. */
  let zero_or_inverse_unchecked = x => {
    let b = Field.Constant.equal(x, Field.Constant.zero);
    let invertable = if (b) {Field.Constant.one} else {x};
    if (b) {
      x;
    } else {
      Field.Constant.inv(invertable);
    };
  };

  /* Check the value matches [expected_value]. */
  let matches_unchecked = (x, expected_value) => {
    let y = zero_or_inverse(x);
    Field.Assert.equal(y, expected_value);
  };

  let input = () => failwith("Exercise 3: Data_spec here");
  let keypair = failwith("Exercise 3: Keypair here");
  let proof = x =>
    prove(
      Keypair.pk(keypair),
      input(),
      (),
      matches_unchecked,
      x,
      zero_or_inverse_unchecked(x),
    );

  let proof_0 = proof(Field.Constant.zero);
  let proof_1 = proof(Field.Constant.one);
  let proof_15 = proof(Field.Constant.of_int(15));
  let is_valid_0 = failwith("Exercise 3: Verify proof_0");
  let is_valid_1 = failwith("Exercise 3: Verify proof_1");
  let is_valid_15 = failwith("Exercise 3: Verify proof_15");
  printf(
    "Matched expected output for\n0? %b\n1? %b\n15? %b\n",
    is_valid_0,
    is_valid_1,
    is_valid_15,
  );
};

/* Exercise 3: Comment this out when you're ready to test it! */
/* let () = exercise3 () */

/* So far, the only data we have passed to [prove] have been Field elements.
      To pass other kinds of data, we will need to change the values that we pass
      as the [Data_spec].

      The values in our [Data_spec] list are [Typ.t]s, which tell [prove] how to
      convert our normal data into [Field.Var.t]s.

      For example, [Boolean.typ] has type [(Boolean.var, bool) Typ.t]; this tells
      us that we can pass a [bool] value to [prove] and it will turn it into a
      [Boolean.var].

      Exercise 4:
      Create a Data_spec for [either].
   */
let exercise4 = () => {
  let either = (x, y) => {
    let z = Boolean.(x && y);
    Boolean.Assert.is_true(z);
  };

  let input = () => failwith("Exercise 4: Data_spec here");
  let keypair = failwith("Exercise 4: Keypair here");
  let proof = prove(Keypair.pk(keypair), input(), (), either, true, true);
  let is_valid = (proof, x, y) =>
    verify(proof, Keypair.vk(keypair), input(), x, y);
  let proved = (x, y) => is_valid(proof, x, y);
  printf(
    "Proved that:\n true && true is true? %b\n true && false is true? %b\n",
    proved(true, true),
    proved(true, false),
  );
};

/* Exercise 4: Comment this out when you're ready to test it! */
/* let () = exercise4 () */

/* We can encode richer data types in the same way; as long as we can tell
      [prove] how to turn our OCaml value into field elements with a [Typ.t], we
      can pass any value we like!

      The [Typ] module has some useful functions for building new [Typ.t]s out of
      [Typ.t]s we already have.

      Exercise 5:
      Fill in [product] below, and use [Typ.list] to create a proof from
      [product_equals] that it gives the correct value.

      Hint: the functions in [Checked.List] are useful for working with checked
      functions over lists!
   */

let product = (l: list(Field.t)): Field.t =>
  failwith("Exercise 5");

let product_equals = (l: list(Field.t), expected_total: Field.t) => {
  let total = product(l);
  Field.Assert.equal(total, expected_total);
};

let product_unchecked = (l: list(Field.t)) =>
  List.fold(~init=Field.one, ~f=Field.mul, l);

let exercise5 = () => {
  let input = () => failwith("Exercise 5: Data_spec here");
  let keypair = failwith("Exercise 5: Keypair here");
  let proof = (l, expected_total) => failwith("Exercise 5: Proof");
  let is_valid = (proof, l, expected_total) =>
    failwith("Exercise 5: Verify");
  let proved = (l: list(int)) => {
    let l: list(Field.t) = (List.map(~f=Field.of_int, l): list(Field.t));
    let expected_total = product_unchecked(l);
    is_valid(proof(l, expected_total), l, expected_total);
  };

  printf(
    "Does product [1; 2; 3; 4; 5] = 120? %b\n",
    proved([1, 2, 3, 4, 5]),
  );
};

/* Exercise 5: Comment this out when you're ready to test it! */
/* let () = exercise5 () */

/* Exercise 6:
      Adapt your solution to exercise 5 to create a checked version of
      [product_triple] below.
   */

let product_triple = ((x, y, z): (Field.t, Field.t, Field.t)): Field.t =>
  Field.(x * y * z);

let exercise6 = () => failwith("Exercise 6");

/* Exercise 6: Comment this out when you're ready to test it! */
/* let () = exercise6 () */

/* At this point, we have covered one way to pass data to a checked
      computation: describing what types the data has in our [Data_spec], and
      then passing the data into [prove]. This works perfectly well, but we have
      to hand the same data to the verifier before they can check our proof! To
      avoid handing over data that we want to keep 'secret', we need a different
      strategy.

      The most general way to do this is using the [exists] function. It takes a
      [Typ.t] argument, so that it knows how to translate the data, and two
      (optional) arguments
      * [~compute] passes a computation of type [('value, _) As_prover.t] which
        describes how to compute the value
      * [~request] passes a computation of type
        [('value Request.t, _) As_prover.t], which describes how to build a
        'request' for the a value of type ['value].

      For now, we will focus on the [~compute] argument.

      Exercise 7:
      Rework your solution to exercise 5 to provide a proof that [product] and
      [product_unchecked] return the same value, but without exposing the
      result from [product_unchecked].
   */

let product = (l: list(Field.t)): Field.t =>
  failwith("Exercise 7");

let product_unchecked = (l: list(Field.Constant.t)) =>
  List.fold(~init=Field.Constant.one, ~f=Field.Constant.mul, l);

let product_equals = (l: list(Field.t)) => {
  let total = product(l);
  let expected_total =
    exists(
      Field.typ,
      ~compute={
        open As_prover;
        /* Everything in this block is run 'as the prover'.

              This means that we have special powers, like reading the values
              from our checked computation back into normal OCaml values.
           */
        let l = read(Typ.list(~length=List.length(l), Field.typ), l);
        /* Now we have l back as a [Field.Constant.t list], so we can call
              [product_unchecked] on it.
           */
        () => product_unchecked(l);
      },
    );

  Field.Assert.equal(total, expected_total);
};

let exercise7 = () => {
  let input = () => failwith("Exercise 7: Data_spec here");
  let keypair = failwith("Exercise 7: Keypair here");
  let proof = l => failwith("Exercise 7: Proof");
  let is_valid = (proof, l) => failwith("Exercise 7: Verify");
  let proved = (l: list(int)) => {
    let l: list(Field.t) = (List.map(~f=Field.of_int, l): list(Field.t));
    is_valid(proof(l), l);
  };

  printf(
    "Have we proved that we've calculated the product of the list [1; 2; 3; 4; 5]? %b\n",
    proved([1, 2, 3, 4, 5]),
  );
};

/* Exercise 7: Comment this out when you're ready to test it! */
/* let () = exercise7 () */

/* Now we can say that a value 'exists' without giving it away to the verifier,
      but we have to say right there in our checked computation how we're going to
      work it out.

      If we don't have all the information we need yet, we can build a Request for
      it instead, and use [exists ~request] to send it. New kinds of request can
      be made by extending the [Request.t] type.

      When we are ready to handle the request, we use the [handle] function.
      A handler looks like [fun (With {request; respond}) -> ...], where [request]
      is the request received and respond lets us send back the new value.

      Exercise 8:
      Fill in [in_list] and [choose_two_from_list].
   */

/* Add a new type of request [Choose_two_from_list], which takes an ['a list].
      Handlers for this request have to return a value of type ['a].
   */
type Request.t(_) +=
  | Choose_two_from_list(list('a)): Request.t(('a, 'a));

let in_list = (x: Field.t, l: list(Field.t)) =>
  /* Assert that x is equal to one of the values in l. */
  failwith("Exercise 8");

let choose_two_from_list = (l: list(Field.t)) =>
  exists(
    Typ.(Field.typ * Field.typ),
    ~request=
      As_prover.
        /* Read the values from l and create a [Choose_two_from_list] request
           with them. */
        (failwith("Exercise 8")),
  );

let chosen_two_different = (l: list(Field.t)) => {
  let (choice1, choice2) = choose_two_from_list(l);
  in_list(choice1, l);
  in_list(choice2, l);
  Field.Assert.not_equal(choice1, choice2);
};

let exercise8 = () => {
  let input = () => failwith("Exercise 8: Data_spec here");
  let keypair = failwith("Exercise 8: Keypair here");
  let secret1 = 1;
  let secret2 = 3;
  let handled_chosen_two_different = l =>
    /* Add a handler for our [Choose_two_from_list] request */
    handle(() => chosen_two_different(l), (With({request, respond})) =>
      switch (request) {
      | Choose_two_from_list(l) =>
        let choice1 = List.nth_exn(l, secret1);
        let choice2 = List.nth_exn(l, secret2);
        respond([@implicit_arity] Provide(choice1, choice2));
      | _ => unhandled
      }
    );

  let proof = l =>
    prove(Keypair.pk(keypair), input(), (), handled_chosen_two_different, l);

  let is_valid = (proof, l) =>
    verify(proof, Keypair.vk(keypair), input(), l);
  let proved = (l: list(int)) => {
    let l: list(Field.t) = (List.map(~f=Field.of_int, l): list(Field.t));
    is_valid(proof(l), l);
  };

  printf(
    "Have we chosen two different values from list [1; 2; 3; 4; 5]? %b\n",
    proved([1, 2, 3, 4, 5]),
  );
};

/* Exercise 8: Comment this out when you're ready to test it! */
/* let () = exercise8 () */

module Exercise9 = {
  /* We can define a matrix over some ring as follows */
  module Matrix =
         (
           R: {
             [@deriving sexp]
             type t;

             let zero: t;

             let mul: (t, t) => t;

             let add: (t, t) => t;
           },
         ) => {
    [@deriving sexp]
    type t = array(array(R.t));

    let rows = t => Array.length(t);

    let row = (t, i) => t[i];

    let col = (t, i) => Array.map(t, ~f=xs => xs[i]);

    let cols = t => Array.length(t[0]);

    let mul = (a, b) => {
      /* n x m * m x p -> n x p */
      assert(cols(a) == rows(b));
      Array.init(rows(a), ~f=i =>
        Array.init(cols(b), ~f=j =>
          Array.fold2_exn(
            row(a, i), col(b, j), ~init=R.zero, ~f=(acc, aik, bkj) =>
            R.add(acc, R.mul(aik, bkj))
          )
        )
      );
    };
  };

  /* A Field is a ring */
  module Mat = Matrix(Field.Constant);

  /* We can multiply */
  let a =
    Field.(
      [|
        [|of_int(1), of_int(2), of_int(3)|],
        [|of_int(4), of_int(5), of_int(6)|],
      |]
    );

  let b =
    Field.(
      [|
        [|of_int(1), of_int(2)|],
        [|of_int(3), of_int(4)|],
        [|of_int(5), of_int(6)|],
      |]
    );

  /* let () = printf !"Result %{sexp: Mat.t}\n%!" (Mat.mul a b) */

  /* Exercise 9:
        To bring everything together, we want to prove something more substantial.
        Here, we will build a program that
        * creates a secret matrix
        * squares it
        * proves that it knows the square root of the result
        * reveals that result and the proof to the verifier.

        To start, fill in [random_matrix], making sure that the number of rows and
        columns in the matrix are the same.
        (Feel free to hard-code a matrix here to start with; it makes it much
        easier to track down any small bugs!)
     */
  let random_matrix = () => failwith("Exercise 9: Random matrix");

  module Mat_checked = {
    type t = array(array(Field.t));

    /* Next, we need to make a checked version of [Matrix.mul] from above.
         This should feel familiar: we did a very similar thing when we were
         finding the product of a list!
       */
    let mul: (t, t) => t = (
      (a, b) => failwith("Exercise 9: Write mul"): (t, t) => t
    );

    module Assert = {
      /* Now, we want a way to say that two matrices are equal. */
      let equal: (t, t) => unit = (
        (a, b) => failwith("Exercise 9: Write Assert.equal"):
          (t, t) => unit
      );
    };
    /* Bonus: Try an adjust the Matrix definition above to functor over a
          monad, making mul monadic. Then instantiate the Field version with the
          identity monad, and the Field.Checked version with the Checked monad.
       */
  };

  /* Create a [Typ.t] to let us convert between [Mat.t] and [Mat_checked.t].

        NB: SNARKs require fixed-size inputs, so this Typ.t will have to describe
        matrices of a fixed-size. Make sure that this size matches your
        [random_matrix]'s output size!
     */
  let typ = (): Typ.t(Mat_checked.t, Mat.t) =>
    failwith("Exercise 9: Write typ");

  /* Fill in this function to check that [sqrt_x] squares to [x]. */
  let assert_is_sqrt = (x: Mat_checked.t, sqrt_x: Mat_checked.t) =>
    failwith("Exercise 9: Write assert_is_sqrt");

  /* Add a new kind of [Request.t] that asks for the square root matrix that we
     will generate later, and use [assert_is_sqrt] to check that it squares to
     the matrix we've been given. */
  let assert_exists_sqrt = (x: Mat_checked.t) =>
    failwith("Exercise 9: Write assert_exists_sqrt");

  let input = () => Data_spec.[typ()];

  let keypair = () => generate_keypair(~exposing=input(), assert_exists_sqrt);

  /* Build a proof.
        This should consist of:
        * creating a random matrix (to use as our square root)
        * squaring this matrix, to work out what our public input should be
        * setting a handler for the [Request.t] that [assert_exists_sqrt] sends
        * creating a proof for [assert_exists_sqrt]

        NB: This function should return the public input and the proof, so that
        the input can be passed to the verifier.
     */
  let proof = (): (Mat.t, Proof.t) => failwith("Exercise 9: Write proof");

  /* Verify the proof. */
  let is_valid = (proof, x: Mat.t) => failwith("Exercise 9: Write is_valid");

  let run = () => {
    let (x, proof) = proof();
    printf(
      "Does %{sexp: Mat.t} have a square root?\n%b%!"^,
      x,
      is_valid(proof, x),
    );
  };
};

/* Exercise 9: Comment this out when you're ready to test it! */
/* let () = Exercise9.run () */
/* TODO: To_bits of_bits */
