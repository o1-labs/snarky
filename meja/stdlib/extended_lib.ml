let line = __LINE__ + 3

let t =
  {meja|
request((field, field)) Base_point;

module Extended_lib = {
  type quadruple('a) = ('a, 'a, 'a, 'a);
  type triple('a) = ('a, 'a, 'a);
  type double('a) = ('a, 'a);

  let max_int32 = 4294967295i;

  module UInt32 = {
    let length = 32i;

    type t = array(boolean);

    let xor : t -> t -> t = fun (t1, t2) => {
      Array.map2(Boolean.lxor, t1, t2);
    };

    let zero : t = Array.init(length, fun (_) => { 0b; });

    let ceil_log2 = loop(fun (self, n) => {
      switch (n) {
        | 0i => 0i
        | _ => 1i + self(n / 2i)
      };
    });

    let take = fun (n, xs) => {
      let (xs, _) =
        List.fold_left(fun ((xs, k), x) => {
          switch (k >= n) {
            | true => (xs, k)
            | false => (x :: xs, k + 1i)
          };
        }, ([], 0i), xs);
      List.rev(xs);
    };

    let sum : list(t) -> t = fun (xs) => {
      let max_bit_length : int = ceil_log2(List.length(xs) * max_int32);
      let xs_sum =
        List.fold_left(
          fun (acc, x) => {
            Field.(+)(Field.of_bits(Array.to_list(x)), acc);
          }, 
          0,
          xs);
      take(
        32i,
        // TODO: Habe allow_overflow:false
        Field.to_bits(~length=max_bit_length, xs_sum))
      |> Array.of_list;
    };

    let rotr : t -> int -> t = fun (t, by) => {
      Array.init(length, fun (i) => {
          Array.get(t, (mod(i + by, length))); });
    };

    let int_get_bit = fun (n : int, i : int) => {
      switch(land(lsr(n, i), 1i)) {
        | 1i => 1b
        | _ => 0b
      };
    };

    let of_int : int -> t = fun (n) => {
      loop( 
        fun (self, (i, acc)) => {
          switch (i) {
            | 32i => acc
            | _ =>
              self((i + 1i,  int_get_bit(n, (31i - i)) :: acc))
          };
        },
        (0i, [])
      ) |> Array.of_list;
    };
  };

  module Blake2 = {
    let r1 = 16i;

    let r2 = 12i;

    let r3 = 8i;

    let r4 = 7i;

    let mixing_g = fun (v, a, b, c, d, x, y) => {
      let ( = ) = fun (i, t) => { Array.set(v, i, t); };
      let (!) = Array.get(v);
      let sum = UInt32.sum;
      let xorrot = fun (t1, t2, k) => {
        UInt32.rotr(UInt32.xor(t1, t2), k);
      };
      a = sum([!a, !b, x]) ;
      d = xorrot( !d, !a, r1 );
      c = sum([!c, !d]);
      b = xorrot( !b, !c, r2 );
      a = sum([ !a, !b, y]);
      d = xorrot(!d, !a, r3);
      c = sum([!c, !d]);
      b = xorrot(!b, !c, r4);
    };

    let iv =
      Array.map(
        UInt32.of_int,
        Array.of_list(
          [ 1779033703i
          , 3144134277i
          , 1013904242i
          , 2773480762i
          , 1359893119i
          , 2600822924i
          , 528734635i
          , 1541459225i ]));

    let sigma =
      Array.of_list (
      [ [0i, 1i, 2i, 3i, 4i, 5i, 6i, 7i, 8i, 9i, 10i, 11i, 12i, 13i, 14i, 15i]
      , [14i, 10i, 4i, 8i, 9i, 15i, 13i, 6i, 1i, 12i, 0i, 2i, 11i, 7i, 5i, 3i]
      , [11i, 8i, 12i, 0i, 5i, 2i, 15i, 13i, 10i, 14i, 3i, 6i, 7i, 1i, 9i, 4i]
      , [7i, 9i, 3i, 1i, 13i, 12i, 11i, 14i, 2i, 6i, 5i, 10i, 4i, 0i, 15i, 8i]
      , [9i, 0i, 5i, 7i, 2i, 4i, 10i, 15i, 14i, 1i, 11i, 12i, 6i, 8i, 3i, 13i]
      , [2i, 12i, 6i, 10i, 0i, 11i, 8i, 3i, 4i, 13i, 7i, 5i, 15i, 14i, 1i, 9i]
      , [12i, 5i, 1i, 15i, 14i, 13i, 4i, 10i, 0i, 7i, 6i, 3i, 9i, 2i, 8i, 11i]
      , [13i, 11i, 7i, 14i, 12i, 1i, 3i, 9i, 5i, 0i, 15i, 4i, 8i, 6i, 2i, 10i]
      , [6i, 15i, 14i, 9i, 11i, 3i, 0i, 8i, 12i, 2i, 13i, 7i, 1i, 4i, 10i, 5i]
      , [10i, 2i, 8i, 4i, 7i, 6i, 1i, 5i, 15i, 11i, 9i, 14i, 3i, 12i, 13i, 0i] ] )
      |> Array.map(Array.of_list);

    let splitu64 = fun (u : Int64.t) => {
      let low = Int64.logand(u, Int64.of_int(max_int32));
      let high = Int64.shift_right(u, 32i);
      (low, high);
    };

    let for_ : int -> (int -> unit) -> unit = fun (n, f) => {
      loop(fun(self, i) => {
        switch (i = n) {
          | true => ()
          | false => {
            f(i);
            self(i + 1i);
            }
        };
      }, 0i);
    };

    open UInt32;

    let compression = fun (h, m : array(UInt32.t), t, f) => {
      let v = Array.append (h, iv);
      let (tlo, thi) = splitu64(t);
      Array.set(v, 12i, xor(Array.get(v, 12i), of_int(Int64.to_int(tlo))));
      Array.set(v, 13i, xor(Array.get(v, 13i), of_int(Int64.to_int(thi))));

      switch (f) {
        | false => ()
        | true =>
          Array.set(v, 14i, xor(Array.get(v, 14i), of_int(max_int32)))
      };

      for_(10i, fun (i) => {
        let s = Array.get(sigma, i);
        let mix = fun (a, b, c, d, i1, i2) => {
          mixing_g(v, a, b, c, d, Array.get(m, Array.get(s, i1)), Array.get(m, Array.get(s, i2)));
        };
        mix(0i, 4i, 8i, 12i, 0i, 1i);
        mix(1i, 5i, 9i, 13i, 2i, 3i);
        mix(2i, 6i,10i, 14i, 4i, 5i);
        mix(3i, 7i,11i, 15i, 6i, 7i);
        mix(0i, 5i,10i, 15i, 8i, 9i);
        mix(1i, 6i,11i, 12i,10i,11i);
        mix(2i, 7i, 8i, 13i,12i,13i);
        mix(3i, 4i, 9i, 14i,14i,15i);
      });

      for_(8i, fun(i) => {
        Array.set(h, i, xor(Array.get(h, i), Array.get(v, i)));
        Array.set(h, i, xor(Array.get(h, i), Array.get(v, i + 8i)));
      });
    };

    let block_size_in_bits = 512i;

    let digest_length_in_bits = 256i;

    let pad_input = fun (bs) => {
      let n = Array.length(bs);
      switch (mod(n, block_size_in_bits)) {
        | 0i => bs
        | k => 
          Array.append(bs,
            Array.create(block_size_in_bits - k, 0b))
      };
    };

    let concat_int32s = fun (ts : array(UInt32.t)) => {
      let n = Array.length(ts);
      Array.init(n * UInt32.length, fun (i) => {
        Array.get(
          Array.get(ts, (i / UInt32.length)),
          mod(i, UInt32.length));
      });
    };

    let personalization =
      String.init( 8i, fun (_) => { char_of_int(0i); });

    let blake2s : list(boolean) -> list(boolean) = fun (input) => {
      let input = Array.of_list(input);
      let p = fun (o) => {
        let c = fun(j) => {
          lsl(
            int_of_char(String.get(personalization, o + j)),
            8i * j);
        };
        c(0i) + c(1i) + c(2i) + c(3i);
      };
      let h =
        /* Here we xor the initial values with the parameters of the
          hash function that we're using:
          depth = 1
          fanout = 1
          digest_length = 32
          personalization = personalization */
        Array.map(
          UInt32.of_int,
          Array.of_list(
            [ lxor(1779033703i, 16842752i)
            , 3144134277i
            , 1013904242i
            , 2773480762i
            , 1359893119i
            , 2600822924i
            , lxor(528734635i, p(0i))
            , lxor(1541459225i, p(4i))
            ]));
      let padded = pad_input(input);
      let blocks : array(array(UInt32.t)) = {
        let n = Array.length(padded);
        switch (n) {
          | 0i =>
            Array.of_list(
              [ Array.create(
                  block_size_in_bits / UInt32.length,
                  UInt32.zero) ])
          | _ =>
            Array.init(n / block_size_in_bits, fun (i) => {
                Array.init(block_size_in_bits / UInt32.length, fun (j) => {
                    Array.init(UInt32.length, fun (k) => {
                      Array.get(padded,
                                (block_size_in_bits * i)
                                + (UInt32.length * j)
                                + k);
                    });
                });
            })
        };
      };
      for_(Array.length(blocks) - 1i, fun (i) => {
          compression(
            h, Array.get(blocks ,i),
            Int64.mul(
              Int64.add(Int64.of_int(i), Int64.of_int(1i)),
              Int64.of_int(64i)),
            false);
      });
      let input_length_in_bytes = (Array.length(input) + 7i) / 8i;
      compression(h,
        Array.get(blocks, Array.length(blocks) - 1i),
        Int64.of_int(input_length_in_bytes),
        true);
      concat_int32s(h) |> Array.to_list;
    };

    let string_to_bool_list = fun (s) => {
      List.init(8i * String.length(s), fun (i) => {
        let c = int_of_char(String.get(s, i / 8i));
        let j = mod(i, 8i);
        land(lsr(c,j), 1i) = 1i;
      });
    };
  };

  module Curve = {
    type coefficients('a) = {
      a : 'a,
      b : 'a
    };

    open Field;

    // Would be nice to have "subset syntax" for the Typ for this type.
    type t = double(field);

    let div_unsafe = fun (x, y) => {
      let z : field =  Prover { Field.(/)(x, y); };
      assert_r1(z, y, x);
      z;
      /* It would be nice if this were a special syntax but that's
          a "nice to have" */
      /* assert (x * y == z); */
    };

    let add_helper = fun (div, (ax, ay), (bx, by)) => {
      let lambda = div(Field.(-)(by, ay), Field.(-)(bx, ax));
      let cx = Prover {
        square(lambda) - (ax + bx);
      };
      let cy = Prover {
        (lambda * (ax - cx)) - ay;
      };
      assert_r1(lambda, lambda, cx + ax + bx);
      assert_r1(lambda, (ax - cx) , (cy + ay));
      (cx, cy);
    };

    let add_unsafe = add_helper(div_unsafe);
    let add = add_helper(Field.(/));

    let double = fun ((x, y)) => {
      let xy = x * y;
      let xx = x * x;
      let yy = y * y;
      let a = (2 * xy) / (xx + yy);
      let b = (yy - xx) / (2 - xx - yy);
      (a, b);
    };

    module Assert = {
      let on_curve = fun ({a, b}, (x, y)) => {
        let fx = x * (x * x + a) + b;
        assert_r1(y, y, fx);
      };

      let not_equal = fun ((x1, y1), (x2, y2)) => {
        Boolean.Assert.any([
          Boolean.not(Field.equal(x1, x2)),
          Boolean.not(Field.equal(y1, y2))
        ]);
      };

      let equal = fun ((x1, y1), (x2, y2)) => {
        Field.Assert.equal(x1, x2);
        Field.Assert.equal(y1, y2);
      };

    };

    let negate = fun ((x, y)) => {
      (x, (0 - y));
    };

    let scale = fun (coeffs, bs: list(boolean), g : t) => {
      let base_point : t = request { Base_point; };
      Assert.on_curve(coeffs, base_point);
      Assert.not_equal(base_point, g);

      let (acc, _) =
        List.fold_left(fun ((acc, two_i_g), b) => {
          let acc = select(b, ~then_=add(acc, two_i_g), ~else_=acc);
          let two_i_g = double(two_i_g);
          (acc, two_i_g);
        }, (base_point, g), bs);

      add(acc, negate(base_point));
    };

  };

  module Merkle_tree = {
    // A Merkle tree with field elements at the leaves.
    type t =
      | Leaf (Field.Constant.t)
      | Node (Field.Constant.t , t , t);

    let get_hash = fun (t) => {
      switch (t) {
        | Leaf (x) => x
        | Node (h, _, _) => h
      };
    };

    // Here the path has the most significant bit first.
    let get_path = fun (t, addr) => {
      loop(fun(self, (t, addr)) => {
        switch (t, addr) {
          | (Leaf (_), []) => []
          | (Node (h, l, r), true :: bs) =>
            get_hash(l) :: self((r, bs))
          | (Node (h, l, r), false :: bs) =>
            get_hash(r) :: self((l, bs))
        };
      }, (t, addr));
    };

    /* The path should have the neighbors from top to bottom
       The address should have the bits from top to bottom
    */
    let implied_root =
      fun (~hash, leaf, (path: list(field)), addr) => {
        List.fold_left2 (fun (acc : field, neighbor, b) => {
            let (left, right) = select(
              b
              , ~then_=(neighbor, acc)
              , ~else_=(acc, neighbor));
            hash(
              Field.to_bits(left) @ Field.to_bits(right));
          },
          leaf,
          List.rev(path),
          List.rev(addr));
      };
  };

  module Pedersen = {
    module Digest = {
      type t = field;

      let to_bits = Field.to_bits(~length=Field.size_in_bits);
    };

    module Params = {
      type t = array(quadruple((field, field)));

      let load = fun (path) => {
        let comma = char_of_int(44i);
        let semi_colon = char_of_int(59i);

        let read_pair = fun (s) => {
          switch (String.split_on_char(comma, s)) {
            | [ x, y ] =>
              (Field.of_string(x), Field.of_string(y))
          };
        };

        let strs = Array.of_list(read_lines(path));

        Array.map(fun (s) => {
          switch ( List.map(read_pair, String.split_on_char(semi_colon, s)) ) {
            | [x1, x2, x3, x4] => (x1, x2, x3, x4)
          };
        }, strs);
      };
    };

    /* 4 * 2 = 2 * 4 */
    let transpose : quadruple(double('a)) -> double(quadruple('a)) =
      fun ( ((x0, y0), (x1, y1), (x2, y2), (x3, y3)) ) => {
        ( (x0, x1, x2, x3), (y0, y1, y2, y3) );
      };

    let add_int = ( + );

    open Field;

    let lookup = fun ((s0, s1, s2) : triple(boolean), q : quadruple(Curve.t)) => {
      let s_and = Boolean.(&&)(s0, s1) ;
      let bool : boolean -> field = Boolean.to_field;
      let lookup_one = fun ((a1, a2, a3, a4)) => {
        a1
        + ((a2 - a1) * bool(s0)) /* Need some way to make booleans field elements */
        + ((a3 - a1) * bool(s1))
        + ((a4 + a1 - a2 - a3) * bool(s_and));
      };
      let (x_q, y_q) = transpose(q);
      let y = lookup_one(y_q);
      let neg_one = 0 - 1;
      let s2 = bool(s2);
      let a0 = 2 * s2;
      let a1 = 1 - a0;
      let y = a1 * y; 
      (lookup_one(x_q), y);
    };

    let digest = fun (params, triples : list(triple(boolean))) : Digest.t => {
      switch (triples) {
        | [] => failwith("Cannot handle empty list")
        | (t::ts) => {
          let (_, (x, _y)) =
            List.fold_left (fun ((i, acc), t) => {
                let term = lookup(t, Array.get(params, i));
                (add_int(i, 1i), Curve.add_unsafe(acc,term));
              }, (1i, lookup(t, Array.get(params, 0i) )), ts);
          x;
        }
      };
    };

    type three('a) =
      | Zero
      | One ('a)
      | Two ('a, 'a);

    let group3 = fun (xs) => {
      let default=0b;
      let (ts, r) =
        List.fold_left (fun ((ts, acc), x) => {
            switch (acc) {
              | Zero => (ts, One(x))
              | One(x0) => (ts, Two(x0, x))
              | Two(x0, x1) => ((x0, x1, x) :: ts, Zero)
            };
          },
          ([], Zero),
          xs
        );
      let ts =
        switch(r) {
          | Zero => ts
          | One(x0) => (x0, default, default) :: ts
          | Two(x0, x1) => (x0, x1, default) :: ts
        };
      List.rev(ts);
    };

    let digest_bits : Params.t -> list(boolean) -> field =
      fun (params, bs) => { digest(params, group3(bs)); };
  };
};
|meja}
