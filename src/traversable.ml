open Core_kernel

module type Basic = sig
  type 'a t

  (* TODO-someday: Should be applicative, it's anonying because Applicative.S is not a subsignature
   of Monad.S, but Monad.S is more common so we go with that. *)
  module Traverse (A : Monad.S) : sig
    val f : 'a t -> f:('a -> 'b A.t) -> 'b t A.t
  end
end

module type S = sig
  include Basic

  module Traverse2 (A : Monad.S2) : sig
    val f : 'a t -> f:('a -> ('b, 'c) A.t) -> ('b t, 'c) A.t
  end

  module Traverse3 (A : Monad.S3) : sig
    val f : 'a t -> f:('a -> ('b, 'c, 'd) A.t) -> ('b t, 'c, 'd) A.t
  end
end

module type Res = sig
  type result

  val result : result
end

module Make (Basic : Basic) = struct
  include Basic

  module Traverse2 (A : Monad.S2) = struct
    let f (type b c) (x : 'a t) ~(f : 'a -> (b, c) A.t) =
      let (module Res : Res with type result = (b t, c) A.t) =
        ( module struct
          module Traverse = Traverse (struct
            type 'a t = ('a, c) A.t

            include (A : Monad.S with type 'a t := 'a t)
          end)

          type result = (b t, c) A.t

          let result = Traverse.f x ~f
        end )
      in
      Res.result
  end

  module Traverse3 (A : Monad.S3) = struct
    let f (type b c d) (x : 'a t) ~(f : 'a -> (b, c, d) A.t) =
      let (module Res : Res with type result = (b t, c, d) A.t) =
        ( module struct
          module Traverse = Traverse (struct
            type 'a t = ('a, c, d) A.t

            include (A : Monad.S with type 'a t := 'a t)
          end)

          type result = (b t, c, d) A.t

          let result = Traverse.f x ~f
        end )
      in
      Res.result
  end
end
