(** Custom requests.

    These allow checked computations to use information that isn't passed as
    arguments -- and hence isn't compiled into the proof. These can be used to
    include 'secret' information in computations which should be hidden from
    verifiers.

    To add a new kind of request, extend {!type:t}:
{[
type _ Request.t += Foo : request_type -> result_type Request.t
]}
    When a checked computation [comp] makes a request
    ({!val:Snark_intf.Basic.request}) using the new [Foo] constructor,
    you can attach a handler to respond to it with
    {!val:Snark_intf.Basic.handle}:
{[
let handled_comp = handle comp (fun (With {request; respond}) ->
  match request with
  | Foo data ->
    let value = (* ... *) in
    respond (Provide value)
  | _ -> unhandled)
]}
*)

(** The type of all requests. This is an open type: you can extend it with your
    own requests as needed. *)
type _ t = ..

(** The fail request *)
type _ t += Fail : 'a t

type 'a req = 'a t

(** The type of responses. Use {!recfield:respond} to create a response from a
    {!type:Response.t}. *)
type response

(** Indicates an unhandled response. Equivalent to calling {!recfield:respond}
    on {!const:Response.Unhandled}. *)
val unhandled : response

module Response : sig
  (** The type of responses a handler may return. *)
  type nonrec 'a t = Provide of 'a | Delegate of 'a t | Unhandled
end

type request =
  | With : {request: 'a t; respond: 'a Response.t -> response} -> request

(** Internal, used by {!module:Snark0}. *)
module Handler : sig
  (** A single response handler. Create using {!val:create_single}. *)
  type single

  (** A stack of handlers which are run on any requests that are raised. *)
  type t

  val fail : t

  val create_single : (request -> response) -> single

  (** Add a single handler. *)
  val push : t -> single -> t

  (** Run the handler on a request. Throws an error on failure. *)
  val run : t -> string list -> 'a req -> 'a
end
