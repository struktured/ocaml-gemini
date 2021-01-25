(** A strongly typed gemini trading api written in pure OCaml
    with both websocket and rest endpoints supported.

    Submodules of the [V1] module are either REST operations which
    can be invoked via a well typed invocation of [Operation.post],
    or they are web socket interfaces such as [Market_data] and
    [Order_events]. Use the [client] function to get a typed response pipe over
    the socket of these services.

    All service invocations require a [Cfg] module which is usually
    provided from the command line or environment variables for api host and
    secret information.
*)

open Common
module Auth = Auth
module Result = Json.Result


(** Version v1 of the Gemini REST and web socket apis. *)
module V1 = V1
