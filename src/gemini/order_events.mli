(** Order events web sockets api for the gemini trading exchange. This
    is used to get private events about orders and trades without having to
    depend on the slower and clumsier json REST api. Users still must submit
    orders using REST, however. This is only for responses.
*)

open Common
module Request = Nonce.Request

type uri_args = [ `None ] [@@deriving sexp, enumerate]
(** The order events api has no uri arguments .*)

type heartbeat = {
  timestampms : Timestamp.Ms.t;
  sequence : Int_number.t;
  trace_id : string;
  socket_sequence : Int_number.t;
}
[@@deriving sexp, yojson]
(** Response type for heart beat messages. *)

(** Represents different order event types. *)
module Order_event_type : sig
  type t =
    [ `Subscription_ack
    | `Heartbeat
    | `Initial
    | `Accepted
    | `Rejected
    | `Booked
    | `Fill
    | `Cancelled
    | `Closed ]
  [@@deriving sexp, enumerate, compare]
  (** Enumerates all possible order events. *)

  include Json.S with type t := t
  include Comparable with type t := t
end

type query =
  [ `Symbol_filter of Symbol.t
  | `Event_type_filter of Order_event_type.t
  | `Api_session_filter of string ]
[@@deriving sexp]
(** Events can be filtered by symbol using [Symbol_filter symbol] or
    for an event type using [Order_event_type_filter event_type] or using
    an api session filter [Api_session_filter session-id].
*)

(** Represents differents reasons an order event occurred. *)
module Reason : sig
  type t = [ `Place | `Trade | `Cancel | `Initial ] [@@deriving sexp, enumerate]

  include Json.ENUM_STRING with type t := t
end

module Reject_reason : sig
  type t = [ `Invalid_quantity ] [@@deriving sexp, enumerate]

  include Json.ENUM_STRING with type t := t
end

(** Represents different liquidity types *)
module Liquidity : sig
  type t = [ `Taker ] [@@deriving sexp, enumerate]
  (** [Taker] is the only known liquidity type but this
      is poorly documented so other values might exist.
  *)

  include Json.ENUM_STRING with type t := t
end

(** Type type of an order fill event. *)
module Fill : sig
  type t = {
    trade_id : Int_string.t;
    liquidity : Liquidity.t;
    price : Decimal_string.t;
    amount : Decimal_string.t;
    fee : Decimal_string.t;
    fee_currency : Currency.t;
  }
  [@@deriving sexp, yojson, fields, csv]
end

(** Represents an api session name *)
module Api_session : sig
  type t = string [@@deriving sexp, yojson]
  (** An api session name. *)
end

module Order_event : sig
  type t = {
    order_id : string;
    api_session : Api_session.t;
    client_order_id : string option; [@default None]
    event_id : string option; [@default None]
    order_type : Order_type.t;
    symbol : Symbol.t;
    reason : Reject_reason.t option; [@default None]
    side : Side.t;
    behavior : string option; [@default None]
    type_ : Order_event_type.t; [@key "type"]
    timestamp : Timestamp.t;
    timestampms : Timestamp.Ms.t;
    is_live : bool;
    is_cancelled : bool;
    is_hidden : bool;
    avg_execution_price : Decimal_string.t option; [@default None]
    executed_amount : Decimal_string.t option; [@default None]
    remaining_amount : Decimal_string.t option; [@default None]
    original_amount : Decimal_string.t option; [@default None]
    price : Decimal_string.t option; [@default None]
    total_spend : Decimal_string.t option; [@default None]
    fill : Fill.t option; [@default None]
    socket_sequence : Int_number.t;
  }
  [@@deriving sexp, yojson, fields, csv]
  (** The type of an order event. *)
end

module Subscription_ack : sig
  type t = {
    account_id : Int_number.t; [@key "accountId"]
    subscription_id : string; [@key "subscriptionId"]
    symbol_filter : Symbol.t list; [@key "symbolFilter"]
    api_session_fiter : string list; [@key "apiSessionFilter"]
    event_type_filter : Order_event_type.t list; [@key "eventTypeFilter"]
  }
  [@@deriving sexp, yojson, fields, csv]
  (** The type of a subscription acknowledgement event. *)
end

type response =
  [ `Subscription_ack of Subscription_ack.t
  | `Heartbeat of heartbeat
  | `Order_event of Order_event.t
  | `Order_events of Order_event.t list ]
[@@deriving sexp]
(** The response type for any order message. *)

module Event_type : sig
  type t = [ `Order_event | `Subscription_ack ]
  [@@deriving sexp, enumerate, compare]

  include Comparable.S with type t := t
  include Json.S with type t := t
end

include
  Ws.CHANNEL
    with module Event_type := Event_type
    with type uri_args := uri_args
    with type query := query
    with type response := response

val command : string * Async.Command.t

val client :
  nonce:int Inf_pipe.Reader.t ->
  (module Cfg.S) ->
  ?query:Sexp.t list ->
  ?uri_args:uri_args ->
  unit ->
  response Pipe.Reader.t Deferred.t
