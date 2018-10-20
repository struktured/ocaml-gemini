open Common

module Request = Nonce.Request
type uri_args = [`None] [@@deriving sexp, enumerate]


type request =
  { heartbeat : bool option [@default None] }
[@@deriving sexp, yojson]

module Message_type : sig
  type t = [`Update | `Heartbeat] [@@deriving sexp, enumerate]
  include Json.ENUM_STRING with type t := t
end

type heartbeat = {timestampms:Timestamp.ms;
                  sequence:int_number;
                  trace_id:string;
                  socket_sequence:int_number
                 } [@@deriving sexp, yojson]

module Event_type : sig
  type t = [ `Subscription_ack
           | `Heartbeat
           | `Initial
           | `Accepted
           | `Rejected
           | `Booked
           | `Fill
           | `Cancelled
           | `Closed
           ] [@@deriving sexp, enumerate]

  include Json.ENUM_STRING with type t:= t
end


type query = [ `Symbol_filter of Symbol.t
             | `Event_type_filter of Event_type.t
             | `Api_session_filter of string
             ] [@@deriving sexp]

val encode_query : query -> string * string

module Reason : sig
  type t =
    [`Place | `Trade | `Cancel | `Initial] [@@deriving sexp, enumerate]
  include Json.ENUM_STRING with type t := t
end

module Liquidity : sig
  type t = [`Taker] [@@deriving sexp, enumerate]
  include Json.ENUM_STRING with type t := t
end

type fill = {
  trade_id:int_string;
  liquidity:Liquidity.t;
  price:decimal_string;
  amount:decimal_string;
  fee:decimal_string;
  fee_currency:Currency.t;
} [@@deriving sexp, yojson]

module Api_session : sig
  type t = string [@@deriving sexp,yojson]
end

type order_event =
  {order_id:string;
   api_session:Api_session.t;
   client_order_id:string option [@default None];
   event_id:string option [@default None];
   order_type:Order_type.t;
   symbol:Symbol.t;
   side:Side.t;
   behavior:string option [@default None];
   type_ : Event_type.t [@key "type"];
   timestamp:Timestamp.t;
   timestampms:Timestamp.ms;
   is_live : bool;
   is_cancelled : bool;
   is_hidden : bool;
   avg_execution_price : decimal_string option [@default None];
   executed_amount : decimal_string option [@default None];
   remaining_amount : decimal_string option [@default None];
   original_amount : decimal_string option [@default None];
   price : decimal_string option [@default None];
   total_spend : decimal_string option [@default None];
   fill : fill option [@default None];
   socket_sequence:int_number
  } [@@deriving sexp, yojson]

type subscription_ack =
  {account_id:int_number [@key "accountId"];
   subscription_id:string [@key "subscriptionId"];
   symbol_filter:Symbol.t list [@key "symbolFilter"];
   api_session_fiter:string list [@key "apiSessionFilter"];
   event_type_filter:Event_type.t list [@key "eventTypeFilter"]
  } [@@deriving sexp, yojson]

type response =
  [ `Subscription_ack of subscription_ack
  | `Heartbeat of heartbeat
  | `Order_event of order_event
  | `Order_events of order_event list
  ] [@@deriving sexp]



val handle_client :
  [< Socket.Address.t ] -> Reader.t -> Writer.t -> unit Deferred.t
val command : decimal_string * Async.Command.t
val client :
  nonce:int Pipe.Reader.t ->
  (module Cfg.S) ->
  ?query:Sexp.t sexp_list ->
  ?uri_args:uri_args ->
  unit -> (unit * unit) Async_extra__.Import.Deferred.t

