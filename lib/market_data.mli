(** Market data websockets api for the Gemini trading exchange. This
    broadcasts only public events and require no authentication.
*)
open! Common

(** Encapsulates various concepts of side in the market dat api. *)
module Side :
sig

  (** Represents a bid or ask side. *)
  module Bid_ask :
  sig
    type t = [ `Ask | `Bid ] [@@deriving sexp]
    include Json.S with type t := t
  end

  (** Represents an auction style side. *)
  module Auction :
  sig
    type t = [ `Auction ] [@@deriving sexp]
    include Json.S with type t := t
  end

  (** A most general type of side- one of [`Bid], [`Ask], or [`Auction]. *)
  type t = [ Bid_ask.t | Auction.t] [@@deriving sexp]

  include Json.S with type t := t
end

(** Represents market data message types supported by the Gemini exchange. *)
module Message_type :
sig
  (* The support message types for order events -
     either [`Update] or [`Heartbeat].
  *)
  type t = [`Heartbeat | `Update] [@@deriving sexp]
  include Json.S with type t := t
end

(** Represents different types of market data events *)
module Event_type :
sig
  (** Market data event types. One of  [`Trade], [`Change], and [`Auction]. *)
  type t = [`Trade | `Change | `Auction] [@@deriving sexp]
  include Json.S with type t := t
end

(** The heartbeat response type has no payload *)
type heartbeat = unit [@@deriving sexp, of_yojson]


(** Different reasons a market data event occured *)
module Reason :
sig
  (** Reasons a market data event occured *)
  type t =
    [`Place | `Trade | `Cancel | `Initial]
  [@@deriving sexp]
  include Json.S with type t := t
end

(** An change event to an order. The [reaosn] field indicates
    the type of change. *)
module Change_event :
sig
  type t = {
    price : Decimal_string.t;
    side : Side.Bid_ask.t;
    reason : Reason.t;
    remaining : Decimal_string.t;
    delta : Decimal_string.t;
  } [@@deriving sexp, of_yojson, csv, fields]
end

(** An trade event to an order. *)
type trade_event = {
  tid : Int_number.t;
  price : Decimal_string.t;
  amount : Decimal_string.t;
  maker_side : Side.t;
} [@@deriving sexp, of_yojson]

(** An auction open event. *)
type auction_open_event = {
  auction_open_ms : Timestamp.ms;
  auction_time_ms : Timestamp.ms;
  first_indicative_ms : Timestamp.ms;
  last_cancel_time_ms : Timestamp.ms;
} [@@deriving sexp, of_yojson]


(** Represents different results possible from an auction. *)
module Auction_result :
sig
  (** the type of an auction result- one of [`Success] or [`Failure]. *)
  type t =
    [`Success | `Failure] [@@deriving sexp]
  include Json.S with type t := t
end


(** An auction indicative price event. *)
type auction_indicative_price_event = {
  eid : Int_number.t;
  result : Auction_result.t;
  time_ms : Timestamp.ms;
  highest_bid_price : Decimal_string.t;
  lowest_ask_price : Decimal_string.t;
  collar_price : Decimal_string.t;
  indicative_price : Decimal_string.t;
  indicative_quantity : Decimal_string.t;
}

(** An auction outcome event. *)
type auction_outcome_event = {
  eid : Int_number.t;
  result : Auction_result.t;
  time_ms : Timestamp.ms;
  highest_bid_price : Decimal_string.t;
  lowest_ask_price : Decimal_string.t;
  collar_price : Decimal_string.t;
  auction_price : Decimal_string.t;
  auction_quantity : Decimal_string.t;
} [@@deriving sexp, of_yojson]


(** Represents different auction event types. *)
module Auction_event_type :
sig

  (** Enumerates an auction event type. *)
  type t =
    [ `Auction_open | `Auction_indicative_price | `Auction_outcome ]
  [@@deriving sexp]
  include Json.S with type t := t
end

(** The type of an auction event, unified over all auction event types. *)
type auction_event =
  [ `Auction_indicative_price of auction_indicative_price_event
  | `Auction_open of auction_open_event
  | `Auction_outcome of auction_outcome_event ]
[@@deriving sexp, of_yojson]

(** The type of event, unified over auction, change, and trade events. *)
type event =
  [ `Auction of auction_event
  | `Change of Change_event.t
  | `Trade of trade_event ]
[@@deriving sexp, of_yojson]


(** The type of a market data update message. *)
type update = {
  event_id : Int_number.t;
  events : event array;
  timestamp : Timestamp.sec option;
  timestampms : Timestamp.ms option;
} [@@deriving sexp, of_yojson]


(** The type of a market data message- a heartbeat or update. *)
type message =
  [ `Heartbeat of heartbeat | `Update of update ]
[@@deriving sexp]


(** The type of a market data response. *)
type response = {
  socket_sequence : Int_number.t;
  message : message;
} [@@deriving sexp]

include Ws.CHANNEL
  with type uri_args = Symbol.t
  with type query = unit
  with type response := response

val client :
  (module Cfg.S) ->
  ?query:Sexp.t sexp_list ->
  ?uri_args:uri_args -> unit ->
  response Pipe.Reader.t Deferred.t

val command : string * Command.t
