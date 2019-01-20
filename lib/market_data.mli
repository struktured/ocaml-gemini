open! Common
module Side :
sig
  module Bid_ask :
  sig
    type t = [ `Ask | `Bid ] [@@deriving sexp, enumerate, yojson]
    val to_string : [< t ] -> string
  end
  module Auction :
  sig
    type t = [ `Auction ] [@@deriving sexp, enumerate, yojson]
    val to_string : [< t ] -> string
  end

  type t = [ `Ask | `Auction | `Bid ]
  [@@deriving sexp, enumerate, yojson]

  val to_string : [< t ] -> string
end
val name : string
val path : string list
type uri_args = Symbol.t [@@deriving yojson, sexp]
val encode_uri_args : [< Symbol.t ] -> string

type request = { heartbeat : bool option; } [@@deriving sexp, yojson]
module Message_type :
sig
  type t = [`Heartbeat | `Update]
  [@@deriving sexp, yojson, enumerate]
  val to_string : [< t ] -> string
end

module Event_type :
sig
  type t = [`Trade | `Change | `Auction]
  [@@deriving sexp, yojson, enumerate]
  val to_string : [< t ] -> string
end

type heartbeat = unit [@@deriving sexp, of_yojson]

module Reason : sig
  type t =
    [`Place | `Trade | `Cancel | `Initial]
  [@@deriving sexp, yojson, enumerate]
  val to_string : [< t ] -> string
end

type change_event = {
  price : decimal_string;
  side : Side.Bid_ask.t;
  reason : Reason.t;
  remaining : decimal_string;
  delta : decimal_string;
} [@@deriving sexp, of_yojson]

type trade_event = {
  tid : int_number;
  price : decimal_string;
  amount : decimal_string;
  maker_side : Side.t;
} [@@deriving sexp, of_yojson]

type auction_open_event = {
  auction_open_ms : Timestamp.ms;
  auction_time_ms : Timestamp.ms;
  first_indicative_ms : Timestamp.ms;
  last_cancel_time_ms : Timestamp.ms;
} [@@deriving sexp, of_yojson]

module Auction_result :
sig
  type t =
    [`Success | `Failure] [@@deriving sexp, enumerate, yojson]
  val to_string : [<t] -> string
end

type auction_indicative_price_event = {
  eid : int_number;
  result : Auction_result.t;
  time_ms : Timestamp.ms;
  highest_bid_price : decimal_string;
  lowest_ask_price : decimal_string;
  collar_price : decimal_string;
  indicative_price : decimal_string;
  indicative_quantity : decimal_string;
}

type auction_outcome_event = {
  eid : int_number;
  result : Auction_result.t;
  time_ms : Timestamp.ms;
  highest_bid_price : decimal_string;
  lowest_ask_price : decimal_string;
  collar_price : decimal_string;
  auction_price : decimal_string;
  auction_quantity : decimal_string;
} [@@deriving sexp, of_yojson]

module Auction_event_type :
sig
  type t =
    [ `Auction_open
    | `Auction_indicative_price
    | `Auction_outcome ]
  [@@deriving sexp, enumerate, yojson]
  val to_string : [<t] -> string
end

type auction_event =
  [ `Auction_indicative_price of auction_indicative_price_event
  | `Auction_open of auction_open_event
  | `Auction_outcome of auction_outcome_event ]
[@@deriving sexp, of_yojson]

type event =
  [ `Auction of auction_event
  | `Change of change_event
  | `Trade of trade_event ]
[@@deriving sexp, of_yojson]

type update = {
  event_id : int_number;
  events : event array;
  timestamp : Timestamp.sec option;
  timestampms : Timestamp.ms option;
} [@@deriving sexp, of_yojson]

type message =
  [ `Heartbeat of heartbeat | `Update of update ]
[@@deriving sexp]

type response = {
  socket_sequence : int_number;
  message : message;
} [@@deriving sexp, of_yojson]


val client :
  (module Cfg.S) ->
  ?query:Sexp.t sexp_list ->
  ?uri_args:uri_args -> unit ->
  response Pipe.Reader.t Deferred.t

val command : string * Command.t
