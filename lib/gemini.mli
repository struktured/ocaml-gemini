module Auth = Auth
module Result = Json.Result

type int_number = int64 [@encoding `number] [@@deriving sexp, yojson]
type int_string = int64 [@encoding `string] [@@deriving sexp, yojson]
type decimal_number = float [@encoding `number] [@@deriving sexp, yojson]
type decimal_string = string [@@deriving sexp, yojson]


module V1 : sig
  val path : string list
  module Heartbeat :
    sig
      type request = unit [@@deriving sexp, yojson]
      type response = { result : bool; } [@@deriving sexp, yojson]
      val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
        | Rest.Error.post
        | `Ok of response
        ] Deferred.t
    end
  module Timestamp :
    sig
      type t = Time.t [@@deriving sexp, yojson]
      type ms = t [@@deriving sexp, yojson]
      type sec = t [@@deriving sexp, yojson]
    end
  module Currency :
    sig
      type t =
        [ `Btc | `Eth | `Usd ] [@@deriving sexp, yojson, enumerate]
      val to_string : [< t] -> string
    end
  module Symbol :
    sig
      type t =
        [ `Btcusd | `Ethbtc | `Ethusd ] [@@deriving sexp, yojson, enumerate]
      val to_string : [< t] -> string
    end
  module Exchange :
    sig
      type t = [ `Gemini ] [@@deriving sexp, yojson, enumerate]
      val to_string : [< t] -> string
    end
  module Side :
    sig
      type t = [ `Buy | `Sell ] [@@deriving sexp, yojson, enumerate]
      val to_string : [< t] -> string
    end
  module Order_type :
    sig
      type t = [ `Exchange_limit ] [@@deriving sexp, yojson, enumerate]
      val to_string : [< t] -> string
    end
  module Order_execution_option :
    sig
      type t =
        [ `Auction_only | `Immediate_or_cancel | `Maker_or_cancel ]
      [@@deriving sexp, yojson, enumerate]
      val to_string : [< t] -> string
    end
  module Order :
    sig
      val name : string
      val path : string list
      module Status :
      sig
        val name : string
        val path : string list
        type request =
          { order_id : int_number; } [@@deriving sexp, yojson]
        type response = {
          client_order_id : string option;
          order_id : int_string;
          id : int_string;
          symbol : Symbol.t;
          exchange : Exchange.t;
          avg_execution_price : decimal_string;
          side : Side.t;
          type_ : Order_type.t;
          timestamp : Timestamp.sec;
          timestampms : Timestamp.ms;
          is_live : bool;
          is_cancelled : bool;
          is_hidden : bool;
          was_forced : bool;
          executed_amount : decimal_string;
          remaining_amount : decimal_string;
          options : Order_execution_option.t list;
          price : decimal_string;
          original_amount : decimal_string;
        } [@@deriving sexp, yojson]
        val post :
          (module Cfg.S) ->
          Nonce.reader ->
          request ->
          [
            | Rest.Error.post
            | `Ok of response
          ] Deferred.t
        val command : string * Command.t
      end
      module New :
        sig
          val name : string
          val path : string list
          type request = {
            client_order_id : string;
            symbol : Symbol.t;
            amount : decimal_string;
            price : decimal_string;
            side : Side.t;
            type_ : Order_type.t;
            options : Order_execution_option.t list;
          } [@@deriving sexp, yojson]
          type response = Status.response [@@deriving sexp, yojson]
          val post :
            (module Cfg.S) ->
            Nonce.reader ->
            request ->
            [
              | Rest.Error.post
              | `Ok of response
            ] Deferred.t
          val command : string * Command.t
        end
      module Cancel :
        sig
          val name : string
          val path : string list
          module By_order_id :
            sig
              val name : string
              val path : string list
              type request =
                { order_id : int_string; } [@@deriving sexp, yojson]
              type response = Status.response [@@deriving sexp, yojson]
              val post :
                (module Cfg.S) ->
                Nonce.reader ->
                request ->
                [
                  | Rest.Error.post
                  | `Ok of response
                ] Deferred.t
              val command : string * Command.t
            end
          type details = {
            cancelled_orders : Status.response list;
            cancel_rejects : Status.response list;
          } [@@deriving sexp, yojson]
          module All :
            sig
              val name : string
              val path : string list
              type request = unit [@@deriving sexp, yojson]
              type response =
                { details : details; } [@@deriving sexp, yojson]
              val post :
                (module Cfg.S) ->
                Nonce.reader ->
                request ->
                [
                  | Rest.Error.post
                  | `Ok of response
                ] Deferred.t
              val command : string * Command.t
            end
          module Session :
            sig
              val name : string
              val path : string list
              type request = unit [@@deriving sexp, yojson]
              type response = { details : details; } [@@deriving sexp, yojson]
              val post :
                (module Cfg.S) ->
                Nonce.reader ->
                request ->
                [
                  | Rest.Error.post
                  | `Ok of response
                ] Deferred.t
              val command : string * Command.t
            end
          val command : string * Command.t
        end
      val command : string * Command.t
    end
  module Orders :
    sig
      val name : string
      val path : string list
      type request = unit [@@deriving sexp, yojson]
      type response = Order.Status.response list [@@deriving sexp, yojson]
      val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
          | Rest.Error.post
          | `Ok of response
        ] Deferred.t
      val command : string * Command.t
    end
  module Mytrades :
    sig
      type trade = {
        price : decimal_string;
        amount : decimal_string;
        timestamp : Timestamp.sec;
        timestampms : Timestamp.ms;
        type_ : Side.t;
        aggressor : bool;
        fee_currency : Currency.t;
        fee_amount : decimal_string;
        tid : int_number;
        order_id : int_string;
        client_order_id : string option;
        is_auction_fill : bool;
        exchange : Exchange.t;
      } [@@deriving sexp, yojson]
      val name : string
      val path : string list
      type request = {
        symbol : Symbol.t;
        limit_trades : int option;
        timestamp : Timestamp.sec option;
      } [@@deriving sexp, yojson]
      type response = trade list [@@deriving sexp, yojson]
      val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
          | Rest.Error.post
          | `Ok of response
        ] Deferred.t
      val command : string * Command.t
    end
  module Tradevolume :
    sig
      type volume = {
        account_id : int_number;
        symbol : Symbol.t;
        base_currency : Currency.t;
        notional_currency : Currency.t;
        data_date : string;
        total_volume_base : decimal_number;
        maker_buy_sell_ratio : decimal_number;
        buy_maker_base : decimal_number;
        buy_maker_notional : decimal_number;
        buy_maker_count : int_number;
        sell_maker_base : decimal_number;
        sell_maker_notional : decimal_number;
        sell_maker_count : int_number;
        buy_taker_base : decimal_number;
        buy_taker_notional : decimal_number;
        buy_taker_count : int_number;
        sell_taker_base : decimal_number;
        sell_taker_notional : decimal_number;
        sell_taker_count : int_number;
      } [@@deriving sexp, yojson]
      val name : string
      val path : string list
      type request = unit [@@deriving sexp, yojson]
      type response = volume list list [@@deriving sexp, yojson]
      val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
          | Rest.Error.post
          | `Ok of response
        ] Deferred.t
      val command : string * Command.t
    end
  module Balances :
    sig
      val name : string
      val path : string list
      type request = unit [@@deriving sexp, yojson]
      type balance =
      {
        currency : Currency.t;
        amount : decimal_string;
        available : decimal_string;
        available_for_withdrawal : decimal_string;
        type_ : string;
      } [@@deriving sexp, yojson]
      type response = balance list [@@deriving sexp, yojson]
      val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
          | Rest.Error.post
          | `Ok of response
        ] Deferred.t
      val command : string * Command.t
    end
  module Market_data :
    sig
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
      val uri_args_to_string : [< Symbol.t ] -> string

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

      type heartbeat = unit [@@deriving sexp, yojson]

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
      } [@@deriving sexp, yojson]

      type trade_event = {
        tid : int_number;
        price : decimal_string;
        amount : decimal_string;
        maker_side : Side.t;
      } [@@deriving sexp, yojson]

      type auction_open_event = {
        auction_open_ms : Timestamp.ms;
        auction_time_ms : Timestamp.ms;
        first_indicative_ms : Timestamp.ms;
        last_cancel_time_ms : Timestamp.ms;
      } [@@deriving sexp, yojson]

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
      } [@@deriving sexp, yojson]

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
      [@@deriving sexp, yojson]

      type event =
          [ `Auction of auction_event
          | `Change of change_event
          | `Trade of trade_event ]
      [@@deriving sexp, yojson]

      type update = {
        event_id : int_number;
        events : event array;
        timestamp : Timestamp.sec option;
        timestampms : Timestamp.ms option;
      } [@@deriving sexp, yojson]

      type message =
        [ `Heartbeat of heartbeat | `Update of update ]
      [@@deriving sexp]

      type response = {
        socket_sequence : int_number;
        message : message;
      } [@@deriving sexp, yojson]

      val client :
        (module Cfg.S) ->
        ?protocol:string ->
        ?extensions:string ->
        uri_args -> (unit * unit) Deferred.t
      val handle_client :
        [< Socket.Address.t ] ->
        Reader.t -> Writer.t -> unit Deferred.t
      val command : string * Command.t
    end
  val command : Command.t
end
