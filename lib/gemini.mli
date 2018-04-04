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
      type t = Core.Time.t [@@deriving sexp, yojson]
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
          module T :
            sig
              val name : string
              val path : string list
              type request = { order_id : int_number; }
              val request_to_yojson : request -> Yojson.Safe.json
              val request_of_yojson :
                Yojson.Safe.json ->
                request Ppx_deriving_yojson_runtime.error_or
              val request_of_sexp : Sexplib.Sexp.t -> request
              val sexp_of_request : request -> Sexplib.Sexp.t
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
              }
              val response_to_yojson : response -> Yojson.Safe.json
              val response_of_yojson :
                Yojson.Safe.json ->
                response Ppx_deriving_yojson_runtime.error_or
              val response_of_sexp : Sexplib.Sexp.t -> response
              val sexp_of_response : response -> Sexplib.Sexp.t
            end
          val name : string
          val path : string list
          type request = T.request = { order_id : int_number; }
          val request_to_yojson : request -> Yojson.Safe.json
          val request_of_yojson :
            Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
          val request_of_sexp : Sexplib.Sexp.t -> request
          val sexp_of_request : request -> Sexplib.Sexp.t
          type response =
            T.response = {
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
          }
          val response_to_yojson : response -> Yojson.Safe.json
          val response_of_yojson :
            Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
          val response_of_sexp : Sexplib.Sexp.t -> response
          val sexp_of_response : response -> Sexplib.Sexp.t
          val post :
            (module Gemini__.Cfg.S) ->
            Gemini__.Nonce.reader ->
            T.request ->
            [ `Bad_request of string
            | `Error of Gemini__Rest.Error.detail
            | `Json_parse_error of Gemini__Rest.Error.json_error
            | `Not_acceptable of string
            | `Not_found
            | `Ok of T.response
            | `Unauthorized of string ] Async.Deferred.t
          val command : string * Async.Command.t
        end
      module New :
        sig
          module T :
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
              }
              val request_to_yojson : request -> Yojson.Safe.json
              val request_of_yojson :
                Yojson.Safe.json ->
                request Ppx_deriving_yojson_runtime.error_or
              val request_of_sexp : Sexplib.Sexp.t -> request
              val sexp_of_request : request -> Sexplib.Sexp.t
              type response = Status.response
              val response_to_yojson : response -> Yojson.Safe.json
              val response_of_yojson :
                Yojson.Safe.json ->
                response Ppx_deriving_yojson_runtime.error_or
              val response_of_sexp : Sexplib.Sexp.t -> response
              val sexp_of_response : response -> Sexplib.Sexp.t
            end
          val name : string
          val path : string list
          type request =
            T.request = {
            client_order_id : string;
            symbol : Symbol.t;
            amount : decimal_string;
            price : decimal_string;
            side : Side.t;
            type_ : Order_type.t;
            options : Order_execution_option.t list;
          }
          val request_to_yojson : request -> Yojson.Safe.json
          val request_of_yojson :
            Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
          val request_of_sexp : Sexplib.Sexp.t -> request
          val sexp_of_request : request -> Sexplib.Sexp.t
          type response = Status.response
          val response_to_yojson : response -> Yojson.Safe.json
          val response_of_yojson :
            Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
          val response_of_sexp : Sexplib.Sexp.t -> response
          val sexp_of_response : response -> Sexplib.Sexp.t
          val post :
            (module Gemini__.Cfg.S) ->
            Gemini__.Nonce.reader ->
            T.request ->
            [ `Bad_request of string
            | `Error of Gemini__Rest.Error.detail
            | `Json_parse_error of Gemini__Rest.Error.json_error
            | `Not_acceptable of string
            | `Not_found
            | `Ok of T.response
            | `Unauthorized of string ] Async.Deferred.t
          val command : string * Async.Command.t
        end
      module Cancel :
        sig
          val name : string
          val path : string list
          module By_order_id :
            sig
              module T :
                sig
                  val name : string
                  val path : string list
                  type request = { order_id : int_string; }
                  val request_to_yojson : request -> Yojson.Safe.json
                  val request_of_yojson :
                    Yojson.Safe.json ->
                    request Ppx_deriving_yojson_runtime.error_or
                  val request_of_sexp : Sexplib.Sexp.t -> request
                  val sexp_of_request : request -> Sexplib.Sexp.t
                  type response = Status.response
                  val response_to_yojson : response -> Yojson.Safe.json
                  val response_of_yojson :
                    Yojson.Safe.json ->
                    response Ppx_deriving_yojson_runtime.error_or
                  val response_of_sexp : Sexplib.Sexp.t -> response
                  val sexp_of_response : response -> Sexplib.Sexp.t
                end
              val name : string
              val path : string list
              type request = T.request = { order_id : int_string; }
              val request_to_yojson : request -> Yojson.Safe.json
              val request_of_yojson :
                Yojson.Safe.json ->
                request Ppx_deriving_yojson_runtime.error_or
              val request_of_sexp : Sexplib.Sexp.t -> request
              val sexp_of_request : request -> Sexplib.Sexp.t
              type response = Status.response
              val response_to_yojson : response -> Yojson.Safe.json
              val response_of_yojson :
                Yojson.Safe.json ->
                response Ppx_deriving_yojson_runtime.error_or
              val response_of_sexp : Sexplib.Sexp.t -> response
              val sexp_of_response : response -> Sexplib.Sexp.t
              val post :
                (module Gemini__.Cfg.S) ->
                Gemini__.Nonce.reader ->
                T.request ->
                [ `Bad_request of string
                | `Error of Gemini__Rest.Error.detail
                | `Json_parse_error of Gemini__Rest.Error.json_error
                | `Not_acceptable of string
                | `Not_found
                | `Ok of T.response
                | `Unauthorized of string ] Async.Deferred.t
              val command : string * Async.Command.t
            end
          type details = {
            cancelled_orders : Status.response list;
            cancel_rejects : Status.response list;
          }
          val details_to_yojson : details -> Yojson.Safe.json
          val details_of_yojson :
            Yojson.Safe.json -> details Ppx_deriving_yojson_runtime.error_or
          val details_of_sexp : Sexplib.Sexp.t -> details
          val sexp_of_details : details -> Sexplib.Sexp.t
          module All :
            sig
              module T :
                sig
                  val name : string
                  val path : string list
                  type request = unit
                  val request_to_yojson : request -> Yojson.Safe.json
                  val request_of_yojson :
                    Yojson.Safe.json ->
                    request Ppx_deriving_yojson_runtime.error_or
                  val request_of_sexp : Sexplib.Sexp.t -> request
                  val sexp_of_request : request -> Sexplib.Sexp.t
                  type response = { details : details; }
                  val response_to_yojson : response -> Yojson.Safe.json
                  val response_of_yojson :
                    Yojson.Safe.json ->
                    response Ppx_deriving_yojson_runtime.error_or
                  val response_of_sexp : Sexplib.Sexp.t -> response
                  val sexp_of_response : response -> Sexplib.Sexp.t
                end
              val name : string
              val path : string list
              type request = unit
              val request_to_yojson : request -> Yojson.Safe.json
              val request_of_yojson :
                Yojson.Safe.json ->
                request Ppx_deriving_yojson_runtime.error_or
              val request_of_sexp : Sexplib.Sexp.t -> request
              val sexp_of_request : request -> Sexplib.Sexp.t
              type response = T.response = { details : details; }
              val response_to_yojson : response -> Yojson.Safe.json
              val response_of_yojson :
                Yojson.Safe.json ->
                response Ppx_deriving_yojson_runtime.error_or
              val response_of_sexp : Sexplib.Sexp.t -> response
              val sexp_of_response : response -> Sexplib.Sexp.t
              val post :
                (module Gemini__.Cfg.S) ->
                Gemini__.Nonce.reader ->
                T.request ->
                [ `Bad_request of string
                | `Error of Gemini__Rest.Error.detail
                | `Json_parse_error of Gemini__Rest.Error.json_error
                | `Not_acceptable of string
                | `Not_found
                | `Ok of T.response
                | `Unauthorized of string ] Async.Deferred.t
              val command : string * Async.Command.t
            end
          module Session :
            sig
              module T :
                sig
                  val name : string
                  val path : string list
                  type request = unit
                  val request_to_yojson : request -> Yojson.Safe.json
                  val request_of_yojson :
                    Yojson.Safe.json ->
                    request Ppx_deriving_yojson_runtime.error_or
                  val request_of_sexp : Sexplib.Sexp.t -> request
                  val sexp_of_request : request -> Sexplib.Sexp.t
                  type response = { details : details; }
                  val response_to_yojson : response -> Yojson.Safe.json
                  val response_of_yojson :
                    Yojson.Safe.json ->
                    response Ppx_deriving_yojson_runtime.error_or
                  val response_of_sexp : Sexplib.Sexp.t -> response
                  val sexp_of_response : response -> Sexplib.Sexp.t
                end
              val name : string
              val path : string list
              type request = unit
              val request_to_yojson : request -> Yojson.Safe.json
              val request_of_yojson :
                Yojson.Safe.json ->
                request Ppx_deriving_yojson_runtime.error_or
              val request_of_sexp : Sexplib.Sexp.t -> request
              val sexp_of_request : request -> Sexplib.Sexp.t
              type response = T.response = { details : details; }
              val response_to_yojson : response -> Yojson.Safe.json
              val response_of_yojson :
                Yojson.Safe.json ->
                response Ppx_deriving_yojson_runtime.error_or
              val response_of_sexp : Sexplib.Sexp.t -> response
              val sexp_of_response : response -> Sexplib.Sexp.t
              val post :
                (module Gemini__.Cfg.S) ->
                Gemini__.Nonce.reader ->
                T.request ->
                [ `Bad_request of string
                | `Error of Gemini__Rest.Error.detail
                | `Json_parse_error of Gemini__Rest.Error.json_error
                | `Not_acceptable of string
                | `Not_found
                | `Ok of T.response
                | `Unauthorized of string ] Async.Deferred.t
              val command : string * Async.Command.t
            end
          val command : string * Async.Command.t
        end
      val command : string * Async.Command.t
    end
  module Orders :
    sig
      module T :
        sig
          val name : string
          val path : string list
          type request = unit
          val request_to_yojson : request -> Yojson.Safe.json
          val request_of_yojson :
            Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
          val request_of_sexp : Sexplib.Sexp.t -> request
          val sexp_of_request : request -> Sexplib.Sexp.t
          type response = Order.Status.response list
          val response_to_yojson : response -> Yojson.Safe.json
          val response_of_yojson :
            Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
          val response_of_sexp : Sexplib.Sexp.t -> response
          val sexp_of_response : response -> Sexplib.Sexp.t
        end
      val name : string
      val path : string list
      type request = unit
      val request_to_yojson : request -> Yojson.Safe.json
      val request_of_yojson :
        Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
      val request_of_sexp : Sexplib.Sexp.t -> request
      val sexp_of_request : request -> Sexplib.Sexp.t
      type response = Order.Status.response list
      val response_to_yojson : response -> Yojson.Safe.json
      val response_of_yojson :
        Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
      val response_of_sexp : Sexplib.Sexp.t -> response
      val sexp_of_response : response -> Sexplib.Sexp.t
      val post :
        (module Gemini__.Cfg.S) ->
        Gemini__.Nonce.reader ->
        T.request ->
        [ `Bad_request of string
        | `Error of Gemini__Rest.Error.detail
        | `Json_parse_error of Gemini__Rest.Error.json_error
        | `Not_acceptable of string
        | `Not_found
        | `Ok of T.response
        | `Unauthorized of string ] Async.Deferred.t
      val command : string * Async.Command.t
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
      }
      val trade_to_yojson : trade -> Yojson.Safe.json
      val trade_of_yojson :
        Yojson.Safe.json -> trade Ppx_deriving_yojson_runtime.error_or
      val trade_of_sexp : Sexplib.Sexp.t -> trade
      val sexp_of_trade : trade -> Sexplib.Sexp.t
      module T :
        sig
          val name : string
          val path : string list
          type request = {
            symbol : Symbol.t;
            limit_trades : int option;
            timestamp : Timestamp.sec option;
          }
          val request_to_yojson : request -> Yojson.Safe.json
          val request_of_yojson :
            Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
          val request_of_sexp : Sexplib.Sexp.t -> request
          val sexp_of_request : request -> Sexplib.Sexp.t
          type response = trade list
          val response_to_yojson : response -> Yojson.Safe.json
          val response_of_yojson :
            Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
          val response_of_sexp : Sexplib.Sexp.t -> response
          val sexp_of_response : response -> Sexplib.Sexp.t
        end
      val name : string
      val path : string list
      type request =
        T.request = {
        symbol : Symbol.t;
        limit_trades : int option;
        timestamp : Timestamp.sec option;
      }
      val request_to_yojson : request -> Yojson.Safe.json
      val request_of_yojson :
        Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
      val request_of_sexp : Sexplib.Sexp.t -> request
      val sexp_of_request : request -> Sexplib.Sexp.t
      type response = trade list
      val response_to_yojson : response -> Yojson.Safe.json
      val response_of_yojson :
        Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
      val response_of_sexp : Sexplib.Sexp.t -> response
      val sexp_of_response : response -> Sexplib.Sexp.t
      val post :
        (module Gemini__.Cfg.S) ->
        Gemini__.Nonce.reader ->
        T.request ->
        [ `Bad_request of string
        | `Error of Gemini__Rest.Error.detail
        | `Json_parse_error of Gemini__Rest.Error.json_error
        | `Not_acceptable of string
        | `Not_found
        | `Ok of T.response
        | `Unauthorized of string ] Async.Deferred.t
      val command : string * Async.Command.t
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
      }
      val volume_to_yojson : volume -> Yojson.Safe.json
      val volume_of_yojson :
        Yojson.Safe.json -> volume Ppx_deriving_yojson_runtime.error_or
      val volume_of_sexp : Sexplib.Sexp.t -> volume
      val sexp_of_volume : volume -> Sexplib.Sexp.t
      module T :
        sig
          val name : string
          val path : string list
          type request = unit
          val request_to_yojson : request -> Yojson.Safe.json
          val request_of_yojson :
            Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
          val request_of_sexp : Sexplib.Sexp.t -> request
          val sexp_of_request : request -> Sexplib.Sexp.t
          type response = volume list list
          val response_to_yojson : response -> Yojson.Safe.json
          val response_of_yojson :
            Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
          val response_of_sexp : Sexplib.Sexp.t -> response
          val sexp_of_response : response -> Sexplib.Sexp.t
        end
      val name : string
      val path : string list
      type request = unit
      val request_to_yojson : request -> Yojson.Safe.json
      val request_of_yojson :
        Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
      val request_of_sexp : Sexplib.Sexp.t -> request
      val sexp_of_request : request -> Sexplib.Sexp.t
      type response = volume list list
      val response_to_yojson : response -> Yojson.Safe.json
      val response_of_yojson :
        Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
      val response_of_sexp : Sexplib.Sexp.t -> response
      val sexp_of_response : response -> Sexplib.Sexp.t
      val post :
        (module Gemini__.Cfg.S) ->
        Gemini__.Nonce.reader ->
        T.request ->
        [ `Bad_request of string
        | `Error of Gemini__Rest.Error.detail
        | `Json_parse_error of Gemini__Rest.Error.json_error
        | `Not_acceptable of string
        | `Not_found
        | `Ok of T.response
        | `Unauthorized of string ] Async.Deferred.t
      val command : string * Async.Command.t
    end
  module Balances :
    sig
      module T :
        sig
          val name : string
          val path : string list
          type request = unit
          val request_to_yojson : request -> Yojson.Safe.json
          val request_of_yojson :
            Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
          val request_of_sexp : Sexplib.Sexp.t -> request
          val sexp_of_request : request -> Sexplib.Sexp.t
          type balance = {
            currency : Currency.t;
            amount : decimal_string;
            available : decimal_string;
            available_for_withdrawal : decimal_string;
            type_ : string;
          }
          val balance_to_yojson : balance -> Yojson.Safe.json
          val balance_of_yojson :
            Yojson.Safe.json -> balance Ppx_deriving_yojson_runtime.error_or
          val balance_of_sexp : Sexplib.Sexp.t -> balance
          val sexp_of_balance : balance -> Sexplib.Sexp.t
          type response = balance list
          val response_to_yojson : response -> Yojson.Safe.json
          val response_of_yojson :
            Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
          val response_of_sexp : Sexplib.Sexp.t -> response
          val sexp_of_response : response -> Sexplib.Sexp.t
        end
      val name : string
      val path : string list
      type request = unit
      val request_to_yojson : request -> Yojson.Safe.json
      val request_of_yojson :
        Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
      val request_of_sexp : Sexplib.Sexp.t -> request
      val sexp_of_request : request -> Sexplib.Sexp.t
      type balance =
        T.balance = {
        currency : Currency.t;
        amount : decimal_string;
        available : decimal_string;
        available_for_withdrawal : decimal_string;
        type_ : string;
      }
      val balance_to_yojson : balance -> Yojson.Safe.json
      val balance_of_yojson :
        Yojson.Safe.json -> balance Ppx_deriving_yojson_runtime.error_or
      val balance_of_sexp : Sexplib.Sexp.t -> balance
      val sexp_of_balance : balance -> Sexplib.Sexp.t
      type response = balance list
      val response_to_yojson : response -> Yojson.Safe.json
      val response_of_yojson :
        Yojson.Safe.json -> response Ppx_deriving_yojson_runtime.error_or
      val response_of_sexp : Sexplib.Sexp.t -> response
      val sexp_of_response : response -> Sexplib.Sexp.t
      val post :
        (module Gemini__.Cfg.S) ->
        Gemini__.Nonce.reader ->
        T.request ->
        [ `Bad_request of string
        | `Error of Gemini__Rest.Error.detail
        | `Json_parse_error of Gemini__Rest.Error.json_error
        | `Not_acceptable of string
        | `Not_found
        | `Ok of T.response
        | `Unauthorized of string ] Async.Deferred.t
      val command : string * Async.Command.t
    end
  module Market_data :
    sig
      module Side :
        sig
          module Bid_ask :
            sig
              module T :
                sig
                  type t = [ `Ask | `Bid ]
                  val __t_of_sexp__ : Sexplib.Sexp.t -> t
                  val t_of_sexp : Sexplib.Sexp.t -> t
                  val sexp_of_t : t -> Sexplib.Sexp.t
                  val all : t list
                  val to_string : [< t ] -> string
                end
              type t = [ `Ask | `Bid ]
              val __t_of_sexp__ : Sexplib.Sexp.t -> t
              val t_of_sexp : Sexplib.Sexp.t -> t
              val sexp_of_t : t -> Sexplib.Sexp.t
              val all : t list
              val to_string : [< t ] -> string
              val dict : (string * T.t) Core.List.t
              val of_string : string -> T.t option
              val to_yojson : T.t -> [> `String of string ]
              val of_yojson :
                Yojson.Safe.json -> (T.t, string) Gemini__Json.Result.t
            end
          module Auction :
            sig
              module T :
                sig
                  type t = [ `Auction ]
                  val __t_of_sexp__ : Sexplib.Sexp.t -> t
                  val t_of_sexp : Sexplib.Sexp.t -> t
                  val sexp_of_t : t -> Sexplib.Sexp.t
                  val all : t list
                  val to_string : [< `Auction ] -> string
                end
              type t = [ `Auction ]
              val __t_of_sexp__ : Sexplib.Sexp.t -> t
              val t_of_sexp : Sexplib.Sexp.t -> t
              val sexp_of_t : t -> Sexplib.Sexp.t
              val all : t list
              val to_string : [< `Auction ] -> string
              val dict : (string * T.t) Core.List.t
              val of_string : string -> T.t option
              val to_yojson : T.t -> [> `String of string ]
              val of_yojson :
                Yojson.Safe.json -> (T.t, string) Gemini__Json.Result.t
            end
          module T :
            sig
              type t = [ `Ask | `Auction | `Bid ]
              val __t_of_sexp__ : Sexplib.Sexp.t -> t
              val t_of_sexp : Sexplib.Sexp.t -> t
              val sexp_of_t : t -> Sexplib.Sexp.t
              val all : t list
              val to_string : [< t ] -> string
            end
          type t = [ `Ask | `Auction | `Bid ]
          val __t_of_sexp__ : Sexplib.Sexp.t -> t
          val t_of_sexp : Sexplib.Sexp.t -> t
          val sexp_of_t : t -> Sexplib.Sexp.t
          val all : t list
          val to_string : [< t ] -> string
          val dict : (string * T.t) Core.List.t
          val of_string : string -> T.t option
          val to_yojson : T.t -> [> `String of string ]
          val of_yojson :
            Yojson.Safe.json -> (T.t, string) Gemini__Json.Result.t
        end
      module T :
        sig
          val name : string
          val path : string list
          type uri_args = Symbol.t
          val uri_args_to_yojson : uri_args -> Yojson.Safe.json
          val uri_args_of_yojson :
            Yojson.Safe.json -> uri_args Ppx_deriving_yojson_runtime.error_or
          val uri_args_of_sexp : Sexplib.Sexp.t -> uri_args
          val sexp_of_uri_args : uri_args -> Sexplib.Sexp.t
          val all_of_uri_args : uri_args list
          val uri_args_to_string : [< `Btcusd | `Ethbtc | `Ethusd ] -> string
          type request = { heartbeat : bool option; }
          val request_to_yojson : request -> Yojson.Safe.json
          val request_of_yojson :
            Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
          val request_of_sexp : Sexplib.Sexp.t -> request
          val sexp_of_request : request -> Sexplib.Sexp.t
          module Message_type :
            sig
              module T :
                sig
                  type t = [ `Heartbeat | `Update ]
                  val __t_of_sexp__ : Sexplib.Sexp.t -> t
                  val t_of_sexp : Sexplib.Sexp.t -> t
                  val sexp_of_t : t -> Sexplib.Sexp.t
                  val all : t list
                  val to_string : [< `Heartbeat | `Update ] -> string
                end
              type t = [ `Heartbeat | `Update ]
              val __t_of_sexp__ : Sexplib.Sexp.t -> t
              val t_of_sexp : Sexplib.Sexp.t -> t
              val sexp_of_t : t -> Sexplib.Sexp.t
              val all : t list
              val to_string : [< `Heartbeat | `Update ] -> string
              val dict : (string * T.t) Core.List.t
              val of_string : string -> T.t option
              val to_yojson : T.t -> [> `String of string ]
              val of_yojson :
                Yojson.Safe.json -> (T.t, string) Gemini__Json.Result.t
            end
          module Event_type :
            sig
              module T :
                sig
                  type t = [ `Auction | `Change | `Trade ]
                  val __t_of_sexp__ : Sexplib.Sexp.t -> t
                  val t_of_sexp : Sexplib.Sexp.t -> t
                  val sexp_of_t : t -> Sexplib.Sexp.t
                  val all : t list
                  val to_string : [< `Auction | `Change | `Trade ] -> string
                end
              type t = [ `Auction | `Change | `Trade ]
              val __t_of_sexp__ : Sexplib.Sexp.t -> t
              val t_of_sexp : Sexplib.Sexp.t -> t
              val sexp_of_t : t -> Sexplib.Sexp.t
              val all : t list
              val to_string : [< `Auction | `Change | `Trade ] -> string
              val dict : (string * T.t) Core.List.t
              val of_string : string -> T.t option
              val to_yojson : T.t -> [> `String of string ]
              val of_yojson :
                Yojson.Safe.json -> (T.t, string) Gemini__Json.Result.t
            end
          type heartbeat = unit
          val heartbeat_to_yojson : heartbeat -> Yojson.Safe.json
          val heartbeat_of_yojson :
            Yojson.Safe.json ->
            heartbeat Ppx_deriving_yojson_runtime.error_or
          val heartbeat_of_sexp : Sexplib.Sexp.t -> heartbeat
          val sexp_of_heartbeat : heartbeat -> Sexplib.Sexp.t
          module Reason :
            sig
              module T :
                sig
                  type t = [ `Cancel | `Initial | `Place | `Trade ]
                  val __t_of_sexp__ : Sexplib.Sexp.t -> t
                  val t_of_sexp : Sexplib.Sexp.t -> t
                  val sexp_of_t : t -> Sexplib.Sexp.t
                  val all : t list
                  val to_string :
                    [< `Cancel | `Initial | `Place | `Trade ] -> string
                end
              type t = [ `Cancel | `Initial | `Place | `Trade ]
              val __t_of_sexp__ : Sexplib.Sexp.t -> t
              val t_of_sexp : Sexplib.Sexp.t -> t
              val sexp_of_t : t -> Sexplib.Sexp.t
              val all : t list
              val to_string :
                [< `Cancel | `Initial | `Place | `Trade ] -> string
              val dict : (string * T.t) Core.List.t
              val of_string : string -> T.t option
              val to_yojson : T.t -> [> `String of string ]
              val of_yojson :
                Yojson.Safe.json -> (T.t, string) Gemini__Json.Result.t
            end
          type change_event = {
            price : decimal_string;
            side : Side.Bid_ask.t;
            reason : Reason.t;
            remaining : decimal_string;
            delta : decimal_string;
          }
          val change_event_to_yojson : change_event -> Yojson.Safe.json
          val change_event_of_yojson :
            Yojson.Safe.json ->
            change_event Ppx_deriving_yojson_runtime.error_or
          val change_event_of_sexp : Sexplib.Sexp.t -> change_event
          val sexp_of_change_event : change_event -> Sexplib.Sexp.t
          type trade_event = {
            tid : int_number;
            price : decimal_string;
            amount : decimal_string;
            maker_side : Side.t;
          }
          val trade_event_to_yojson : trade_event -> Yojson.Safe.json
          val trade_event_of_yojson :
            Yojson.Safe.json ->
            trade_event Ppx_deriving_yojson_runtime.error_or
          val trade_event_of_sexp : Sexplib.Sexp.t -> trade_event
          val sexp_of_trade_event : trade_event -> Sexplib.Sexp.t
          type auction_open_event = {
            auction_open_ms : Timestamp.ms;
            auction_time_ms : Timestamp.ms;
            first_indicative_ms : Timestamp.ms;
            last_cancel_time_ms : Timestamp.ms;
          }
          val auction_open_event_to_yojson :
            auction_open_event -> Yojson.Safe.json
          val auction_open_event_of_yojson :
            Yojson.Safe.json ->
            auction_open_event Ppx_deriving_yojson_runtime.error_or
          val auction_open_event_of_sexp :
            Sexplib.Sexp.t -> auction_open_event
          val sexp_of_auction_open_event :
            auction_open_event -> Sexplib.Sexp.t
          module Auction_result :
            sig
              module T :
                sig
                  type t = [ `Failure | `Success ]
                  val __t_of_sexp__ : Sexplib.Sexp.t -> t
                  val t_of_sexp : Sexplib.Sexp.t -> t
                  val sexp_of_t : t -> Sexplib.Sexp.t
                  val all : t list
                  val to_string : [< `Failure | `Success ] -> string
                end
              type t = [ `Failure | `Success ]
              val __t_of_sexp__ : Sexplib.Sexp.t -> t
              val t_of_sexp : Sexplib.Sexp.t -> t
              val sexp_of_t : t -> Sexplib.Sexp.t
              val all : t list
              val to_string : [< `Failure | `Success ] -> string
              val dict : (string * T.t) Core.List.t
              val of_string : string -> T.t option
              val to_yojson : T.t -> [> `String of string ]
              val of_yojson :
                Yojson.Safe.json -> (T.t, string) Gemini__Json.Result.t
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
          val auction_indicative_price_event_to_yojson :
            auction_indicative_price_event -> Yojson.Safe.json
          val auction_indicative_price_event_of_yojson :
            Yojson.Safe.json ->
            auction_indicative_price_event
            Ppx_deriving_yojson_runtime.error_or
          val auction_indicative_price_event_of_sexp :
            Sexplib.Sexp.t -> auction_indicative_price_event
          val sexp_of_auction_indicative_price_event :
            auction_indicative_price_event -> Sexplib.Sexp.t
          type auction_outcome_event = {
            eid : int_number;
            result : Auction_result.t;
            time_ms : Timestamp.ms;
            highest_bid_price : decimal_string;
            lowest_ask_price : decimal_string;
            collar_price : decimal_string;
            auction_price : decimal_string;
            auction_quantity : decimal_string;
          }
          val auction_outcome_event_to_yojson :
            auction_outcome_event -> Yojson.Safe.json
          val auction_outcome_event_of_yojson :
            Yojson.Safe.json ->
            auction_outcome_event Ppx_deriving_yojson_runtime.error_or
          val auction_outcome_event_of_sexp :
            Sexplib.Sexp.t -> auction_outcome_event
          val sexp_of_auction_outcome_event :
            auction_outcome_event -> Sexplib.Sexp.t
          module Auction_event_type :
            sig
              module T :
                sig
                  type t =
                      [ `Auction_indicative_price
                      | `Auction_open
                      | `Auction_outcome ]
                  val __t_of_sexp__ : Sexplib.Sexp.t -> t
                  val t_of_sexp : Sexplib.Sexp.t -> t
                  val sexp_of_t : t -> Sexplib.Sexp.t
                  val all : t list
                  val to_string :
                    [< `Auction_indicative_price
                     | `Auction_open
                     | `Auction_outcome ] ->
                    string
                end
              type t =
                  [ `Auction_indicative_price
                  | `Auction_open
                  | `Auction_outcome ]
              val __t_of_sexp__ : Sexplib.Sexp.t -> t
              val t_of_sexp : Sexplib.Sexp.t -> t
              val sexp_of_t : t -> Sexplib.Sexp.t
              val all : t list
              val to_string :
                [< `Auction_indicative_price
                 | `Auction_open
                 | `Auction_outcome ] ->
                string
              val dict : (string * T.t) Core.List.t
              val of_string : string -> T.t option
              val to_yojson : T.t -> [> `String of string ]
              val of_yojson :
                Yojson.Safe.json -> (T.t, string) Gemini__Json.Result.t
            end
          type auction_event =
              [ `Auction_indicative_price of auction_indicative_price_event
              | `Auction_open of auction_open_event
              | `Auction_outcome of auction_outcome_event ]
          val __auction_event_of_sexp__ : Sexplib.Sexp.t -> auction_event
          val auction_event_of_sexp : Sexplib.Sexp.t -> auction_event
          val sexp_of_auction_event : auction_event -> Sexplib.Sexp.t
          val auction_event_to_yojson : auction_event -> Yojson.Safe.json
          val auction_event_of_yojson :
            Yojson.Safe.json -> (auction_event, string) Result.t
          type event =
              [ `Auction of auction_event
              | `Change of change_event
              | `Trade of trade_event ]
          val __event_of_sexp__ : Sexplib.Sexp.t -> event
          val event_of_sexp : Sexplib.Sexp.t -> event
          val sexp_of_event : event -> Sexplib.Sexp.t
          val event_to_yojson : event -> Yojson.Safe.json
          val event_of_yojson : Yojson.Safe.json -> (event, string) Result.t
          type update = {
            event_id : int_number;
            events : event array;
            timestamp : Timestamp.sec option;
            timestampms : Timestamp.ms option;
          }
          val update_to_yojson : update -> Yojson.Safe.json
          val update_of_yojson :
            Yojson.Safe.json -> update Ppx_deriving_yojson_runtime.error_or
          val update_of_sexp : Sexplib.Sexp.t -> update
          val sexp_of_update : update -> Sexplib.Sexp.t
          type message = [ `Heartbeat of heartbeat | `Update of update ]
          val __message_of_sexp__ : Sexplib.Sexp.t -> message
          val message_of_sexp : Sexplib.Sexp.t -> message
          val sexp_of_message : message -> Sexplib.Sexp.t
          type response = {
            socket_sequence : int_number;
            message : message;
          }
          val response_of_sexp : Sexplib.Sexp.t -> response
          val sexp_of_response : response -> Sexplib.Sexp.t
          val response_to_yojson : response -> Yojson.Safe.json
          val response_of_yojson :
            Yojson.Safe.json -> (response, string) Result.t
        end
      val name : string
      val path : string list
      type uri_args = Symbol.t [@@deriving yojson, sexp]
      val uri_args_to_string : [< Symbol.t ] -> string
      type request = T.request = { heartbeat : bool option; }
      val request_to_yojson : request -> Yojson.Safe.json
      val request_of_yojson :
        Yojson.Safe.json -> request Ppx_deriving_yojson_runtime.error_or
      val request_of_sexp : Sexplib.Sexp.t -> request
      val sexp_of_request : request -> Sexplib.Sexp.t
      module Message_type = T.Message_type
      module Event_type = T.Event_type
      type heartbeat = unit
      val heartbeat_to_yojson : heartbeat -> Yojson.Safe.json
      val heartbeat_of_yojson :
        Yojson.Safe.json -> heartbeat Ppx_deriving_yojson_runtime.error_or
      val heartbeat_of_sexp : Sexplib.Sexp.t -> heartbeat
      val sexp_of_heartbeat : heartbeat -> Sexplib.Sexp.t
      module Reason = T.Reason
      type change_event =
        T.change_event = {
        price : decimal_string;
        side : Side.Bid_ask.t;
        reason : Reason.t;
        remaining : decimal_string;
        delta : decimal_string;
      }
      val change_event_to_yojson : change_event -> Yojson.Safe.json
      val change_event_of_yojson :
        Yojson.Safe.json -> change_event Ppx_deriving_yojson_runtime.error_or
      val change_event_of_sexp : Sexplib.Sexp.t -> change_event
      val sexp_of_change_event : change_event -> Sexplib.Sexp.t
      type trade_event =
        T.trade_event = {
        tid : int_number;
        price : decimal_string;
        amount : decimal_string;
        maker_side : Side.t;
      }
      val trade_event_to_yojson : trade_event -> Yojson.Safe.json
      val trade_event_of_yojson :
        Yojson.Safe.json -> trade_event Ppx_deriving_yojson_runtime.error_or
      val trade_event_of_sexp : Sexplib.Sexp.t -> trade_event
      val sexp_of_trade_event : trade_event -> Sexplib.Sexp.t
      type auction_open_event =
        T.auction_open_event = {
        auction_open_ms : Timestamp.ms;
        auction_time_ms : Timestamp.ms;
        first_indicative_ms : Timestamp.ms;
        last_cancel_time_ms : Timestamp.ms;
      }
      val auction_open_event_to_yojson :
        auction_open_event -> Yojson.Safe.json
      val auction_open_event_of_yojson :
        Yojson.Safe.json ->
        auction_open_event Ppx_deriving_yojson_runtime.error_or
      val auction_open_event_of_sexp : Sexplib.Sexp.t -> auction_open_event
      val sexp_of_auction_open_event : auction_open_event -> Sexplib.Sexp.t
      module Auction_result = T.Auction_result
      type auction_indicative_price_event =
        T.auction_indicative_price_event = {
        eid : int_number;
        result : Auction_result.t;
        time_ms : Timestamp.ms;
        highest_bid_price : decimal_string;
        lowest_ask_price : decimal_string;
        collar_price : decimal_string;
        indicative_price : decimal_string;
        indicative_quantity : decimal_string;
      }
      val auction_indicative_price_event_to_yojson :
        auction_indicative_price_event -> Yojson.Safe.json
      val auction_indicative_price_event_of_yojson :
        Yojson.Safe.json ->
        auction_indicative_price_event Ppx_deriving_yojson_runtime.error_or
      val auction_indicative_price_event_of_sexp :
        Sexplib.Sexp.t -> auction_indicative_price_event
      val sexp_of_auction_indicative_price_event :
        auction_indicative_price_event -> Sexplib.Sexp.t
      type auction_outcome_event =
        T.auction_outcome_event = {
        eid : int_number;
        result : Auction_result.t;
        time_ms : Timestamp.ms;
        highest_bid_price : decimal_string;
        lowest_ask_price : decimal_string;
        collar_price : decimal_string;
        auction_price : decimal_string;
        auction_quantity : decimal_string;
      }
      val auction_outcome_event_to_yojson :
        auction_outcome_event -> Yojson.Safe.json
      val auction_outcome_event_of_yojson :
        Yojson.Safe.json ->
        auction_outcome_event Ppx_deriving_yojson_runtime.error_or
      val auction_outcome_event_of_sexp :
        Sexplib.Sexp.t -> auction_outcome_event
      val sexp_of_auction_outcome_event :
        auction_outcome_event -> Sexplib.Sexp.t
      module Auction_event_type = T.Auction_event_type
      type auction_event =
          [ `Auction_indicative_price of auction_indicative_price_event
          | `Auction_open of auction_open_event
          | `Auction_outcome of auction_outcome_event ]
      val __auction_event_of_sexp__ : Sexplib.Sexp.t -> auction_event
      val auction_event_of_sexp : Sexplib.Sexp.t -> auction_event
      val sexp_of_auction_event : auction_event -> Sexplib.Sexp.t
      val auction_event_to_yojson : auction_event -> Yojson.Safe.json
      val auction_event_of_yojson :
        Yojson.Safe.json -> (auction_event, string) Result.t
      type event =
          [ `Auction of auction_event
          | `Change of change_event
          | `Trade of trade_event ]
      val __event_of_sexp__ : Sexplib.Sexp.t -> event
      val event_of_sexp : Sexplib.Sexp.t -> event
      val sexp_of_event : event -> Sexplib.Sexp.t
      val event_to_yojson : event -> Yojson.Safe.json
      val event_of_yojson : Yojson.Safe.json -> (event, string) Result.t
      type update =
        T.update = {
        event_id : int_number;
        events : event array;
        timestamp : Timestamp.sec option;
        timestampms : Timestamp.ms option;
      }
      val update_to_yojson : update -> Yojson.Safe.json
      val update_of_yojson :
        Yojson.Safe.json -> update Ppx_deriving_yojson_runtime.error_or
      val update_of_sexp : Sexplib.Sexp.t -> update
      val sexp_of_update : update -> Sexplib.Sexp.t
      type message = [ `Heartbeat of heartbeat | `Update of update ]
      val __message_of_sexp__ : Sexplib.Sexp.t -> message
      val message_of_sexp : Sexplib.Sexp.t -> message
      val sexp_of_message : message -> Sexplib.Sexp.t
      type response =
        T.response = {
        socket_sequence : int_number;
        message : message;
      }
      val response_of_sexp : Sexplib.Sexp.t -> response
      val sexp_of_response : response -> Sexplib.Sexp.t
      val response_to_yojson : response -> Yojson.Safe.json
      val response_of_yojson :
        Yojson.Safe.json -> (response, string) Result.t
      val client :
        (module Gemini__.Cfg.S) ->
        string Core.Option.t ->
        string Core.Option.t ->
        T.uri_args -> (unit * unit) Async_extra__.Import.Deferred.t
      val handle_client :
        [< Async.Socket.Address.t ] ->
        Async.Reader.t -> Async.Writer.t -> unit Async.Deferred.t
      val command : string * Async.Command.t
    end
  val command : Async.Command.t
end
