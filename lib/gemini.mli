open Common
module Auth = Auth
module Result = Json.Result


(** Version v1 of the Gemini REST and web socket apis. *)
module V1 : sig
  val path : string list
  module Heartbeat :
    sig
      type request = unit [@@deriving sexp, yojson]
      type response = { result : bool; } [@@deriving sexp, of_yojson]
      val post :
        (module Cfg.S) ->
        Nonce.reader ->
        request ->
        [
        | Rest.Error.post
        | `Ok of response
        ] Deferred.t
    end
  module Timestamp : module type of Timestamp
  module Currency : module type of Currency
  module Symbol : module type of Symbol
  module Exchange : module type of Exchange
  module Side : module type of Side
  module Order_type : module type of Order_type
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
        type request =
          { order_id : int_number; } [@@deriving sexp, of_yojson]
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
        } [@@deriving sexp]
        include Rest.Operation.S
          with type request := request
          with type response := response
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
          type request = {
            client_order_id : string;
            symbol : Symbol.t;
            amount : decimal_string;
            price : decimal_string;
            side : Side.t;
            type_ : Order_type.t;
            options : Order_execution_option.t list;
          } [@@deriving sexp, of_yojson]
          type response = Status.response [@@deriving sexp]
          include Rest.Operation.S
            with type request := request
            with type response := response
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
              type request =
                { order_id : int_string; } [@@deriving sexp, of_yojson]
              type response = Status.response [@@deriving sexp]
              include Rest.Operation.S
                with type request := request
                with type response := response
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
              type request = unit [@@deriving sexp, of_yojson]
              type response =
                { details : details; } [@@deriving sexp]
              include Rest.Operation.S
                with type request := request
                with type response := response
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
              type request = unit [@@deriving sexp, of_yojson]
              type response = { details : details; }
              [@@deriving sexp]
              include Rest.Operation.S
                with type request := request
                with type response := response
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
      type request = unit [@@deriving sexp, of_yojson]
      type response = Order.Status.response list [@@deriving sexp]
      include Rest.Operation.S
        with type request := request
        with type response := response
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

      type request = {
        symbol : Symbol.t;
        limit_trades : int option;
        timestamp : Timestamp.sec option;
      } [@@deriving sexp, of_yojson]

      type response = trade list [@@deriving sexp]

      include Rest.Operation.S
        with type request := request
        with type response := response
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
      type request = unit [@@deriving sexp, of_yojson]
      type response = volume list list [@@deriving sexp]
      include Rest.Operation.S
        with type request := request
        with type response := response
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
      type request = unit [@@deriving sexp, of_yojson]
      type balance =
      {
        currency : Currency.t;
        amount : decimal_string;
        available : decimal_string;
        available_for_withdrawal : decimal_string;
        type_ : string;
      } [@@deriving sexp, yojson]
      type response = balance list [@@deriving sexp]
      include Rest.Operation.S
        with type request := request
        with type response := response
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
  module Market_data = Market_data
  val command : Command.t
end
