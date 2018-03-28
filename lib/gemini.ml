module Auth = Auth
module Result = Json.Result

type int_number = int64 [@encoding `number] [@@deriving sexp, yojson]
type int_string = int64 [@encoding `string] [@@deriving sexp, yojson]
type decimal_number = float [@encoding `number] [@@deriving sexp, yojson]
type decimal_string = string [@@deriving sexp, yojson]


module V1 = struct
let path = ["v1"]

module Heartbeat = struct
  module T = struct
    let name = "heartbeat"
    let path = path@["heartbeat"]
    type request = unit [@@deriving sexp, yojson]
    type response = {result:bool [@default true]} [@@deriving sexp, yojson]
  end
  include T
  include Rest.Make_no_arg(T)
end

module Timestamp = struct

  type t = Time.t [@@deriving sexp]

  type ms = t [@@deriving sexp]
  type sec = t [@@deriving sexp]

  let to_yojson t =
    Time.to_span_since_epoch t |>
    Time.Span.to_ms |>
    Float.to_string_hum ~decimals:0 |>
    fun s -> `String s

  let of_yojson span_fn json =
    (match json with
    | `String s ->
      `Ok (Float.of_string s)
    | `Int i ->
      `Ok (Float.of_int i)
    | `Int64 i ->
      `Ok (Float.of_int64 i)
    | #Yojson.Safe.json as json ->
      `Error json
    ) |>
    function
     | `Error json ->
      Result.Error
        (sprintf "expected float as json but got %S"
           (Yojson.Safe.pretty_to_string json))
     | `Ok f ->
       span_fn f |>
       Time.of_span_since_epoch |>
       fun ok -> Result.Ok ok

  let ms_of_yojson (ms:Yojson.Safe.json) =
    of_yojson Time.Span.of_ms ms
  let ms_to_yojson (ms:ms) = to_yojson ms

  let sec_of_yojson (sec:Yojson.Safe.json) =
    of_yojson Time.Span.of_sec sec
  let sec_to_yojson (sec:sec) = to_yojson sec


end

module Currency = struct

  module T = struct
    type t = [`Eth | `Btc | `Usd] [@@deriving sexp, enumerate]
    let to_string = function
      | `Eth -> "eth"
      | `Btc -> "btc"
      | `Usd -> "usd"
  end
  include T
  include Json.Make(T)

end

module Symbol = struct
  module T = struct
   type t = [`Btcusd | `Ethusd | `Ethbtc] [@@deriving sexp, enumerate]
    let to_string = function
      | `Btcusd -> "btcusd"
      | `Ethusd -> "ethusd"
      | `Ethbtc -> "ethbtc"
  end
  include T
  include Json.Make(T)

end


module Exchange = struct

  module T = struct
    type t = [`Gemini] [@@deriving sexp, enumerate]
    let to_string `Gemini = "gemini"
  end
  include T
  include Json.Make(T)
end

module Side =
struct
  module T = struct
    type t = [`Buy | `Sell] [@@deriving sexp, enumerate]

    let to_string = function
      | `Buy -> "buy"
      | `Sell -> "sell"
  end
  include T
  include Json.Make(T)
end

module Order_type = struct
  module T = struct
    type t = [`Exchange_limit] [@@deriving sexp, enumerate]
    let to_string = function
      | `Exchange_limit -> "exchange limit"
   end
  include T
  include Json.Make(T)

end

module Order_execution_option = struct

  module T = struct
    type t =
    [`Maker_or_cancel
    |`Immediate_or_cancel
    |`Auction_only
    ] [@@deriving sexp, enumerate]

    let to_string = function
    | `Maker_or_cancel -> "maker_or_cancel"
    | `Immediate_or_cancel -> "immediate_or_cancel"
    | `Auction_only -> "auction_only"

  end
  include T
  include Json.Make(T)

end



module Order =
struct
  let name = "order"
  let path = path@["order"]

  module Status =
  struct
    module T = struct
      let name = "status"
      let path = path@["status"]

      type request = {
        order_id:int_number;
      } [@@deriving yojson, sexp]

      type response = {
        order_id : int_string;
        id : int_string;
        symbol : Symbol.t;
        exchange : Exchange.t;
        avg_execution_price : decimal_string;
        side : Side.t;
        type_ : Order_type.t [@key "type"];
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
      } [@@deriving yojson, sexp]
    end
    include T
    include Rest.Make(T)
  end

  module New = struct
    module T = struct
      let name = "new"
      let path = path@["new"]

      type request = {
        client_order_id:string;
        symbol:Symbol.t;
        amount:decimal_string;
        price:decimal_string;
        side:Side.t;
        type_:Order_type.t [@key "type"];
        options: Order_execution_option.t list;
      } [@@deriving sexp, yojson]

      type response = Status.response [@@deriving yojson, sexp]
    end
    include T
    include Rest.Make(T)

  end

  module Cancel = struct
    let name = "cancel"
    let path = path@["cancel"]

    module By_order_id = struct
      module T = struct
      let name = "by-order-id"
      let path = path

      type request = {order_id:int_string} [@@deriving sexp, yojson]

      type response = Status.response [@@deriving sexp, yojson]
      end
    include T
    include Rest.Make(T)
  end
    type details =
          {cancelled_orders:Status.response list [@key "cancelledOrders"];
           cancel_rejects:Status.response list [@key "cancelRejects"]
          } [@@deriving sexp, yojson]

    module All = struct
      module T = struct
        let name = "all"
        let path = path@["all"]
        type request = unit [@@deriving sexp, yojson]

        type response = {details:details} [@@deriving sexp, yojson]
      end
      include T
      include Rest.Make_no_arg(T)
    end

    module Session = struct
      module T = struct
        let name = "session"
        let path = path@["session"]
        type request = unit [@@deriving sexp, yojson]
        type response = {details:details} [@@deriving sexp, yojson]
      end
      include T
      include Rest.Make_no_arg(T)
    end

    let command : string * Command.t =
    (name,
     Command.group
       ~summary:(Path.to_summary ~has_subnames:true path)
       [By_order_id.command;
        Session.command;
        All.command
       ]
    )
  end

  let command : string * Command.t =
    (name,
     Command.group
       ~summary:(Path.to_summary ~has_subnames:true path)
       [New.command;
        Cancel.command;
        Status.command
       ]
    )

end

module Orders = struct

  module T = struct
    let name = "orders"
    let path = path@["orders"]

    type request = unit [@@deriving sexp, yojson]
    type response =
      Order.Status.response list [@@deriving yojson, sexp]
  end
  include T
  include Rest.Make_no_arg(T)
end

module Mytrades = struct
  type trade = {price:decimal_string;
                amount:decimal_string;
                timestamp:Timestamp.sec;
                timestampms:Timestamp.ms;
                type_: Side.t [@key "type"];
                aggressor: bool;
                fee_currency: Currency.t;
                fee_amount : decimal_string;
                tid:int_number;
                order_id : int_string;
                client_order_id : string option [@default None];
                is_auction_fill : bool;
                exchange : Exchange.t;
                (*break : string option [@default None] (* TODO make enum *) *)
               } [@@deriving yojson, sexp]
  module T = struct
    let name = "mytrades"
    let path = path@["mytrades"]
    type request =
      { symbol : Symbol.t;
        limit_trades: int option [@default None];
        timestamp: Timestamp.sec option [@default None]
      } [@@deriving sexp, yojson]
    type response = trade list [@@deriving yojson, sexp]
  end
  include T
  include Rest.Make(T)

end

module Tradevolume = struct

    type volume =
      {account_id:int_number;
       symbol:Symbol.t;
       base_currency:Currency.t;
       notional_currency:Currency.t;
       data_date:string; (*TODO use timestamp or a date module with MMMM-DD-YY *)
       total_volume_base:decimal_number;
       maker_buy_sell_ratio:decimal_number;
       buy_maker_base:decimal_number;
       buy_maker_notional:decimal_number;
       buy_maker_count:int_number;
       sell_maker_base:decimal_number;
       sell_maker_notional:decimal_number;
       sell_maker_count:int_number;
       buy_taker_base:decimal_number;
       buy_taker_notional:decimal_number;
       buy_taker_count:int_number;
       sell_taker_base:decimal_number;
       sell_taker_notional:decimal_number;
       sell_taker_count:int_number;
      } [@@deriving yojson, sexp]
   module T = struct
    let name = "tradevolume"
    let path = path@["tradevolume"]
    type request = unit [@@deriving yojson, sexp]
    type response = volume list list [@@deriving yojson, sexp]
  end
  include T
  include Rest.Make_no_arg(T)
end

module Balances = struct

  module T = struct
    let name = "balances"
    let path = path@["balances"]

    type request = unit [@@deriving yojson, sexp]
    type balance =
      {currency:Currency.t;
       amount:decimal_string;
       available:decimal_string;
       available_for_withdrawal:decimal_string [@key "availableForWithdrawal"];
       type_: string [@key "type"]
      } [@@deriving yojson, sexp]
    type response = balance list [@@deriving yojson, sexp]
  end

  include T
  include Rest.Make_no_arg(T)
end

  module Market_data =
  struct

    module Side =
    struct
      module Bid_ask = struct
        module T = struct
          type t = [`Bid | `Ask] [@@deriving sexp, enumerate]
          let to_string : [<t] -> string = function
            | `Bid -> "bid"
            | `Ask -> "ask"
        end
        include T
        include Json.Make(T)
      end

      module Auction = struct
          module T = struct
            type t = [`Auction] [@@deriving sexp, enumerate]
            let to_string = function
                `Auction -> "auction"
          end
          include T
          include Json.Make(T)
      end

      module T = struct
        type t = [Bid_ask.t | Auction.t] [@@deriving sexp, enumerate]
        let to_string : [<t] -> string = function
          | #Bid_ask.t as bid_ask -> Bid_ask.to_string bid_ask
          | #Auction.t as auction -> Auction.to_string auction
      end
      include T
      include Json.Make(T)
    end

    module T = struct
      let name = "marketdata"
      let path = path@["marketdata";"ethusd"]
      type uri_path = Symbol.t [@@deriving sexp, yojson]
      type request =
        { heartbeat : bool option [@default None] } [@@deriving sexp, yojson]

      module Message_type = struct
        module T = struct
          type t = [`Update | `Heartbeat] [@@deriving sexp, enumerate]
          let to_string = function
            | `Update -> "update"
            | `Heartbeat -> "heartbeat"
        end
        include T
        include Json.Make(T)
      end

      module Event_type = struct
        module T = struct
          type t = [`Trade | `Change | `Auction]
          [@@deriving sexp, enumerate]

          let to_string = function
            | `Trade -> "trade"
            | `Change -> "change"
            | `Auction -> "auction"
        end
        include T
        include Json.Make(T)
      end

      type heartbeat = unit [@@deriving sexp, yojson]

      module Reason = struct
        module T = struct
        type t =
          [`Place | `Trade | `Cancel | `Initial] [@@deriving sexp, enumerate]
        let to_string = function
          | `Place -> "place"
          | `Trade -> "trade"
          | `Cancel -> "cancel"
          | `Initial -> "initial"
        end
        include T
        include Json.Make(T)
      end

      type change_event =
        {price:decimal_string;
         side:Side.Bid_ask.t;
         reason:Reason.t;
         remaining:decimal_string;
         delta:decimal_string
        } [@@deriving sexp, yojson]

      type trade_event =
        {tid:int_number;
         price:decimal_string;
         amount:decimal_string;
         maker_side:Side.t [@key "makerSide"]
        } [@@deriving yojson, sexp]

      type auction_open_event =
        {auction_open_ms:Timestamp.ms;
         auction_time_ms:Timestamp.ms;
         first_indicative_ms:Timestamp.ms;
         last_cancel_time_ms:Timestamp.ms
        } [@@deriving sexp, yojson]


      module Auction_result = struct
        module T = struct
          type t =
            [`Success | `Failure] [@@deriving sexp, enumerate]
          let to_string = function
            | `Success -> "success"
            | `Failure -> "failure"
        end
        include T
        include Json.Make(T)
      end

      type auction_indicative_price_event =
        {eid:int_number;
         result:Auction_result.t;
         time_ms:Timestamp.ms;
         highest_bid_price:decimal_string;
         lowest_ask_price:decimal_string;
         collar_price:decimal_string;
         indicative_price:decimal_string;
         indicative_quantity:decimal_string
        } [@@deriving sexp, yojson]


      type auction_outcome_event =
        {eid:int_number;
         result:Auction_result.t;
         time_ms:Timestamp.ms;
         highest_bid_price:decimal_string;
         lowest_ask_price:decimal_string;
         collar_price:decimal_string;
         auction_price:decimal_string;
         auction_quantity:decimal_string
        } [@@deriving sexp, yojson]

      module Auction_event_type = struct
        module T = struct
          type t =
            [ `Auction_open
            | `Auction_indicative_price
            | `Auction_outcome ]
          [@@deriving sexp, enumerate]

          let to_string = function
            | `Auction_open ->
              "auction_open"
            | `Auction_indicative_price ->
              "auction_indicative_price"
            | `Auction_outcome ->
              "auction_outcome"
        end
        include T
        include Json.Make(T)
      end

      type auction_event =
        [
          | `Auction_open of auction_open_event
          | `Auction_indicative_price of
              auction_indicative_price_event
          | `Auction_outcome of auction_outcome_event
        ] [@@deriving sexp]

      let auction_event_to_yojson :
        auction_event -> Yojson.Safe.json = fun _ ->
        failwith "auction_event_to_yojson: unsupported"

      let auction_event_of_yojson :
        Yojson.Safe.json -> (auction_event, string) Result.t = function
        | `Assoc assoc as json ->
          (List.Assoc.find assoc ~equal:String.equal
             "type" |> function
           | None ->
             Result.failf "no auction event type in json payload: %s"
               (Yojson.Safe.to_string json)
           | Some event_type ->
             Auction_event_type.of_yojson event_type |> function
             | Result.Error _ as e -> e
             | Result.Ok event_type ->
               let json' = `Assoc
                   (List.Assoc.remove ~equal:String.equal assoc "type") in
               (match event_type with
                | `Auction_open ->
                  auction_open_event_of_yojson json' |>
                  Result.map ~f:(fun event -> `Auction_open event)
                | `Auction_indicative_price ->
                  auction_indicative_price_event_of_yojson json' |>
                  Result.map ~f:(fun event -> `Auction_indicative_price event)
                | `Auction_outcome ->
                  auction_outcome_event_of_yojson json' |>
                  Result.map ~f:(fun event -> `Auction_outcome event)
               )
          )
          | #Yojson.Safe.json as json ->
            Result.failf "expected association type in json payload: %s"
              (Yojson.Safe.to_string json)


      type event =
        [ `Change of change_event
        | `Trade of trade_event
        | `Auction of auction_event
        ] [@@deriving sexp]

      let event_to_yojson : event -> Yojson.Safe.json
        = fun _ -> failwith "event_to_yojson: unsupported"

      let event_of_yojson :
        Yojson.Safe.json -> (event,string) Result.t = function
        | `Assoc assoc as json ->
          (List.Assoc.find assoc ~equal:String.equal
             "type" |> function
           | None ->
             Result.failf "no event type in json payload: %s"
               (Yojson.Safe.to_string json)
           | Some event_type ->
             Event_type.of_yojson event_type |> function
             | Result.Error _ as e -> e
             | Result.Ok event_type ->
               let json' = `Assoc
                   (List.Assoc.remove ~equal:String.equal assoc "type") in
               (match event_type with
                | `Change ->
                  change_event_of_yojson json' |>
                  Result.map ~f:(fun event -> `Change event)
                | `Trade ->
                  trade_event_of_yojson json' |>
                  Result.map ~f:(fun event -> `Trade event)
                | `Auction ->
                  auction_event_of_yojson json' |>
                  Result.map ~f:(fun event -> `Auction event)
               )
          )
          | #Yojson.Safe.json as json ->
            Result.failf "expected association type in json payload: %s"
              (Yojson.Safe.to_string json)


      type update =
        { event_id : int_number [@key "eventId"];
          events : event array;
          timestamp : Timestamp.sec option [@default None];
          timestampms : Timestamp.ms option [@default None]
        } [@@deriving sexp, yojson]

      type message =
        [`Heartbeat of heartbeat | `Update of update] [@@deriving sexp]

      type response =
        {
          socket_sequence : int_number;
          message : message
        } [@@deriving sexp]

      let response_to_yojson : response -> Yojson.Safe.json =
        fun _ -> failwith "response_to_yojson: unsupported"

      let response_of_yojson :
        Yojson.Safe.json -> (response, string) Result.t = function
        | `Assoc assoc as json ->
          (
            (
              List.Assoc.find ~equal:String.equal assoc "socket_sequence",
              List.Assoc.find ~equal:String.equal assoc "type"
            ) |> function
            | (None, _) ->
             Result.failf "no sequence number in json payload: %s"
               (Yojson.Safe.to_string json)
            | (_, None) ->
             Result.failf "no message type in json payload: %s"
               (Yojson.Safe.to_string json)
            | (Some socket_sequence, Some message_type) ->
              Result.both
                (int_number_of_yojson socket_sequence)
                (Message_type.of_yojson message_type) |> function
             | Result.Error _ as e -> e
             | Result.Ok (socket_sequence, message_type) ->
               let json' = `Assoc
                   (List.Assoc.remove
                      ~equal:String.equal assoc "type" |> fun assoc ->
                    List.Assoc.remove
                      ~equal:String.equal assoc "socket_sequence"
                   ) in
               (
                 (match message_type with
                  | `Heartbeat ->
                    heartbeat_of_yojson json' |>
                    Result.map ~f:(fun event -> `Heartbeat event)
                  | `Update ->
                    update_of_yojson json' |>
                    Result.map ~f:(fun event -> `Update event)
                 )
                 |> Result.map
                   ~f:(fun message -> {socket_sequence;message})
               )
          )
          | #Yojson.Safe.json as json ->
            Result.failf "response_of_yojson:expected association type \
            in json payload: %s"
              (Yojson.Safe.to_string json)

    end
    include T
    include Ws.Make(T)

  end
let command : Command.t =
  Command.group
    ~summary:"Gemini Command System"
    [Heartbeat.command;
     Order.command;
     Orders.command;
     Mytrades.command;
     Tradevolume.command;
     Balances.command;
     Market_data.command
    ]

end
