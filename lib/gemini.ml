open Core
open Async

module Auth = Auth

type int_number = int64 [@encoding `number] [@@deriving sexp, yojson]
type int_string = int64 [@encoding `string] [@@deriving sexp, yojson]
type decimal_number = float [@encoding `number] [@@deriving sexp, yojson]
type decimal_string = string [@@deriving yojson, sexp]


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

  let ms_of_yojson (ms:Yojson.Safe.json) = of_yojson Time.Span.of_ms ms
  let ms_to_yojson (ms:ms) = to_yojson ms

  let sec_of_yojson (sec:Yojson.Safe.json) = of_yojson Time.Span.of_sec sec
  let sec_to_yojson (sec:sec) = to_yojson sec


end

module Currency = struct

  type t = [`Eth | `Btc | `Usd] [@@deriving sexp]

  let of_yojson json =
    match json with
    | `String s ->
      (match String.lowercase s with
       | "btc" -> Result.Ok `Btc
       | "eth" -> Result.Ok `Eth
       | "usd" -> Result.Ok `Usd
       | (_:string) ->
         Result.Error
           (
             sprintf
               "symbol must be \"btcusd, ethusd, or ethbtc\", but got %s"
               s
           )
      )
    | #Yojson.Safe.json as json ->
      Result.Error
        (
          sprintf
            "symbol must be a json string, but got %s"
            (Yojson.Safe.to_string json)
        )

  let to_yojson = function
    | `Btc -> `String "btc"
    | `Eth -> `String "eth"
    | `Usd -> `String "usd"


end
module Symbol = struct
  type t = [`Btc_usd | `Eth_usd | `Eth_btc] [@@deriving sexp]

  let of_yojson json =
    match json with
    | `String s ->
      (match String.lowercase s with
       | "btcusd" -> Result.Ok `Btc_usd
       | "ethusd" -> Result.Ok `Eth_usd
       | "ethbtc" -> Result.Ok `Eth_btc
       | (_:string) ->
         Result.Error
           (
             sprintf
               "symbol must be \"btcusd, ethusd, or ethbtc\", but got %s"
               s
           )
      )
    | #Yojson.Safe.json as json ->
      Result.Error
        (
          sprintf
            "symbol must be a json string, but got %s"
            (Yojson.Safe.to_string json)
        )

  let to_string = function
    | `Btc_usd -> "btcusd"
    | `Eth_usd -> "ethusd"
    | `Eth_btc -> "ethbtc"


  let to_yojson t = to_string t |> fun s -> `String s

end


module Exchange = struct

  type t = [`Gemini] [@@deriving sexp]

  let of_yojson json =
    match json with
    | `String s ->
      (match String.lowercase s with
       | "gemini" -> Result.Ok `Gemini
       | (_:string) ->
         Result.Error
           (
             sprintf
               "exchange must be \"gemini\", but got %s"
               s
           )
      )
    | #Yojson.Safe.json as json ->
      Result.Error
        (
          sprintf
            "exchange must be a json string, but got %s"
            (Yojson.Safe.to_string json)
        )

  let to_yojson = function
    | `Gemini -> `String "gemini"
end

module Side =
struct
  type t = [`Buy | `Sell] [@@deriving sexp]

  let of_yojson json =
    match json with
    | `String s ->
      (match String.lowercase s with
       | "buy" -> Result.Ok `Buy
       | "sell" -> Result.Ok `Sell
       | (_:string) ->
         Result.Error
           (
             sprintf
               "side must be one of \"buy\" or \"sell\", but got %s"
               s
           )
      )
    | #Yojson.Safe.json as json ->
      Result.Error
        (
          sprintf
            "side must be a json string, but got %s"
            (Yojson.Safe.to_string json)
        )

  let to_yojson = function
    | `Buy -> `String "buy"
    | `Sell -> `String "sell"
end

module Order_type = struct
  type t = [`Exchange_limit] [@@deriving sexp]

  let of_yojson json =
    match json with
    | `String s ->
      (match String.lowercase s with
       | "exchange limit" -> Result.Ok `Exchange_limit
       | (_:string) ->
         Result.Error
           (
             sprintf
               "order_type must be \"exchange limit\", but got %s"
               s
           )
      )
    | #Yojson.Safe.json as json ->
      Result.Error
        (
          sprintf
            "order_type must be a json string, but got %s"
            (Yojson.Safe.to_string json)
        )

  let to_yojson = function
    | `Exchange_limit -> `String "exchange limit"
end

module Order_execution_option = struct

  type t =
    [`Maker_or_cancel
    |`Immediate_or_cancel
    |`Auction_only
    ] [@@deriving sexp]

  let of_yojson json =
    match json with
    | `String s ->
      (match String.lowercase s with
       | "maker_or_cancel" -> Result.Ok `Maker_or_cancel
       | "immediate_or_cancel" -> Result.Ok `Immediate_or_cancel
       | "auction_only" -> Result.Ok `Auction_only
       | (_:string) ->
         Result.Error
           (
             sprintf
               "order_execution_option must be \"exchange_limit\", but got %s"
               s
           )
      )
    | #Yojson.Safe.json as json ->
      Result.Error
        (
          sprintf
            "order_execution_option must be a json string, but got %s"
            (Yojson.Safe.to_string json)
        )

  let to_yojson : t -> Yojson.Safe.json = function
    | `Maker_or_cancel -> `String "maker_or_cancel"
    | `Immediate_or_cancel -> `String "immediate_or_cancel"
    | `Auction_only -> `String "auction_only"

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


module Websocket = struct

  module type CHANNEL = sig
    val path : string list
    val name : string
    type request [@@deriving sexp, yojson]
  end

  module Subscribe(Channel:CHANNEL) = struct
    let subscribe
        (module Cfg:Cfg.S)
        =
      let uri = Uri.make ~scheme:"wss" ~host:Cfg.api_host
          ~path:(Path.to_string Channel.path) () in
      Log.Global.info "uri: %s" (Uri.to_string uri);
      Unix.Inet_addr.of_string_or_getbyname
        (Uri.to_string uri) >>= fun inet_addr ->
      Log.Global.info "inet_addr: %s"
        (Unix.Inet_addr.to_string inet_addr);
      let addr = Socket.Address.Inet.create inet_addr
          ~port:443 in
      Log.Global.info "addr: %s"
        (Socket.Address.Inet.to_string addr);
      let extra_headers = None in
      let socket = Socket.create Socket.Type.tcp in
      return socket
      (*Socket.connect socket addr*) >>|
      fun socket ->
      Log.Global.info "connected to websocket";
      let writer = Writer.create (Socket.fd socket) in
      let reader = Reader.create (Socket.fd socket) in
      let reader, writer =
        Websocket_async.client_ez
          ~log:(Lazy.force Log.Global.log)
          ~name:Channel.name
          ?extra_headers
          uri
          socket
          reader
          writer in
      reader,writer

    let command =
    let open Command.Let_syntax in
    (Channel.name,
     Command.async
       ~summary:(Path.to_summary ~has_subnames:false Channel.path)
       [%map_open
         let config = Cfg.param
       (*  and request = anon ("request" %: sexp)*)
         in
         fun () ->
(*           let request = Channel.request_of_sexp request in
           Log.Global.info "request:\n %s"
             (Channel.sexp_of_request request |> Sexp.to_string); *)
           let config = Cfg.get config in
           subscribe config >>= fun (reader,_writer) ->
           let rec process () =
             Log.Global.info "process ()";
             Log.Global.flushed () >>= fun () ->
           Pipe.values_available reader >>=
           function
           | `Eof -> Deferred.unit
           | `Ok ->
            (Pipe.read reader >>= function
            | `Ok x ->
              Log.Global.info "frame: %s" x;
              process ()
            | `Eof -> Deferred.unit
            ) in
           process ()
       ]
    )

  end

  module Market_data = struct

    module Side =
    struct
      type bid_ask = [`Bid | `Ask] [@@deriving sexp, yojson]
      type auction = [`Auction] [@@deriving sexp, yojson]
      type t = [bid_ask | auction] [@@deriving sexp, yojson]
    end

    module T = struct
      let name = "marketdata"
      let path = path@["marketdata";"ethusd"]
      type uri_path = Symbol.t [@@deriving sexp, yojson]
      type request =
        { heartbeat : bool option [@default None] } [@@deriving sexp, yojson]

      type message_type = [`Update | `Heartbeat] [@@deriving sexp, yojson]

      type response =
        { message_type : message_type [@key "type"];
          socket_sequence: int_number
        } [@@deriving sexp, yojson]

      type event_type = [`Trade| `Change| `Auction] [@@deriving sexp, yojson]

      type heartbeat = unit [@@deriving sexp, yojson]

      module Reason = struct
        type t =
          [`Place | `Trade | `Cancel | `Initial] [@@deriving sexp, yojson]
      end

      type change_event =
        {price:decimal_string;
         side:Side.bid_ask;
         reason:Reason.t;
         remaining:decimal_string;
         delta:decimal_string
        } [@@deriving sexp, yojson]

      type trade_event =
        {price:decimal_string;
         amount:decimal_string;
         maker_side:Side.t [@key "makerSide"]
        } [@@deriving yojson, sexp]

      type auction_open_event =
        {auction_open_ms:Timestamp.ms;
         auction_time_ms:Timestamp.ms;
         first_indicative_ms:Timestamp.ms;
         last_cancel_time_ms:Timestamp.ms
        } [@@deriving sexp, yojson]


      type auction_result =
        [`Success | `Failure] [@@deriving sexp, yojson]

      type auction_indicative_price_event =
        {eid:int_number;
         result:auction_result;
         time_ms:Timestamp.ms;
         highest_bid_price:decimal_string;
         lowest_ask_price:decimal_string;
         collar_price:decimal_string;
         indicative_price:decimal_string;
         indicative_quantity:decimal_string
        } [@@deriving sexp, yojson]


      type auction_outcome_event =
        {eid:int_number;
         result:auction_result;
         time_ms:Timestamp.ms;
         highest_bid_price:decimal_string;
         lowest_ask_price:decimal_string;
         collar_price:decimal_string;
         auction_price:decimal_string;
         auction_quantity:decimal_string
        } [@@deriving sexp, yojson]

      type auction_event =
        [
          | `Auction_open of auction_open_event
          | `Auction_indicative_price of
              auction_indicative_price_event
          | `Auction_outcome of auction_outcome_event
        ] [@@deriving sexp, yojson]

      type event =
        [ `Change of change_event
        | `Trade of trade_event
        | `Auction of auction_event
        ] [@@deriving sexp, yojson]

      type update =
        { event_id : int_number [@key "eventId"];
          events : event array;
          timestamp : Timestamp.sec;
          timestampms : Timestamp.ms
        } [@@deriving sexp, yojson]

    end
    include T
    include Subscribe(T)

  end
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
     Websocket.Market_data.command
    ]

end
