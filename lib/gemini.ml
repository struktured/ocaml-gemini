open Core
open Async

type decimal = string [@@deriving yojson, sexp]

let host = "api.gemini.com"
let version = "v1"
let method_ = `Post

let path_with_version ?(version=version)
    ~entity operation =
  sprintf "%s/%s/%s"
    version
    entity
    operation

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

let to_yojson = function
  | `Btc_usd -> `String "btcusd"
  | `Eth_usd -> `String "ethusd"
  | `Eth_btc -> `String "ethbtc"

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
    | "exchange_limit" -> Result.Ok `Exchange_limit
    | (_:string) ->
      Result.Error
        (
          sprintf
            "order_type must be \"exchange_limit\", but got %s"
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
  | `Exchange_limit -> `String "exchange_limit"
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
  let entity = "order"
  let path_with_version ?version =
    path_with_version ?version ~entity

  let uri ?version operation =
      Uri.make
        ~host
        ~path:(path_with_version ?version operation)

  module Status = struct
    let operation = "status"

    type request = {
      request:string;
      nonce:int;
      order_id:string;
    } [@@deriving yojson, sexp]

    type t = {
      order_id : string;
      id : string;
      symbol : Symbol.t; (*I"btcusd",*)
      exchange : Exchange.t ;(*gemini*)
      avg_execution_price : decimal;
      side : Side.t;
      type_ : Order_type.t;
      timestamp : string;
      timestampms : int;
      is_live : bool;
      is_cancelled : bool;
      is_hidden : bool;
      was_forced : bool;
      executed_amount : int;
      remaining_amount: int;
      options: Order_execution_option.t list;
      price : decimal;
      original_amount : decimal;
    } [@@deriving yojson, sexp]
  end

  module New = struct
    let operation = "new"

    type request = {
      request:string;
      nonce:int;
      client_order_id:string;
      symbol:string;
      amount:string;
      price:decimal; (* zarith *)
      side:Side.t;
      type_:Order_type.t [@name "type"];
      options: Order_execution_option.t
    } [@@deriving sexp, yojson]

(*
request	string	The literal string "/v1/order/new"
nonce	integer	The nonce, as described in Private API Invocation
client_order_id	string	Recommended. A client-specified order id
symbol	string	The symbol for the new order
amount	string	Quoted decimal amount to purchase
price	string	Quoted decimal amount to spend per unit
side	string	"buy" or "sell"
type	string	The order type. Only "exchange limit" supported through this API
options	array	Optional. An optional array containing at most one supported order
execution option. See Order execution options for details.
   *)
    type response = Status.t [@@deriving yojson, sexp]
  end
end

