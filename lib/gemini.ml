open Core
open Async

type decimal = string [@@deriving yojson, sexp]

module Auth = struct

  type t = [`Base64 of Cstruct.t | Hex.t]

  let of_payload s : [< t > `Base64] = `Base64
      (Cstruct.of_string s |> Nocrypto.Base64.encode)

  let hmac_sha384 ~api_secret (`Base64 payload) : [< t > `Hex] =
    let key = Cstruct.of_string api_secret in
    Nocrypto.Hash.SHA384.hmac ~key payload
    |> Hex.of_cstruct

  let to_string : [<t] -> string = function
    | `Base64 cstruct -> Cstruct.to_string cstruct
    | `Hex hex -> hex
end

module Noonce = struct
  type reader = string Pipe.Reader.t

  module type S = sig
    type t [@@deriving sexp, yojson]

    val pipe : init:t -> unit -> string Pipe.Reader.t
  end

  module Int : S with type t = int = struct
    type t = int [@@deriving sexp, yojson]
    let pipe ~init () =
      Pipe.unfold ~init
        ~f:
          (fun s ->
             let s' = s + 1 in
             Some (Int.to_string s', s') |> return
          )
  end

end

module Cfg = struct

  let param ?default ~name ~env () =
    let name = sprintf "GEMINI_%s_%s"
        (String.uppercase env) name in
    match Unix.getenv name with
    | Some param -> param
    | None ->
      match default with
      | None ->
        failwithf "Environment variable \"%s\" must be specified"
          name ()
      | Some default -> default

  let host ~env =
    sprintf "api.%s.gemini.com" (String.lowercase env)
  let version_1 = "v1"

  module type S = sig
    val version : string
    val api_host : string
    val api_key : string
    val api_secret : string
  end

  let api_key = param ~name:"API_KEY"
  let api_secret = param ~name:"API_SECRET"

  let make env =
    let module M = struct
      let env = env
      let param = param ~env
      let version = version_1
      let api_host = host ~env
      let api_key = api_key ~env ()
      let api_secret = api_secret ~env ()
    end in
  (module M : S)

  module Sandbox () =
    struct
      include (val make "sandbox" : S)
    end

  module Production () =
    struct
      include (val make "production" : S)
     end

end

let path_with_version
    ~version
    ~entity
    ~operation =
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
  let path_with_version ~version =
    path_with_version ~version ~entity

  let uri ~version ~operation ~host =
    Uri.make
      ~host
      ~path:(path_with_version ~version ~operation)

  module Status = struct
    let operation = "status"

    type request = {
      order_id:string;
    } [@@deriving yojson, sexp]

    type response = {
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
    type response = Status.response [@@deriving yojson, sexp]
  end

  module Cancel = struct
    let operation = "cancel"

    let sub_operation =
      sprintf "%s/%s" operation


    type request = {order_id:string} [@@deriving sexp, yojson]

    type response = Status.response [@@deriving sexp, yojson]

    module All = struct
      let operation =
        sub_operation "all"
      type request = unit [@@deriving sexp, yojson]
      type response = {result:bool} [@@deriving sexp, yojson]
    end

    module Session = struct
      let operation =
        sub_operation "session"
      type request = unit [@@deriving sexp, yojson]
      type response = {result:bool} [@@deriving sexp, yojson]
    end
  end
end


module Entity = struct

  module type S = sig
    val name : string
  end
end

module Operation = struct

  module type S = sig
    module Entity : Entity.S
    val name : string
    type request [@@deriving sexp]
    type response [@@deriving sexp]
    val yojson_of_request : request -> Yojson.Safe.json
    val response_of_yojson : Yojson.Safe.json ->
      (response, string) Result.t

  end

end

module Request = struct

  type request_noonce =
    {request:string; noonce:string} [@@deriving sexp, yojson]

  type t =
    {request:string; noonce:string; payload:Yojson.Safe.json}

  let make ~request ~noonce payload =
    Pipe.read noonce >>= function
    | `Ok noonce -> return
        {request;noonce;payload}
    | `Eof -> assert false

  let to_yojson {request;noonce;payload} : Yojson.Safe.json =
    match request_noonce_to_yojson {request;noonce} with
   | `Assoc assoc ->
     (match payload with
     | `Null -> `Assoc assoc
     | `Assoc assoc' ->
       `Assoc (assoc @ assoc')
     | #Yojson.Safe.json as unsupported_yojson ->
     failwithf "expected json association for request payload but got %S"
       (Yojson.Safe.to_string unsupported_yojson) ()
     )
   | #Yojson.Safe.json as unsupported_yojson ->
     failwithf "expected json association for type request_noonce but got %S"
       (Yojson.Safe.to_string unsupported_yojson) ()

end

let post
    (type request)
    (type response)
    (module Cfg : Cfg.S)
    (module Operation : Operation.S with
      type request = request and type response = response)
    (noonce : Noonce.reader)
    (request : request) =
  let payload =
    Operation.yojson_of_request request in
  Request.make ~noonce ~request:Operation.name payload >>=
  fun request ->
  (Request.to_yojson request |>
  Yojson.Safe.to_string |>
  Auth.of_payload |> return)
  >>= fun payload ->
  let headers =
    Cohttp.Header.of_list
      ["X-GEMINI-PAYLOAD", Auth.to_string payload;
       "X-GEMINI-APIKEY", Cfg.api_key;
       "X-GEMINI-SIGNATURE",
         Auth.(
           hmac_sha384 ~api_secret:Cfg.api_secret payload |>
           to_string
         )
      ]
  in
  let uri = Uri.make
      ~scheme:"https"
      ~host:Cfg.api_host
      ~path:
        (path_with_version ~version:Cfg.version
           ~entity:Operation.Entity.name
           ~operation:Operation.name
        )
      ?query:None
      () in
  Cohttp_async.Client.post
    ~headers
    ?chunked:None
    ?interrupt:None
    ?ssl_config:None
    ?body:None
    uri >>= fun (response, body) ->
  match Cohttp.Response.status response with
  | `OK ->
    (
      Cohttp_async.Body.to_string body
      >>|
      (fun s ->
          let yojson = Yojson.Safe.from_string s in
          let response = Operation.response_of_yojson yojson in
          match response with
          | Result.Ok ok -> `Ok ok
          | Result.Error e -> `Json_parse_error e
      )
    )
  | `Not_found
  | `Bad_request
  | `Unauthorized as error -> return error
  | (code : Cohttp.Code.status_code) ->
    failwiths "unexpected status code"
      code Cohttp.Code.sexp_of_status_code





