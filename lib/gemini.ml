open Core
open Async

type decimal = string [@@deriving yojson, sexp]

module Error = struct
  type http = [ `Bad_request of string
              | `Not_found
              | `Not_acceptable of string
              | `Unauthorized of string] [@@deriving sexp]
  type json_error = {message:string;body:string} [@@deriving sexp]
  type json = [`Json_parse_error of json_error] [@@deriving sexp]

  type detail = {reason:string;message:string} [@@deriving sexp, yojson]
  type response = [`Error of detail] [@@deriving sexp]

  type post = [http|json|response] [@@deriving sexp]
end


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

let path_to_string path = sprintf "/%s"
    (String.concat ~sep:"/" path)

let path_to_summary ~has_subcommands
    path = sprintf "Gemini %s command%s"
    (String.concat ~sep:" " path)
    (match has_subcommands with
     | true -> "s"
     | false -> ""
    )


module Cfg = struct

  let create_config_dir () =
    let dirname = sprintf "%s/%s"
      (Unix.getenv_exn "HOME") ".gemini" in
    try_with ~extract_exn:true
    (fun () -> Unix.mkdir ?p:None ?perm:None dirname) >>=
    function
    | Result.Ok () -> Deferred.unit
    | Result.Error
        (Unix.Unix_error (Unix.Error.EEXIST, _, _)) -> Deferred.unit
    | Result.Error e ->
      Log.Global.error "failed to create gemini config directory
        at %S." dirname; raise e

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
    let env = String.lowercase env in 
    match env with
    | "production" -> sprintf "api.gemini.com"
    | _ -> sprintf "api.%s.gemini.com" env
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

  let arg_type = Command.Arg_type.create
    (fun s -> match String.lowercase s with

      | "production" ->
        let module Cfg : S = Production () in
        (module Cfg : S)
      | "sandbox" ->
        let module Cfg : S = Sandbox () in
        (module Cfg : S)
      | unsupported_env ->
        failwithf "environment %s not supported"
          unsupported_env ()
    )
  let param =
    Command.Param.(
      flag "-cfg" (required arg_type)
        ~doc:(
          sprintf "STRING the configuration the client will connect with \
                 (eg. sandbox or production)."
        )
    )

end

module Nonce = struct
  type reader = int Pipe.Reader.t

  module type S = sig
    type t [@@deriving sexp]

    val pipe : init:t -> unit -> int Pipe.Reader.t Deferred.t
  end

  module Counter : S with type t = int = struct
    type t = int [@@deriving sexp]
    let pipe ~init () =
      Pipe.unfold ~init
        ~f:
          (fun s ->
             let s' = s + 1 in
             Some (s, s') |> return
          )
    |> return
  end

  module File : S with type t = string = struct

    let create_nonce_file ?(default=0) filename =
      try_with ~extract_exn:true (fun () ->
          Unix.with_file ~mode:[`Rdonly] filename
        ~f:(fun fd ->  Deferred.unit)
        ) >>= function
      | Result.Ok _ -> Deferred.unit
      | Result.Error _ ->
        Writer.save ~contents:(sprintf "%d\n" default) filename

    type t = string [@@deriving sexp]


    (* TODO - too expensive- only check the file once, then do
     * everything in memory *)
    let pipe ~init:filename () =
      Cfg.create_config_dir () >>= fun () ->
      create_nonce_file ?default:None filename >>= fun () ->
      Pipe.unfold ~init:() ~f:
        (fun _ ->
          Reader.open_file ?buf_len:None
             filename >>= Reader.really_read_line
             ~wait_time:(Time.Span.of_ms 1.0) >>=
           (function
             | None -> return 0
             | Some nonce -> return @@ Int.of_string nonce
           ) >>= fun nonce ->
           let nonce' = nonce + 1 in
           Writer.save filename
             ~contents:(sprintf "%d\n" nonce') >>= fun () ->
           return @@ Some (nonce, ())
        ) |> return
  end
end


module Operation = struct

  module type S = sig
    val path : string list
    type request [@@deriving sexp]
    type response [@@deriving sexp]
    val request_to_yojson : request -> Yojson.Safe.json
    val response_of_yojson : Yojson.Safe.json ->
      (response, string) Result.t
  end

  type status= [`Ok | `Error of string]

end

module Request = struct

  type request_nonce =
    {request:string; nonce:int} [@@deriving sexp, yojson]

  type t =
    {request:string; nonce:int; payload:Yojson.Safe.json}

  let make ~request ~nonce payload =
    Pipe.read nonce >>= function
    | `Ok nonce ->
      return
        {request;nonce;payload}
    | `Eof -> assert false

  let to_yojson {request;nonce;payload} : Yojson.Safe.json =
    match request_nonce_to_yojson {request;nonce} with
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
      failwithf "expected json association for type request_nonce but got %S"
        (Yojson.Safe.to_string unsupported_yojson) ()

end


module Response = struct

  module Json_result = struct
    type t = [`Error | `Ok] [@@deriving sexp]
    let of_yojson json =
      match json with
      | `String s ->
        (match String.lowercase s with
         | "error" -> Result.Ok `Error
         | "ok" -> Result.Ok `Ok
         | (_:string) ->
           Result.Error
             (
               sprintf
                 "result must be one of %S or %S, but got %S"
                 "ok" "error" s
             )
        )
      | #Yojson.Safe.json as json ->
        Result.Error
          (
            sprintf
              "symbol must be a json string, but got %s"
              (Yojson.Safe.to_string json)
          )

    let to_yojson : t -> Yojson.Safe.json = function
      | `Ok -> `String "ok"
      | `Error -> `String "error"

    let split = function
      | `Assoc assoc as json ->
      (List.Assoc.find assoc ~equal:String.equal
        "result" |> function
      | None -> Result.Ok (None, json)
      | Some json' ->
        of_yojson json' |> Result.map ~f:(fun x ->
          (Some x,
           `Assoc
             (List.Assoc.remove assoc ~equal:String.equal "result")
          )
        )
      )
      | #Yojson.Safe.json as json ->
        Result.Ok (None, json)

  end

  type result_field = {result:Json_result.t} [@@deriving yojson, sexp]

  type t = {result:Json_result.t; payload:Yojson.Safe.json}

  let to_yojson {result;payload} : Yojson.Safe.json =
    match result_field_to_yojson {result} with
    | `Assoc assoc ->
      (match payload with
       | `Null -> `Assoc assoc
       | `Assoc assoc' ->
         `Assoc (assoc @ assoc')
       | #Yojson.Safe.json as unsupported_yojson ->
         failwithf "expected json association for response payload but got %S"
           (Yojson.Safe.to_string unsupported_yojson) ()
      )
    | #Yojson.Safe.json as unsupported_yojson ->
      failwithf "expected json association for type result_field but got %S"
        (Yojson.Safe.to_string unsupported_yojson) ()


  let parse json ok_of_yojson =
    match Json_result.split json with
    | Result.Ok (result, payload) ->
      (match result with
      | None
      | Some `Ok ->
        (ok_of_yojson payload |> function
          | Result.Ok x -> `Ok x
          | Result.Error e ->
            `Json_parse_error
              Error.{message=e; body=Yojson.Safe.to_string payload}

        )
      | Some `Error ->
        (Error.detail_of_yojson payload |> function
          | Result.Ok x -> `Error x
          | Result.Error e ->
            `Json_parse_error
              Error.{message=e; body=Yojson.Safe.to_string payload}
        )
      )
    | Result.Error e ->
      `Json_parse_error
        Error.{message=e;body=Yojson.Safe.to_string json}

end

module Service(Operation:Operation.S) =
struct
  let post
      (module Cfg : Cfg.S)
      (nonce : Nonce.reader)
      (request : Operation.request) :
    [ `Ok of Operation.response
    | Error.post] Deferred.t =
    let payload =
      Operation.request_to_yojson request in
    let path = path_to_string Operation.path in
     Request.make ~nonce
      ~request:path payload >>=
    fun request ->
    (Request.to_yojson request |>
     Yojson.Safe.pretty_to_string |>
     fun s ->
     Log.Global.info "request as json: %s" s;
     Log.Global.flushed () >>| fun () ->
     Auth.of_payload s
    )
    >>= fun payload ->
    let headers =
      Cohttp.Header.of_list
        ["Content-Type", "text/plain";
         "Content-Length", "0";
         "Cache-Control", "no-cache";
         "X-GEMINI-PAYLOAD", Auth.to_string payload;
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
        ~path
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
           Response.parse yojson Operation.response_of_yojson
        )
      )
    | `Not_found -> return `Not_found
    | `Not_acceptable ->
      Cohttp_async.Body.to_string body >>| fun body ->
      `Not_acceptable body
    | `Bad_request ->
      Cohttp_async.Body.to_string body >>| fun body ->
      `Bad_request body
    | `Unauthorized ->
      Cohttp_async.Body.to_string body >>| fun body ->
      `Unauthorized body
    | (code : Cohttp.Code.status_code) ->
      Cohttp_async.Body.to_string body >>| fun body ->
      failwiths (sprintf "unexpected status code (body=%S)" body)
        code Cohttp.Code.sexp_of_status_code

  let nonce_file =
    let root_path = Unix.getenv_exn "HOME" in
    sprintf "%s/.gemini/nonce.txt" root_path

  let command =
    let open Command.Let_syntax in
    (List.last_exn Operation.path,
     Command.async
       ~summary:(path_to_summary ~has_subcommands:false Operation.path)
       [%map_open
         let config = Cfg.param
         and request = anon ("request" %: sexp)
         in
         fun () ->
           let request = Operation.request_of_sexp request in
           Nonce.File.pipe ~init:nonce_file () >>= fun nonce ->
           post config nonce request >>= function
           | `Ok response ->
             Log.Global.info "response: %s"
               (Sexp.to_string
                  (Operation.sexp_of_response response)
               ); Log.Global.flushed ()
           | #Error.post as post_error ->
             failwiths
               (sprintf
                  "post for operation %S failed"
                  (path_to_string Operation.path)
               )
               post_error
               Error.sexp_of_post
       ]
    )

end

module V1 = struct
let path = ["v1"]

module Heartbeat = struct
  module T = struct
    let path = path@["heartbeat"]
    type request = unit [@@deriving sexp, yojson]
    type response = {result:bool} [@@deriving sexp, yojson]
  end
  include T
  include Service(T)
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
        (sprintf "expected json string but got %S"
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
  let path = path@["order"]

  module Status =
  struct
    module T = struct
      let path = path@["status"]

      type request = {
        order_id:(int64 [@encoding `string])
      } [@@deriving yojson, sexp]

      type response = {
        order_id : (int64 [@encoding `string]);
        id : (int64 [@encoding `string]);
        symbol : Symbol.t;
        exchange : Exchange.t;
        avg_execution_price : decimal;
        side : Side.t;
        type_ : Order_type.t [@key "type"];
        timestamp : string;
        timestampms : Timestamp.ms;
        is_live : bool;
        is_cancelled : bool;
        is_hidden : bool;
        was_forced : bool;
        executed_amount : decimal;
        remaining_amount : decimal;
        options : Order_execution_option.t list;
        price : decimal;
        original_amount : decimal;
      } [@@deriving yojson, sexp]
    end
    include T
    include Service(T)
  end

  module New = struct
    module T = struct
      let path = path@["new"]

      type request = {
        client_order_id:string;
        symbol:Symbol.t;
        amount:decimal;
        price:decimal;
        side:Side.t;
        type_:Order_type.t [@key "type"];
        options: Order_execution_option.t list;
      } [@@deriving sexp, yojson]

      type response = Status.response [@@deriving yojson, sexp]
    end
    include T
    include Service(T)

  end

  module Cancel = struct
    module T = struct
      let path = path@["cancel"]

      type request = {order_id:string} [@@deriving sexp, yojson]

      type response = Status.response [@@deriving sexp, yojson]
    end
    include T
    include Service(T)

    module All = struct
      module T = struct
        let path = path@["all"]
        type request = unit [@@deriving sexp, yojson]
        type response = {result:bool} [@@deriving sexp, yojson]
      end
      include T
      include Service(T)
    end

    module Session = struct
      module T = struct
        let path = path@["session"]
        type request = unit [@@deriving sexp, yojson]
        type response = {result:bool} [@@deriving sexp, yojson]
      end
      include T
      include Service(T)
    end

    let command : string * Command.t =
    (List.last_exn path,
     Command.group
       ~summary:(path_to_summary ~has_subcommands:true path)
       [command;
        Session.command;
        All.command
       ]
    )
  end

  let command : string * Command.t =
    (List.last_exn path,
     Command.group
       ~summary:(path_to_summary ~has_subcommands:true path)
       [New.command;
        Cancel.command;
        Status.command
       ]
    )

end

module Orders = struct

  module T = struct
  let path = path@["orders"]

  type request = unit [@@deriving sexp, yojson]
  type response =
    Order.Status.response list [@@deriving yojson, sexp]
  end
  include T
  include Service(T)
end

module Mytrades = struct

  type trade = {price:decimal;
                amount:decimal;
                timestamp:Timestamp.sec;
                timestampms:Timestamp.ms;
                type_: Side.t [@key "type"];
                aggressor: bool;
                fee_currency: string; (* TODO make enum *)
                fee_amount : decimal;
                order_id : string;
                client_order_id : string;
                is_auction_fill : bool;
                break : string option [@default None] (* TODO make enum *)
               } [@@deriving yojson, sexp]
  module T = struct
    let path = path@["mytrades"]
    type request =
      { symbol : Symbol.t;
        limit_trades: int option [@default None];
        timestamp: Timestamp.sec option [@default None]
      } [@@deriving sexp, yojson]
    type response = trade list [@@deriving yojson, sexp]
  end
  include T
  include Service(T)
end

module Tradevolume = struct

  module T = struct
    let path = path@["tradevolume"]
    type request = unit [@@deriving yojson, sexp]
    type response =
      {account_id:string;
       symbol:Symbol.t;
       base_currency:decimal;
       notional_currency:decimal;
       data_date:string;
       total_volume_base:decimal;
       maker_buy_sell_ratio:decimal;
       buy_maker_base:decimal;
       buy_maker_notional:decimal;
       buy_maker_count:decimal;
       sell_maker_base:decimal;
       sell_maker_notional:decimal;
       sell_maker_count:decimal;
       buy_taker_base:decimal;
       buy_taker_count:decimal;
       sell_taker_base:decimal;
       sell_taker_notional:decimal;
       sell_taker_count:decimal;
      } [@@deriving yojson, sexp]
  end
  include T
  include Service(T)
end

module Balances = struct

  module T = struct
    let path = path@["balances"]

    type request = unit [@@deriving yojson, sexp]
    type response =
      {currency:Symbol.t;
       amount:decimal;
       available:decimal;
       available_for_withdrawal:decimal [@key "availableForWithdrawal"];
       type_: Exchange.t [@key "type"]
      } [@@deriving yojson, sexp]
  end

  include T
  include Service(T)
end

let command : Command.t =
  Command.group
    ~summary:"Gemini Command System"
    [Heartbeat.command;
     Order.command;
     Orders.command;
     Mytrades.command;
     Tradevolume.command;
     Balances.command
    ]

end
