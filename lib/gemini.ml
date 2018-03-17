open Core
open Async


type int_number = int64 [@encoding `number] [@@deriving sexp, yojson]
type int_string = int64 [@encoding `string] [@@deriving sexp, yojson]
type decimal_number = float [@encoding `number] [@@deriving sexp, yojson]
type decimal_string = string [@@deriving yojson, sexp]

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

let path_to_summary ~has_subnames
    path = sprintf "Gemini %s Command%s"
    (String.concat ~sep:" " path)
    (match has_subnames with
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

  let of_string s =
    match String.lowercase s with
      | "production" ->
        let module Cfg : S = Production () in
        (module Cfg : S)
      | "sandbox" ->
        let module Cfg : S = Sandbox () in
        (module Cfg : S)
      | unsupported_env ->
        failwithf "environment %s not supported"
          unsupported_env ()

  let arg_type = Command.Arg_type.create of_string
  let param =
    Command.Param.(
      flag "-cfg" (optional arg_type)
        ~doc:(
          sprintf "STRING the configuration the client will connect with \
                   (eg. sandbox or production. defaults to sandbox). Use \
                    GEMINI_ENV to override the default value."
        )
    )

  let get param =
    match param with
    | None ->
      (match Unix.getenv "GEMINI_ENV" with
      | Some env -> of_string env
      | None -> (module Sandbox())
      )
    | Some param -> param


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
    val name : string
    val path : string list
    type request [@@deriving sexp]
    type response [@@deriving sexp]
    val request_to_yojson : request -> Yojson.Safe.json
    val response_of_yojson : Yojson.Safe.json ->
      (response, string) Result.t
  end

  module type S_NO_ARG = sig
    include S with type request = unit
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

module Post(Operation:Operation.S) =
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
     Log.Global.debug "request as json:\n %s" s;
     return @@ Auth.of_payload s
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
           Log.Global.debug "result as json:\n %s" s;
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
end

let nonce_file =
  let root_path = Unix.getenv_exn "HOME" in
  sprintf "%s/.gemini/nonce.txt" root_path


module Rest(Operation:Operation.S) =
struct
  include Post(Operation)
  let command =
    let open Command.Let_syntax in
    (Operation.name,
     Command.async
       ~summary:(path_to_summary ~has_subnames:false Operation.path)
       [%map_open
         let config = Cfg.param
         and request = anon ("request" %: sexp)
         in
         fun () ->
           let request = Operation.request_of_sexp request in
           Log.Global.info "request:\n %s"
             (Operation.sexp_of_request request |> Sexp.to_string);
           let config = Cfg.get config in
           Nonce.File.pipe ~init:nonce_file () >>= fun nonce ->
           post config nonce request >>= function
           | `Ok response ->
             Log.Global.info "response:\n %s"
               (Sexp.to_string_hum
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

module Rest_no_arg(Operation:Operation.S_NO_ARG) =
struct
  include Post(Operation)

  let command =
    let open Command.Let_syntax in
    (Operation.name,
     Command.async
       ~summary:(path_to_summary ~has_subnames:false Operation.path)
       [%map_open
         let config = Cfg.param in
         fun () ->
           let request = () in
           let config = Cfg.get config in
           Nonce.File.pipe ~init:nonce_file () >>= fun nonce ->
           post config nonce request >>= function
           | `Ok response ->
             Log.Global.info "response:\n %s"
               (Sexp.to_string_hum
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
    let name = "heartbeat"
    let path = path@["heartbeat"]
    type request = unit [@@deriving sexp, yojson]
    type response = {result:bool [@default true]} [@@deriving sexp, yojson]
  end
  include T
  include Rest_no_arg(T)
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
    include Rest(T)
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
    include Rest(T)

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
    include Rest(T)
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
      include Rest_no_arg(T)
    end

    module Session = struct
      module T = struct
        let name = "session"
        let path = path@["session"]
        type request = unit [@@deriving sexp, yojson]
        type response = {details:details} [@@deriving sexp, yojson]
      end
      include T
      include Rest_no_arg(T)
    end

    let command : string * Command.t =
    (name,
     Command.group
       ~summary:(path_to_summary ~has_subnames:true path)
       [By_order_id.command;
        Session.command;
        All.command
       ]
    )
  end

  let command : string * Command.t =
    (name,
     Command.group
       ~summary:(path_to_summary ~has_subnames:true path)
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
  include Rest_no_arg(T)
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
  include Rest(T)

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
  include Rest_no_arg(T)
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
  include Rest_no_arg(T)
end


module Websocket = struct
 


  module type CHANNEL = sig
    val path : string list
    val name : string
  end

  let client (module Cfg:Cfg.S) (module Channel : CHANNEL) () =
    let uri = Uri.make ~scheme:"wss" ~host:Cfg.api_host
        ~path:(path_to_string Channel.path) () in
    let extra_headers = None in
    (*let initialized = None in
    let random_string = None in
    let app_to_ws:Websocket_async.Frame.t Pipe.Reader.t =
      failwith "nyi" in
    let ws_to_app:Websocket_async.Frame.t Pipe.Writer.t =
      failwith "nyi" in
    let net_to_ws:Reader.t =
      failwith "nyi" in
    let ws_to_net:Writer.t =
      failwith "nyi" in *)
    let socket = Socket.create Socket.Type.udp in
    let writer = Writer.create (Socket.fd socket) in
    let reader = Reader.create (Socket.fd socket) in
    Websocket_async.client_ez
    ~log:(Lazy.force Log.Global.log)
    ~name:Channel.name
    ?extra_headers
    (*~app_to_ws
    ~ws_to_app
    ~net_to_ws
    ~ws_to_net *)
    uri
    socket
    reader
    writer

   module Market_data = struct

    let name = "marketdata"
    let path = path@["marketdata"]

    module Side =
    struct
      type bid_ask = [`Bid | `Ask] [@@deriving sexp, yojson]
      type auction = [`Auction] [@@deriving sexp, yojson]
      type t = [bid_ask | auction] [@@deriving sexp, yojson]
    end

    type request = Symbol.t [@@deriving sexp, yojson]
    type url_params =
      { heartbeat : bool option [@default None] } [@@deriving sexp, yojson]

    type message_type = [`Update | `Heartbeat] [@@deriving sexp, yojson]

    type response =
      { message_type : message_type [@key "type"];
        socket_sequence: int_number
      } [@@deriving sexp, yojson]

    type event_type = [`Trade| `Change| `Auction] [@@deriving sexp, yojson]

    type heartbeat = unit [@@deriving sexp, yojson]

    module Reason = struct
      type t = [`Place | `Trade | `Cancel | `Initial] [@@deriving sexp, yojson]
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


    type auction_result = [`Success | `Failure] [@@deriving sexp, yojson]

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
       | `Auction_indicative_price of auction_indicative_price_event
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

    let subscribe (module Cfg : Cfg.S) (symbol:Symbol.t) =
      let _path = path@[Symbol.to_string symbol] in
      Deferred.unit



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
     Balances.command
    ]

end
