
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
    let path = Path.to_string Operation.path in
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


module Make(Operation:Operation.S) =
struct
  include Post(Operation)
  let command =
    let open Command.Let_syntax in
    (Operation.name,
     Command.async
       ~summary:(Path.to_summary ~has_subnames:false Operation.path)
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
                  (Path.to_string Operation.path)
               )
               post_error
               Error.sexp_of_post
       ]
    )

end

module Make_no_arg(Operation:Operation.S_NO_ARG) =
struct
  include Post(Operation)

  let command =
    let open Command.Let_syntax in
    (Operation.name,
     Command.async
       ~summary:(Path.to_summary ~has_subnames:false Operation.path)
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
                  (Path.to_string Operation.path)
               )
               post_error
               Error.sexp_of_post
       ]
    )

end


