open Common
let path = [v1]

module T = struct
  let name = "order-events"
  let path = path@["order";"events"]
  type uri_args = [`None] [@@deriving sexp, enumerate]
  let default_uri_args = None
  let uri_args_to_string _ =
    failwith "uri path arguments not support for order events"

  let extra_headers (module Cfg:Cfg.S) ~payload =
    ["X-GEMINI-APIKEY", Cfg.api_key;
     "X-GEMINI-PAYLOAD", Auth.(of_payload payload |> to_string)
    ]

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

      type t = [ `Subscription_ack
               | `Heartbeat
               | `Initial
               | `Accepted
               | `Rejected
               | `Booked
               | `Fill
               | `Cancelled
               | `Closed
               ] [@@deriving sexp, enumerate]

      let to_string : t -> string = function
        | `Subscription_ack -> "subscription_ack"
        | `Heartbeat -> "heartbeat"
        | `Initial -> "initial"
        | `Accepted -> "accepted"
        | `Rejected -> "rejected"
        | `Booked -> "booked"
        | `Fill -> "fill"
        | `Cancelled -> "cancelled"
        | `Closed -> "closed"
    end
    include T
    include Json.Make(T)
  end

  type query = [ `Symbol_filter of Symbol.t
               | `Event_type_filter of Event_type.t
               | `Api_session_filter of string
               ] [@@deriving sexp]

  let encode_query = function
      | `Symbol_filter symbol ->
        "symbolFilter", Symbol.to_string symbol
      | `Event_type_filter event_type ->
        "eventTypeFilter", Event_type.to_string event_type
      | `Api_session_filter session ->
        "apiSessionFilter", session

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

  type order_event =
    {order_id:string;
     api_session:Side.t;
     client_order_id:string;
     symbol:Symbol.t;
     side:Side.t;
     behavior:string (* TODO make enum *);
     order_type:string (* TODO make enum *);
     timestamp:Timestamp.t;
     timestampms:Timestamp.ms;
     is_live : bool;
     is_cancelled : bool;
     is_hidden : bool;
     avg_execution_price : decimal_string;
     executed_amount : decimal_string;
     remaining_amount : decimal_string option [@default None];
     original_amount : decimal_string option [@default None];
     price : decimal_string option [@default None];
     total_spend : decimal_string option [@default None]
    } [@@deriving sexp, yojson]

  type event = order_event [@@deriving sexp]

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
            | _ ->
              order_event_of_yojson json'
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


