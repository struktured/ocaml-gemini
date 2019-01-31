open Common
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
    include (Json.Make(T) : Json.S with type t := t)
  end

  module Auction = struct
    module T = struct
      type t = [`Auction] [@@deriving sexp, enumerate]
      let to_string = function
          `Auction -> "auction"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

  module T = struct
    type t = [Bid_ask.t | Auction.t] [@@deriving sexp, enumerate]
    let to_string : [<t] -> string = function
      | #Bid_ask.t as bid_ask -> Bid_ask.to_string bid_ask
      | #Auction.t as auction -> Auction.to_string auction
  end
  include T
  include (Json.Make(T) : Json.S with type t := t)
end

module T = struct
  let name = "marketdata"
  let version = "v1"
  let path = v1::["marketdata"]
  type uri_args = Symbol.t [@@deriving sexp, yojson, enumerate]

  let authentication = `Public
  let default_uri_args = Some `Ethusd
  let encode_uri_args = Symbol.to_string

  type query = unit [@@deriving sexp]
  let encode_query _ = failwith "queries not supported"

  module Message_type = struct
    module T = struct
      type t = [`Update | `Heartbeat] [@@deriving sexp, enumerate]
      let to_string = function
        | `Update -> "update"
        | `Heartbeat -> "heartbeat"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
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
    include (Json.Make(T) : Json.S with type t := t)
  end

  type heartbeat = unit [@@deriving sexp, of_yojson]

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
    include (Json.Make(T) : Json.S with type t := t)
  end

  module Change_event = struct
     type t =
       {price:Decimal_string.t;
        side:Side.Bid_ask.t;
        reason:Reason.t;
        remaining:Decimal_string.t;
        delta:Decimal_string.t
    } [@@deriving sexp, of_yojson, fields, csv]
  end


  type trade_event =
    {tid:Int_number.t;
     price:Decimal_string.t;
     amount:Decimal_string.t;
     maker_side:Side.t [@key "makerSide"]
    } [@@deriving of_yojson, sexp]

  type auction_open_event =
    {auction_open_ms:Timestamp.ms;
     auction_time_ms:Timestamp.ms;
     first_indicative_ms:Timestamp.ms;
     last_cancel_time_ms:Timestamp.ms
    } [@@deriving sexp, of_yojson]


  module Auction_result = struct
    module T = struct
      type t =
        [`Success | `Failure] [@@deriving sexp, enumerate]
      let to_string = function
        | `Success -> "success"
        | `Failure -> "failure"
    end
    include T
    include (Json.Make(T) : Json.S with type t := t)
  end

  type auction_indicative_price_event =
    {eid:Int_number.t;
     result:Auction_result.t;
     time_ms:Timestamp.ms;
     highest_bid_price:Decimal_string.t;
     lowest_ask_price:Decimal_string.t;
     collar_price:Decimal_string.t;
     indicative_price:Decimal_string.t;
     indicative_quantity:Decimal_string.t
    } [@@deriving sexp, of_yojson]


  type auction_outcome_event =
    {eid:Int_number.t;
     result:Auction_result.t;
     time_ms:Timestamp.ms;
     highest_bid_price:Decimal_string.t;
     lowest_ask_price:Decimal_string.t;
     collar_price:Decimal_string.t;
     auction_price:Decimal_string.t;
     auction_quantity:Decimal_string.t
    } [@@deriving sexp, of_yojson]

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
    include (Json.Make(T) : Json.S with type t := t)
  end

  type auction_event =
    [
      | `Auction_open of auction_open_event
      | `Auction_indicative_price of
          auction_indicative_price_event
      | `Auction_outcome of auction_outcome_event
    ] [@@deriving sexp]

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
    [ `Change of Change_event.t
    | `Trade of trade_event
    | `Auction of auction_event
    ] [@@deriving sexp]

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
              Change_event.of_yojson json' |>
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
    { event_id : Int_number.t [@key "eventId"];
      events : event array;
      timestamp : Timestamp.sec option [@default None];
      timestampms : Timestamp.ms option [@default None]
    } [@@deriving sexp, of_yojson]

  type message =
    [`Heartbeat of heartbeat | `Update of update] [@@deriving sexp]

  type response =
    {
      socket_sequence : Int_number.t;
      message : message
    } [@@deriving sexp]

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
            (Int_number.of_yojson socket_sequence)
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
include Ws.Make_no_request(T)


