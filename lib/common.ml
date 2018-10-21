module Auth = Auth
module Result = Json.Result

type int_number = int64 [@encoding `number] [@@deriving sexp, yojson]
type int_string = int64 [@encoding `string] [@@deriving sexp, yojson]
type decimal_number = float [@encoding `number] [@@deriving sexp, yojson]
type decimal_string = string [@@deriving sexp, yojson]



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


module Symbol = struct
  module T = struct
    type t =
      [ `Btcusd | `Ethusd | `Ethbtc
      | `Zecusd | `Zecbtc | `Zeceth
      | `Ltcusd | `Ltcbtc | `Ltceth
      ]
    [@@deriving sexp, enumerate]

    let to_string : [<t] -> string = function
      | `Btcusd -> "btcusd"
      | `Ethusd -> "ethusd"
      | `Ethbtc -> "ethbtc"
      | `Zecusd -> "zecusd"
      | `Zecbtc -> "zecbtc"
      | `Zeceth -> "zeceth"
      | `Ltcusd -> "ltcusd"
      | `Ltcbtc -> "ltcbtc"
      | `Ltceth -> "ltceth"


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

module Timestamp = struct

  type t = Time.t [@@deriving sexp]

  type ms = t [@@deriving sexp]
  type sec = t [@@deriving sexp]

  let to_yojson t =
    Time.to_span_since_epoch t |>
    Time.Span.to_ms |>
    Float.to_string_hum ~decimals:0 |>
    fun s -> `String s

  let of_yojson' span_fn json =
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


  let of_yojson (ms:Yojson.Safe.json) =
    of_yojson' Time.Span.of_ms ms

  let ms_of_yojson = of_yojson

  let ms_to_yojson (ms:ms) = to_yojson ms

  let sec_of_yojson (sec:Yojson.Safe.json) =
    of_yojson' Time.Span.of_sec sec
  let sec_to_yojson (sec:sec) = to_yojson sec

end

module Currency = struct

  module T = struct
    type t = [`Eth | `Btc | `Usd | `Zec] [@@deriving sexp, enumerate]
    let to_string = function
      | `Eth -> "eth"
      | `Btc -> "btc"
      | `Usd -> "usd"
      | `Zec -> "zec"
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


let v1 = "v1"
