module Error : sig
  type http = [ `Bad_request of string
              | `Not_found
              | `Service_unavailable of string
              | `Not_acceptable of string
              | `Unauthorized of string] [@@deriving sexp]
  type json_error = {message:string;body:string} [@@deriving sexp]
  type json = [`Json_parse_error of json_error] [@@deriving sexp]

  type detail = {reason:string;message:string} [@@deriving sexp, yojson]
  type response = [`Error of detail] [@@deriving sexp]

  type post = [http|json|response] [@@deriving sexp]
end

module Request = Nonce.Request

module Operation : sig

  module type S = sig
    val name : string
    val path : string list
    type request [@@deriving sexp, to_yojson]
    type response [@@deriving sexp, of_yojson]
  end

  module type S_NO_ARG = sig
    include S with type request = unit
  end

  type status= [`Ok | `Error of string]

end

module Response :
sig
  module Json_result :
  sig
    type t = [ `Error | `Ok ] [@@derivin sexp, yojson, enumerate]
    val to_string : [< t] -> string
    val dict : (string * t) sexp_list
    val of_string : string -> t option
    val split :
      [< Yojson.Safe.json ] ->
      (t option * [> Yojson.Safe.json ], string) result
  end
  type result_field =
    { result : Json_result.t; } [@@deriving sexp, of_yojson]
  type t =
    { result : Json_result.t; payload : Yojson.Safe.json; }
  val parse :
    Yojson.Safe.json ->
    ([> Yojson.Safe.json ] -> ('a, string) result) ->
    [ `Error of Error.detail
    | `Json_parse_error of Error.json_error
    | `Ok of 'a
    ]
end
module Post :
  functor (Operation : Operation.S) ->
  sig
    val post :
      (module Cfg.S) ->
      Nonce.reader ->
      Operation.request ->
      [ `Ok of Operation.response
      | Error.post
      ] Deferred.t
  end

module Make :
  functor (Operation : Operation.S) ->
  sig
    val post :
      (module Cfg.S) ->
      Nonce.reader ->
      Operation.request ->
      [`Ok of Operation.response
      | Error.post
      ] Deferred.t
    val command : string * Core.Command.t
  end

module Make_no_arg :
  functor (Operation : Operation.S_NO_ARG) ->
  sig
    val post :
      (module Cfg.S) ->
      Nonce.reader ->
      unit ->
      [ Error.post
      | `Ok of Operation.response
      ] Deferred.t
    val command : string * Core.Command.t
  end


