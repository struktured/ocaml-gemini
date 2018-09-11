module type CHANNEL = sig
  val name : string
  val path : string list

  type uri_args [@@deriving sexp, enumerate]
  val uri_args_to_string : uri_args -> string
  type response [@@deriving sexp, yojson]

end

module Make(Channel:CHANNEL) = struct
let client (module Cfg : Cfg.S) ?protocol ?extensions uri_args =
  let uri = Uri.make
      ~host:Cfg.api_host
      ~scheme:"https"
      ~path:
        (String.concat ~sep:"/"
           (Channel.path
            @
            [(Channel.uri_args_to_string uri_args)]
           )
        )
      ()
      in
  Log.Global.info "Ws.client: uri=%s"
    (Uri.to_string uri);
  let host = Option.value_exn ~message:"no host in uri"
      Uri.(host uri) in
  let port = Option.value ~default:443
      Uri_services.(tcp_port_of_uri uri) in
  let scheme = Option.value ~default:"wss"
      Uri.(scheme uri) in
  let tcp_fun s r w =
    Socket.(setopt s Opt.nodelay true);
    (if scheme = "https" || scheme = "wss" then
       (Unix.Inet_addr.of_string_or_getbyname host >>|
        Ipaddr_unix.of_inet_addr
       ) >>= fun addr ->
       (*`OpenSSL_with_config of string * Ipaddr.t * int * Ssl.config*)
       Conduit_async.connect
         (`OpenSSL_with_config (
             "wss",
             addr,
             port,
             (Conduit_async.Ssl.configure ~version:Tlsv1_2 ())
           )
         )
     else return (r, w)) >>= fun (r, w) ->
    let extra_headers = Cohttp.Header.init () in
    let extra_headers = Option.value_map protocol ~default:extra_headers
        ~f:(fun proto ->
            Cohttp.Header.add
              extra_headers "Sec-Websocket-Protocol" proto)
    in
    let extra_headers = Option.value_map extensions ~default:extra_headers
        ~f:(fun exts ->
            Cohttp.Header.add
              extra_headers "Sec-Websocket-Extensions" exts)
    in
    let r, w = Websocket_async.client_ez
        ~extra_headers
        ~log:Lazy.(force Log.Global.log)
        ~heartbeat:Time_ns.Span.(of_int_sec 5)
        uri r w
    in Deferred.both
      (* TODO decide what to do with input pipe *)
      (Pipe.transfer Reader.(pipe @@ Lazy.force stdin) w ~f:begin fun s ->
        String.chop_suffix_exn s ~suffix:"\n"
      end)
      (Pipe.transfer
         (Pipe.map r ~f:(fun s ->
              Yojson.Safe.from_string s
              |> Channel.response_of_yojson
              |> Result.ok_or_failwith
            |> Channel.sexp_of_response |>
            fun s -> sprintf "%s\n" (Sexp.to_string_hum s))
         )
         Writer.(pipe @@ Lazy.force stderr)
         ~f:Fn.id
      )
    (*]*)
  in
  let hostport = Host_and_port.create ~host ~port in
  Tcp.(with_connection Where_to_connect.(of_host_and_port hostport) tcp_fun)

let handle_client addr reader writer =
  let addr_str = Socket.Address.(to_string addr) in
  Log.Global.info "Client connection from %s" addr_str;
  let app_to_ws, sender_write = Pipe.create () in
  let receiver_read, ws_to_app = Pipe.create () in
  let check_request req =
    let req_str = Format.asprintf "%a" Cohttp.Request.pp_hum req in
    Log.Global.info "Incoming connnection request: %s" req_str ;
    Deferred.return (Cohttp.Request.(uri req |> Uri.path) = "/ws")
  in
  let rec loop () =
    Pipe.read receiver_read >>= function
    | `Eof ->
      Log.Global.info "Client %s disconnected" addr_str;
      Deferred.unit
    | `Ok ({ Websocket_async.Frame.opcode;
             extension=_; final=_; content } as frame
          ) ->
      let open Websocket_async.Frame in
      Log.Global.debug "<- %s" (show frame);
      let frame', closed =
        match opcode with
        | Opcode.Ping -> Some (create ~opcode:Opcode.Pong ~content ()), false
        | Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          if String.length content >= 2 then
            Some (create ~opcode:Opcode.Close
                          ~content:(String.sub content ~pos:0 ~len:2) ()), true
          else
          Some (close 100), true
        | Opcode.Pong -> None, false
        | Opcode.Text
        | Opcode.Binary -> Some frame, false
        | _ -> Some (close 1002), false
      in
      begin
        match frame' with
        | None ->
          Deferred.unit
        | Some frame' ->
          Log.Global.debug "-> %s" (show frame');
          Pipe.write sender_write frame'
      end >>= fun () ->
      if closed then Deferred.unit
      else loop ()
  in
  Deferred.any [
    begin Websocket_async.server ~log:Lazy.(force Log.Global.log)
        ~check_request ~app_to_ws ~ws_to_app ~reader ~writer () >>= function
      | Error err when Error.to_exn err = Exit -> Deferred.unit
      | Error err -> Error.raise err
      | Ok () -> Deferred.unit
    end ;
    loop () ;
  ]

let command =
  let spec =
    let open Command.Spec in
    empty
    +> Cfg.param
    +> flag "-protocol" (optional string)
      ~doc:"str websocket protocol header"
    +> flag "-extensions" (optional string)
      ~doc:"str websocket extensions header"
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
    +> anon ("uri_args" %: sexp)
  in
  let set_loglevel = function
    | 2 -> Log.Global.set_level `Info
    | 3 -> Log.Global.set_level `Debug
    | _ -> ()
  in
  let run cfg
      protocol extensions loglevel uri_args () =
    let cfg = Cfg.get cfg in
    let module Cfg = (val cfg:Cfg.S) in
    Option.iter loglevel ~f:set_loglevel;
    let uri_args = Channel.uri_args_of_sexp uri_args in
    client (module Cfg) ?protocol ?extensions uri_args >>= fun _ ->
    Deferred.unit
  in
  Channel.name,
  Command.async_spec
    ~summary:(sprintf "Gemini %s Websocket Command" Channel.name) spec run
end

