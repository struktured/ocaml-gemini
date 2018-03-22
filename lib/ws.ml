
module W = Websocket_async

module type CHANNEL = sig
  val name : string
  val path : string list

  type response [@@deriving sexp, yojson]

end

module Make(Channel:CHANNEL) = struct
let client (module Cfg : Cfg.S) protocol extensions =
  let uri = Uri.make
      ~host:Cfg.api_host
      ~scheme:"wss"
      ~path:
        (String.concat ~sep:"/" Channel.path)
      ()
      in
  let host = Option.value_exn ~message:"no host in uri"
      Uri.(host uri) in
  let port = Option.value_exn ~message:"no port inferred from scheme"
      Uri_services.(tcp_port_of_uri uri) in
  let scheme = Option.value_exn ~message:"no scheme in uri"
      Uri.(scheme uri) in
  let tcp_fun s r w =
    Socket.(setopt s Opt.nodelay true);
    (if scheme = "https" || scheme = "wss" then
       Conduit_async_ssl.
         (ssl_connect (Ssl_config.configure ~version:Tlsv1_2 ()) r w)
     else return (r, w)) >>= fun (r, w) ->
    let module C = Cohttp in
    let extra_headers = C.Header.init () in
    let extra_headers = Option.value_map protocol ~default:extra_headers
        ~f:(fun proto ->
            C.Header.add
              extra_headers "Sec-Websocket-Protocol" proto)
    in
    let extra_headers = Option.value_map extensions ~default:extra_headers
        ~f:(fun exts ->
            C.Header.add
              extra_headers "Sec-Websocket-Extensions" exts)
    in
    let r, w = W.client_ez
        ~extra_headers
        ~log:Lazy.(force Log.Global.log)
        ~heartbeat:Time_ns.Span.(of_int_sec 5)
        uri s r w
    in
    Deferred.all_unit [
      Pipe.transfer Reader.(pipe @@ Lazy.force stdin) w ~f:begin fun s ->
        String.chop_suffix_exn s ~suffix:"\n"
      end;
      Pipe.transfer r Writer.(pipe @@ Lazy.force stderr) ~f:(fun s -> s ^ "\n")
    ]
  in
  let hostport = Host_and_port.create host port in
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
    | `Ok ({ W.Frame.opcode; extension; final; content } as frame) ->
      let open W.Frame in
      Log.Global.debug "<- %s" W.Frame.(show frame);
      let frame', closed =
        match opcode with
        | Opcode.Ping -> Some (create ~opcode:Opcode.Pong ~content ()), false
        | Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          if String.length content >= 2 then
            Some (create ~opcode:Opcode.Close
                          ~content:(String.sub content 0 2) ()), true
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
    begin W.server ~log:Lazy.(force Log.Global.log)
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
    +> flag "-s" no_arg ~doc:" Run as server (default: no)"
  in
  let set_loglevel = function
    | 2 -> Log.Global.set_level `Info
    | 3 -> Log.Global.set_level `Debug
    | _ -> ()
  in
  let run cfg
      protocol extension loglevel is_server () =
    let cfg = Cfg.get cfg in
    let module Cfg = (val cfg:Cfg.S) in
    let uri = Uri.make
      ~host:Cfg.api_host
      ~scheme:"https"
      ~path:
        (String.concat ~sep:"/" Channel.path)
      () in
    Option.iter loglevel ~f:set_loglevel;
    match is_server with
    | false -> client (module Cfg) protocol extension
    | true ->
      let port = Option.value_exn
          ~message:"no port inferred from scheme"
          Uri_services.(tcp_port_of_uri uri) in
      Tcp.(Server.create
             ~on_handler_error:`Ignore
             Where_to_listen.(of_port port) handle_client) >>=
      Tcp.Server.close_finished
  in
  Command.async_spec
    ~summary:"Gemini %s Websocket" spec run
end

