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
