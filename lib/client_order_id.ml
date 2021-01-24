type reader = string Pipe.Reader.t

module type S = sig
  type t [@@deriving sexp]

  val pipe : init:t -> unit -> reader Deferred.t
end

module Counter : S with type t = int = struct
  type t = int [@@deriving sexp]
  let pipe ~init () =
    Pipe.unfold ~init
      ~f:
        (fun s ->
           let s' = s + 1 in
           Some (Int.to_string s, s') |> return
        )
    |> return
end

module File = struct

  let create_file ?(default=0) filename =
    try_with ~extract_exn:true (fun () ->
        Unix.with_file ~mode:[`Rdonly] filename
          ~f:(fun _fd -> Deferred.unit)
      ) >>= function
    | Result.Ok _ -> Deferred.unit
    | Result.Error _ ->
      Writer.save ~contents:(sprintf "%d\n" default) filename

  type t = string [@@deriving sexp]


  (* TODO - too expensive- only check the file once, then do
   * everything mostly in memory. Also not concurrent-safe if running w/same user *)
  let pipe ~init:filename () =
    Cfg.create_config_dir () >>= fun () ->
    create_file ?default:None filename >>= fun () ->
    Pipe.unfold ~init:() ~f:
      (fun () ->
         Reader.open_file ?buf_len:None
           filename >>= Reader.really_read_line
           ~wait_time:(Time.Span.of_ms 1.0) >>=
         (function
           | None -> return @@ Int.to_string 0
           | Some nonce -> return nonce
         ) >>= fun nonce ->
         let nonce' = Int.of_string nonce + 1 in
         Writer.save filename
           ~contents:(sprintf "%d\n" nonce') >>= fun () ->
         return @@ Some (nonce, ())
      ) |> return

  let default_filename =
    let root_path = Unix.getenv_exn "HOME" in
    sprintf "%s/.gemini/client_order_id.txt" root_path

end

let _assert_module_file_is_nonce =
  let module F = (File : S with type t = string) in ()

