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
        ~f:(fun _fd ->  Deferred.unit)
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


