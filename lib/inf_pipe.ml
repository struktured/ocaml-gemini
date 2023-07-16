open Async

module T = struct
  module Reader = struct
    type 'a t = 'a Pipe.Reader.t

    let create (reader : 'a Pipe.Reader.t) : 'a t = reader
  end

  module Writer = struct
    type 'a t = 'a Pipe.Writer.t

    let create (writer : 'a Pipe.Writer.t) : 'a t = writer
  end

  let create = Pipe.create

  let read ?consumer t =
    Pipe.read ?consumer t >>| function `Ok x -> x | `Eof -> assert false

  let read_now ?consumer t =
    Pipe.read_now ?consumer t |> function
    | `Ok _ as ok -> ok
    | `Nothing_available as na -> na
    | `Eof -> assert false

  let read_exactly ?consumer t ~num_values =
    Pipe.read_exactly ?consumer t ~num_values >>| function
    | `Exactly e -> e
    | `Fewer _ | `Eof -> assert false

  let to_pipe t : 'a Pipe.Reader.t = t
  let interleave l = Pipe.interleave l |> Reader.create

  let unfold ~init ~f : 'a Reader.t =
    Pipe.unfold ~init ~f:(fun (acc : 's) ->
        f acc >>| fun (acc, s) -> Some (acc, s))

  let map = Pipe.map
  let filter_map = Pipe.filter_map
  let folding_map = Pipe.folding_map
  let folding_filter_map = Pipe.folding_filter_map
  let folding_filter_map' = Pipe.folding_filter_map'
  let transfer = Pipe.transfer
end

module type S = sig
  module Reader : sig
    type 'a t = private 'a Pipe.Reader.t

    val create : 'a Pipe.Reader.t -> 'a t
  end

  module Writer : sig
    type 'a t = private 'a Pipe.Writer.t

    val create : 'a Pipe.Writer.t -> 'a t
  end

  val create : ?info:Sexp.t -> unit -> 'a Reader.t * 'a Writer.t
  val read : ?consumer:Pipe.Consumer.t -> 'a Reader.t -> 'a Deferred.t

  val read_now :
    ?consumer:Pipe.Consumer.t ->
    'a Reader.t ->
    [> `Nothing_available | `Ok of 'a ]

  val read_exactly :
    ?consumer:Pipe.Consumer.t ->
    'a Reader.t ->
    num_values:int ->
    'a Base.Queue.t Deferred.t

  val unfold : init:'s -> f:('s -> ('a * 's) Deferred.t) -> 'a Reader.t
  val map : 'a Reader.t -> f:('a -> 'b) -> 'b Reader.t

  val filter_map :
    ?max_queue_length:int -> 'a Reader.t -> f:('a -> 'b option) -> 'b Reader.t

  val interleave : 'a Reader.t list -> 'a Reader.t
  val to_pipe : 'a Reader.t -> 'a Pipe.Reader.t

  val folding_map :
    ?max_queue_length:int ->
    'a Reader.t ->
    init:'accum ->
    f:('accum -> 'a -> 'accum * 'c) ->
    'c Reader.t

  val folding_filter_map :
    ?max_queue_length:int ->
    'a Reader.t ->
    init:'accum ->
    f:('accum -> 'a -> 'accum * 'c option) ->
    'c Reader.t

  val folding_filter_map' :
    ?max_queue_length:int ->
    'a Reader.t ->
    init:'accum ->
    f:('accum -> 'a -> ('accum * 'c option) Deferred.t) ->
    'c Reader.t

  val transfer : 'a Reader.t -> 'b Writer.t -> f:('a -> 'b) -> unit Deferred.t
end

include (T : S)
