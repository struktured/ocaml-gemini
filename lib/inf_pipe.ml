open Async

module T = struct

module Reader = struct
    type 'a t = 'a Pipe.Reader.t [@deriving sexp]
    let create (reader: 'a Pipe.Reader.t ) : 'a t = reader
end 

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

let unfold ~init ~f : 'a Reader.t =
    Pipe.unfold ~init ~f:(fun (acc : 's) ->
        f acc >>| fun (acc, s) -> Some (acc, s))

let map = Pipe.map
let filter_map = Pipe.filter_map

let to_pipe t : 'a Pipe.Reader.t = t
end

module type S = sig

  module Reader: sig
    type 'a t = private 'a Pipe.Reader.t
    val create : 'a Pipe.Reader.t  -> 'a t 
  end
  val read : ?consumer:Pipe.Consumer.t -> 'a Reader.t -> 'a Deferred.t
  val read_now :
    ?consumer:Pipe.Consumer.t ->
    'a Reader.t -> [> `Nothing_available | `Ok of 'a ]
  val read_exactly :
    ?consumer:Pipe.Consumer.t ->
    'a Reader.t -> num_values:int -> 'a Base.Queue.t Deferred.t
  val unfold : init:'s -> f:('s -> ('a * 's) Deferred.t) -> 'a Reader.t
  val map : 'a Reader.t -> f:('a -> 'b) -> 'b Reader.t
  val filter_map :
     ?max_queue_length:int -> 'a Reader.t -> f:('a -> 'b option) -> 'b Reader.t

  val to_pipe : 'a Reader.t -> 'a Pipe.Reader.t
end


include (T : S)
