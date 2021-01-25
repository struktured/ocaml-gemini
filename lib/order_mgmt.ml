open Common
open V1

module Side = Market_data.Side

module Change_event = Market_data.Change_event

module V1_order = Order
type request = Order.New.request [@@deriving sexp]


module Order = struct
  type t =
           { client_order_id:string option;
             symbol: Symbol.t;
             side: Side.Bid_ask.t;
             price: Decimal_number.t;
             volume: Decimal_number.t;
             options: Order_execution_option.t list;
             type_: Order_type.t
           } [@@deriving sexp, compare, make]

  let zero ?client_order_id ?(options=[(`Immediate_or_cancel :> Order_execution_option.t)]) ?(type_=`Exchange_limit) ~symbol ~side () =
    make ?client_order_id ~symbol ~side ~volume:0.
      ~options ~type_ ~price:0. ()

  let of_request ({ client_order_id; symbol; amount; price; side; type_; options }:request) : t =
        {
          client_order_id = Some client_order_id;
          symbol;
          side = Side.Bid_ask.of_order_side side;
          price = Decimal_string.to_decimal_number price;
          volume = Decimal_string.to_decimal_number amount;
          options;
          type_
        }

  let to_request ?client_order_id ({ client_order_id=orig_client_order_id; symbol; volume;
                                     price; side; type_; options }:t) : request option =
    Option.(first_some client_order_id orig_client_order_id |> map ~f:
      (fun client_order_id ->
        ({
          client_order_id;
          symbol;
          side = Side.Bid_ask.to_order_side side;
          price = Decimal_number.to_decimal_string price;
          amount = Decimal_number.to_decimal_string volume;
          options;
          type_
        }:request)
      ))

end

module Side_price = struct
  type t = { side: Side.Bid_ask.t; price:Decimal_number.t } [@@deriving sexp, compare, make]
end


module By_side_price = Map.Make(Side_price)


module Order_book = struct
  type t = { book:Order.t By_side_price.t; symbol: Symbol.t } [@@deriving sexp, compare, make]

  let empty symbol : t = { book=By_side_price.empty; symbol}

  let update (t:t) (change_event:Change_event.t) =
   let side = change_event.side in
   let price = Decimal_string.to_decimal_number change_event.price in
   let key : Side_price.t = {side; price} in
   let volume = Decimal_string.to_decimal_number change_event.remaining in
   match Float.(equal zero volume) || Market_data.Reason.equal change_event.reason `Cancel with
   | true -> {t with book=By_side_price.remove t.book key}
   | false ->
     let book = By_side_price.update t.book key ~f:
      (function
        | Some order ->
          {order with price;volume}
        | None -> {
            client_order_id=None;
            price;
            volume; symbol=t.symbol;
            side;
            options = [];
            type_=`Exchange_limit
          }
      ) in
      {t with book}

  let at_volume ?(side=`Ask) ~(volume:Decimal_number.t) t =
    let avg_order = Order.zero ~symbol:t.symbol ~side () in
    By_side_price.fold ~init:([], avg_order) t.book ~f:(fun ~key:_ ~data:order (orders, avg_order) ->
        match Float.(>=) avg_order.volume volume with
        | true -> (orders, avg_order)
        | false ->
          match Side.Bid_ask.equal side order.side with
          | true ->
            let volume' = Float.(min (volume - avg_order.volume) order.volume) in
            let avg_price = avg_order.price +. volume' *. order.price in
            (order::orders,
             {avg_order with volume = volume' +. order.volume; price=avg_price})
          | false -> (orders, avg_order)
      ) |> fun (orders, (avg_order:Order.t)) ->
           (orders,
            {avg_order with price=avg_order.price /. avg_order.volume}
           )

  let at_price (t:t) ~(price:Decimal_number.t) ~(side:Side.Bid_ask.t) =
    let avg_order = Order.zero ~symbol:t.symbol ~side () in
    By_side_price.fold ~init:([], avg_order) t.book ~f:(fun ~key:_ ~data:order (orders, avg_order) ->
      match not (Side.Bid_ask.equal side order.side) && (Float.(<=) price order.price) with
      | true ->
        let avg_price = avg_order.price +. order.volume *. order.price in
        (order::orders, {avg_order with volume = avg_order.volume +. order.volume; price=avg_price})
      | false -> (orders, avg_order)
      ) |> fun (orders, (avg_order:Order.t)) ->
        (orders, {avg_order with price = avg_order.price /. avg_order.volume})

  let replace (t:t) (order:Order.t) =
    {t with book = By_side_price.update t.book {side=order.side; price=order.price} ~f:(function | None | Some _ -> order)}

end


module Order_books = struct
  module Map = Map.Make(Symbol)

  type t = Order_book.t Map.t [@@deriving sexp, compare]

  let empty = Map.empty

  let set (t:t) (order:Order.t) =
    Map.update t order.symbol ~f:
      (fun order_book ->
        let order_book = Option.value order_book ~default:(Order_book.empty order.symbol) in
        Order_book.replace order_book order
      )

  let update (t:t) currency (change_event:Change_event.t) =
    Map.update t currency ~f:
      (fun order_book ->
        let order_book = Option.value order_book ~default:(Order_book.empty currency) in
        Order_book.update order_book change_event
      )

  let order_book (t:t) (symbol:Symbol.t) = Map.find t symbol
end

module State = struct
  type t = { private_book: Order_books.t ;
             public_book: Order_books.t } [@@deriving sexp, compare]


  let empty = { private_book= Order_books.empty;
                public_book= Order_books.empty
              }
end


let submit_order (module Cfg:Cfg.S) (nonce) (_order:Order.t) =
  let request : V1_order.New.request = failwith "nyi" in
  V1_order.New.post (module Cfg) nonce request >>| function
  | `Ok _response -> failwith "nbyi"
  | #Rest.Error.post as e -> e


