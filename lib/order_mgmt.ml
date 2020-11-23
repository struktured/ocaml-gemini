open Common
module Side = Market_data.Side

module Change_event = Market_data.Change_event

module Order = struct
  type t = { currency: Currency.t;
             side: Side.Bid_ask.t;
             price: Decimal_number.t;
             volume: Decimal_number.t
           } [@@deriving sexp, compare]

end


module Side_price = struct
  type t = { side: Side.Bid_ask.t; price:Decimal_number.t } [@@deriving sexp, compare]
end


module By_side_price = Map.Make(Side_price)


module Order_book = struct
  type t = { book:Order.t By_side_price.t; currency: Currency.t }

  let empty currency : t = { book=By_side_price.empty; currency}

  let update (t:t) (change_event:Change_event.t) =
   let side = change_event.side in
   let price = Decimal_string.to_decimal_number change_event.price in
   let key : Side_price.t = {side; price} in
   let volume = Decimal_string.to_decimal_number change_event.remaining in
   match Float.(equal zero volume) || change_event.reason = `Cancel with
   | true -> {t with book=By_side_price.remove t.book key}
   | false ->
     let book = By_side_price.update t.book key ~f:
      (function
        | Some order ->
          {order with price;volume}
        | None -> {
            price;
            volume;
            currency=t.currency;
            side
          }
      ) in
      {t with book}

  let at_volume (t:t) ~(volume:Decimal_number.t) ~(side:Side.Bid_ask.t) =
    let avg_order : Order.t = {currency=t.currency;side; volume=0.;price=0.} in
    By_side_price.fold ~init:([], avg_order) t.book ~f:(fun ~key:_ ~data:order (orders, avg_order) ->
        match avg_order.volume >=. volume with
        | true -> (orders, avg_order)
        | false ->
          match side <> order.side with
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
    let avg_order : Order.t = {currency=t.currency;side; volume=0.;price=0.} in
    By_side_price.fold ~init:([], avg_order) t.book ~f:(fun ~key:_ ~data:order (orders, avg_order) ->
      match side <> order.side && (price <=. order.price) with
      | true ->
        let avg_price = avg_order.price +. order.volume *. order.price in
        (order::orders, {avg_order with volume = avg_order.volume +. order.volume; price=avg_price})
      | false -> (orders, avg_order)
      ) |> fun (orders, (avg_order:Order.t)) ->
        (orders, {avg_order with price = avg_order.price /. avg_order.volume})

  let submit_order (_order:Order.t) (module Cfg:Cfg.S) = failwith "nyi"
 (*    let request : Gemini.V1.Order.New.request = { !
    Gemini.V1.Order.New.post (module Cfg) *)

end
