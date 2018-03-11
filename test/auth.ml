open Gemini
open V1

let api_secret = "1234abcd"
let result =
  "ewogICAgICJyZXF1ZXN0IjogIi92MS9vcmRlci9zdGF0dXMiLAogICAgICJub25jZSI6IDEyMzQ1NiwKCiAgICAgIm9yZGVyX2lkIjogMTg4MzQKfQo=-"

let noonce () = Pipe.of_list [123456]

let request_str =
{|{
     "request": "/v1/order/status",
     "nonce": 123456,

     "order_id": 18834
}
|}

let hex_encoding_test () =
let noonce = noonce () in
(*let request = Order.Status.{order_id=18834} in
  let yojson = Order.Status.request_to_yojson request in
  Request.make ~request:"/v1/order/status" ~noonce yojson >>= fun request -> *)
  let payload_str = request_str
(*    Request.to_yojson request |>
    Yojson.Safe.to_string in *) in
  let hex = Auth.of_payload payload_str in
  printf "%s" (Auth.to_string hex);
  Deferred.unit

let%expect_test "hex encoding" =
  hex_encoding_test () >>= fun () ->
  [%expect "ewogICAgICJyZXF1ZXN0IjogIi92MS9vcmRlci9zdGF0dXMiLAogICAgICJub25jZSI6IDEyMzQ1NiwKCiAgICAgIm9yZGVyX2lkIjogMTg4MzQKfQo=-"]

