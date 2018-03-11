open Gemini
open V1

let api_secret = "1234abcd"

let noonce () = Pipe.of_list ["123456"]

let%expect_test "hex encoding" =
  let noonce = noonce () in

  let request = Order.Status.{order_id="18834"} in
  let yojson = Order.Status.request_to_yojson request in
  Request.make ~request:"/v1/order/status" ~noonce yojson >>= fun request ->
  let payload_str =
    Request.to_yojson request |>
    Yojson.Safe.to_string in
  let hex = Auth.of_payload payload_str in
  printf "%s" (Auth.to_string hex);
  [%expect {| ewo  |}]
