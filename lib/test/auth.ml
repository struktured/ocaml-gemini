open Gemini
open Auth
open V1

let api_secret = "1234abcd"

let noonce () = Pipe.of_list ["123456"]

let%expect_test "hex encoding" =
   Request.make ~request:"/v1/order/status" ~noonce
     Order.Status.{order_id="18834"} >>= fun request ->
   let payload_str =
     Order.Status.yojson_of_request request
     |> Yojson.Safe.to_string in
   let hex = of_payload payload_str in
   printf "%s" (Auth.to_string hex);
   [%expect {| ewo  |}]
