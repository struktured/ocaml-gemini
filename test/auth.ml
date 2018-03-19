
open Gemini
open Gemini.V1

let api_secret = "1234abcd"

let request_str =
{|{
     "request": "/v1/order/status",
     "nonce": 123456,

     "order_id": 18834
}
|}

let hex_encoding_test () =
  let hex = Auth.of_payload request_str in
  printf "%s" (Auth.to_string hex);
  Deferred.unit


let hmac384_encrypting_test () =
  let hex = Auth.of_payload request_str in
  let hmac = Auth.hmac_sha384 ~api_secret hex in
  let str = Auth.to_string hmac in
  printf "%s" str;
  Deferred.unit


let%expect_test "hex encoding" =
  hex_encoding_test () >>= fun () ->
  [%expect "ewogICAgICJyZXF1ZXN0IjogIi92MS9vcmRlci9zdGF0dXMiLAogICAgICJub25jZSI6IDEyMzQ1NiwKCiAgICAgIm9yZGVyX2lkIjogMTg4MzQKfQo=-"]

let%expect_test "hmac384 encrypting" =
  hmac384_encrypting_test () >>= fun () ->
  [%expect "337cc8b4ea692cfe65b4a85fcc9f042b2e3f702ac956fd098d600ab15705775017beae402be773ceee10719ff70d710f"]
