type t = [`Base64 of Cstruct.t | Hex.t]

let of_payload s : [< t > `Base64] = `Base64
    (Cstruct.of_string s |> Nocrypto.Base64.encode)

let hmac_sha384 ~api_secret (`Base64 payload) : [< t > `Hex] =
  let key = Cstruct.of_string api_secret in
  Nocrypto.Hash.SHA384.hmac ~key payload
  |> Hex.of_cstruct

let to_string : [<t] -> string = function
  | `Base64 cstruct -> Cstruct.to_string cstruct
  | `Hex hex -> hex


