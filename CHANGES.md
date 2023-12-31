## 0.3.0 (2031-2-30)
 - Update to OCaml 5
 - Add support for many more symbols and currencies
 - Gracefully handle unknown currencies using new `Enum_or_string` abtraction
 - Websocket pipes now retain result types when processing json rather than raising
   exceptions inside the pipe (breaking change).
 - Switch from `websockets-async` to simpler, better supported `cohttp_async_websocket`.
 - Fix various parse errors due to new gemini fields.
 - Format the codebase with ocamlformat

## 0.2.1 (2019-03-03)

- Write csv headers if target file is empty or does not exist.

## 0.2.0 (2019-02-08)

- Csv serialization support for web socket apis
- Added reject `reason` field to order event type
- Added block trade event type to market data api

## 0.1.0 (2019-01-20)

- Initial opam release.
