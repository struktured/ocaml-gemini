# Introduction

This library implements the Gemini v1 REST and Market Data web sockets
services. It is backed by cohttp-async and websockets-async to do the heavy
lifting. A provisional console interface is also provided using s-expressions to
encode request parameters.

# Installation

From your project directory run

`opam pin . -y`

You can also build the code locally with make or jbuider.


# Configuration

## Default environment property
```
export GEMINI_ENV=production
```
or

```
export GEMINI_ENV=sandbox
```


## Sandbox properties
```
export GEMINI_SANDBOX_API_KEY=...
export GEMINI_SANDBOX_API_SECRET=...
```

## Production proprties
```
export GEMINI_PRODUCTION_API_SECRET=...
export GEMINI_PRODUCTION_API_KEY=...
```
# Example command line usage

```
gemini tradevolume

2018-03-30 22:01:44.029835-04:00 Info response:
 ((((account_id 123456) (symbol Ethusd) (base_currency Eth)
   (notional_currency Usd) (data_date 2018-03-07)
   (total_volume_base 1.35528896) (maker_buy_sell_ratio 0) (buy_maker_base 0)
   (buy_maker_notional 0) (buy_maker_count 0) (sell_maker_base 0)
   (sell_maker_notional 0) (sell_maker_count 0) (buy_taker_base 1.35528896)
   (buy_taker_notional 997.5062274496) (buy_taker_count 1)
   (sell_taker_base 0) (sell_taker_notional 0) (sell_taker_count 0))
  ((account_id 123456) (symbol Ethusd) (base_currency Eth)
   (notional_currency Usd) (data_date 2018-03-12)
   (total_volume_base 0.30208043) (maker_buy_sell_ratio 0) (buy_maker_base 0)
   (buy_maker_notional 0) (buy_maker_count 0) (sell_maker_base 0)
   (sell_maker_notional 0) (sell_maker_count 0) (buy_taker_base 0.80208043)
   (buy_taker_notional 428.72431038315204) (buy_taker_count 10)
   (sell_taker_base 0) (sell_taker_notional 0) (sell_taker_count 0))
  ((account_id 123456) (symbol Ethusd) (base_currency Eth)
   (notional_currency Usd) (data_date 2018-03-11) (total_volume_base 0.002)
   (maker_buy_sell_ratio 0) (buy_maker_base 0) (buy_maker_notional 0)
   (buy_maker_count 0) (sell_maker_base 0) (sell_maker_notional 0)
   (sell_maker_count 0) (buy_taker_base 0.002)
   (buy_taker_notional 9.18533876062) (buy_taker_count 3) (sell_taker_base 0)
   (sell_taker_notional 0) (sell_taker_count 0))
  ((account_id 123456) (symbol Ethusd) (base_currency Eth)
   (notional_currency Usd) (data_date 2018-03-17) (total_volume_base 0.889)
   (maker_buy_sell_ratio 1) (buy_maker_base 0.540577)
   (buy_maker_notional 201.69711473518811) (buy_maker_count 1)
   (sell_maker_base 0) (sell_maker_notional 0) (sell_maker_count 0)
   (buy_taker_base 0.248423) (buy_taker_notional 194.40261285)
   (buy_taker_count 1) (sell_taker_base 0) (sell_taker_notional 0)
   (sell_taker_count 0))
  ((account_id 123456) (symbol Ethusd) (base_currency Eth)
   (notional_currency Usd) (data_date 2018-03-27) (total_volume_base 1.02)
   (maker_buy_sell_ratio 1) (buy_maker_base 0.93428)
   (buy_maker_notional 483.1405376) (buy_maker_count 1) (sell_maker_base 0)
   (sell_maker_notional 0) (sell_maker_count 0) (buy_taker_base 0.02572)
   (buy_taker_notional 12.4978624) (buy_taker_count 1) (sell_taker_base 0)
   (sell_taker_notional 0) (sell_taker_count 0))))
 ```

```
 gemini order new '((symbol Ethusd) (price 500.0) (amount .001) (type_ Exchange_limit) (client_order_id "test-order") (options ()) (side Sell))'

2018-03-30 22:18:01.210962-04:00 Info response:
 ((client_order_id (test-order)) (order_id 1392122353) (id 1190026576)
 (symbol Ethusd) (exchange Gemini) (avg_execution_price 0.00) (side Sell)
 (type_ Exchange_limit) (timestamp (2018-03-30 22:18:01.000000-04:00))
 (timestampms (2018-03-30 22:18:01.198000-04:00)) (is_live true)
 (is_cancelled false) (is_hidden false) (was_forced false)
 (executed_amount 0) (remaining_amount 0.001) (options ()) (price 500.00)
 (original_amount 0.001))
```

# License

MIT

# Features and Bugs

Open issues on github. I am too busy to add new features personally
but will review and accept PRs when time permits.

# TODO

- order status web sockets api
- fix unit test harness
- document code

