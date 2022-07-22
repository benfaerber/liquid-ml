open Syntax
open Settings

let value_from_currency = function
  | Usd -> String "USD"
  | Eur -> String "EUR"
  | Cad -> String "CAD"
  | Aud -> String "AUD"
  | Gbp -> String "GBP"

let currency_from_value = function
  | String "USD" -> Usd
  | String "EUR" -> Eur
  | String "CAD" -> Cad
  | String "AUD" -> Aud
  | String "GBP" -> Gbp
  | _ -> Usd

let preferred_currency ctx =
  Values.unwrap ctx (Var [preferred_currency_key])
  |> currency_from_value

let preferred_currency_info ctx =
  preferred_currency ctx |> currency_info_from_currency

let add settings ctx =
  ctx
  |> Ctx.add preferred_currency_key (value_from_currency settings.preferred_currency)