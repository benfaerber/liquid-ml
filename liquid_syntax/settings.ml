open Syntax

let forloop = "forloop"
let increment = "*increment"
let cycle = "*cycle"
let next = "*next"
let skip = "*skip"
let style_tag = "*style_tag"


type currency =
  | Usd
  | Eur
  | Cad
  | Aud
  | Gbp

type currency_info =
  { symbol : string
  ; abbr : string
  ; name : string
  }

let currency_info_from_currency = function
  | Usd -> { symbol = "$"; abbr = "USD"; name = "US Dollar" }
  | Eur -> { symbol = "€"; abbr = "EUR"; name = "Euro" }
  | Cad -> { symbol = "$"; abbr = "CAD"; name = "Canadian Dollar" }
  | Aud -> { symbol = "$"; abbr = "AUD"; name = "Australian Dollar" }
  | Gbp -> { symbol = "£"; abbr = "GBP"; name = "Pound Sterling" }

type error_handler = string -> unit
type error_policy =
  | Strict
  | Warn
  | Silent
  | Custom of error_handler

type log_policy =
  | Verbose
  | Minimal
  | Never

let log_policy_key = "*log_policy"
let preferred_currency_key = "*preferred_currency"

type t =
  { log_policy : log_policy
  ; error_policy : error_policy
  ; preferred_currency : currency
  ; filters : liquid_filter_lookup
  ; context : variable_context
  }

let default_filter_lookup _ = None
let default_custom_ctx = Ctx.empty

let make
    ?(log_policy = Verbose)
    ?(error_policy = Strict)
    ?(preferred_currency = Usd)
    ?(filters = default_filter_lookup)
    ?(context = default_custom_ctx)
    ()
  =
  { log_policy
  ; error_policy
  ; preferred_currency
  ; filters
  ; context
  }