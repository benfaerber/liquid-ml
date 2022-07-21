let increment = "*increment"
let cycle = "*cycle"

let forloop = "forloop"

let next = "*next"

let skip = "*skip"

type currency =
  | Usd
  | Eur
  | Cad
  | Aud
  | Gbp

let preferred_currency = Usd

let error_policy_key = "*error_policy"

type error_policy =
  | Strict
  | Warn
  | Silent

type log_policy =
  | Verbose
  | Minimal
  | None

let log_policy_key = "*log_policy"

type interpreter_settings = {
  log_policy: log_policy;
  error_policy: error_policy;
}