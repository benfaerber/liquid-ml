open Base
open Liquid_syntax
open Syntax
open Tools
open Values

let fi = Float.to_int
let identity _ params = List.hd_exn params

let err t = Error ("Liquid Error: " ^ t)
let errc t c = Error (
  Core.sprintf "Liquid Error: %s, Params: %s" t
  (List.map c ~f:Debug.value_as_string |> join_by_comma)
)

let ok v = Ok v
let ok_num v = Ok (Number v)
let ok_str v = Ok (String v)
let ok_list v = Ok (List v)

type whitespace_remover = Beginning | End | Both
let remove_whitespace remover text =
  let exp = match remover with
    | Beginning -> ~/"^\\s+"
    | End -> ~/"\\s+$"
    | Both -> ~/"^\\s+|\\s+$" in

  Re2.rewrite_exn exp ~template:"" text

let pick_at_by_op op ctx params =
  match unwrap_all ctx params with
  | Number a :: Number b :: _ -> Number (if op a b then a else b) |> ok
  | other -> errc "at_most/at_least accepts 2 numbers" other


(* NOTE: For incr, decr to work, it must tolerate nil value. Maybe create incr_op func? *)
let apply_op op ctx params =
  match unwrap_all ctx params with
  | Nil :: Number b :: _ -> Number (op 0. b) |> ok
  | Number a :: Number b :: _ ->
    Number (op a b) |> ok
  | other -> errc "operator accepts 2 numbers" other


let extract_key_from_object_list lst key =
  let lookup_key = function
    | Object obj -> (match Obj.find_opt key obj with Some x -> x | _ -> Nil)
    | _ -> Nil
  in

  List.map lst ~f:lookup_key

type weight_units = Grams | Kilograms | Pounds | Ounces
let weight_unit_as_string = function
  | Grams -> "g"
  | Kilograms -> "kg"
  | Pounds -> "lbs"
  | Ounces -> "ozs"

let parse_weight_unit = function
  | "g" | "gs" | "grams" | "gram" -> Grams
  | "kg" | "kgs" | "kilograms" | "kilogram" -> Kilograms
  | "lb" | "lbs" | "pounds" | "pound" -> Pounds
  | "oz" | "ozs" | "ounces" | "ounce" -> Ounces
  | _ -> Pounds

let capitalize_first_letter s =
  let capped = s |> first_letter |> String.capitalize in
  let tl = String.sub s ~pos:1 ~len:(String.length s - 1) in
  capped ^ tl

(* TODO: Add more currency *)

let format_thousands_int n =
  let i = n |> Int.to_string |> String.rev in
  let r = List.init (String.length i) ~f:(String.get i) in

  let ts = Char.to_string in
  let rec aux = function
    | a :: b :: c :: tl -> [ts c ^ ts b ^ ts a] @ aux tl
    | a :: b :: tl -> [ts b ^ ts a] @ aux tl
    | a :: tl -> [ts a] @ aux tl
    | _ -> []
  in

  aux r
  |> List.rev
  |> String.concat ~sep:","

let format_thousands_float n =
  let dec =
    match nth (n |> Float.to_string |> String.split ~on:'.') 1 with
    | "" -> "0"
    | other -> other
  in
  format_thousands_int (n |> Float.to_int)  ^ "." ^ dec

let format_money_number n =
  let pieces =
    Float.round_decimal n ~decimal_digits:2
    |> Float.to_string
    |> String.split ~on:'.'
  in

  let fs = n |> Float.to_int |> format_thousands_int in
  let sd = nth pieces 1 in

  let clean_sd = match String.length sd with 0 -> "00" | 1 -> sd ^ "0" | _ -> sd in
  fs ^ "." ^ clean_sd

let format_money_symbol (currency_info: Settings.currency_info) num =
  currency_info.symbol ^ format_money_number num

let format_money_currency (currency_info: Settings.currency_info) num =
  currency_info.symbol ^ format_money_number num ^ " " ^ currency_info.abbr

let format_money_symbol_no_zeros (currency_info: Settings.currency_info) num =
  currency_info.symbol ^ (Float.to_int num |> format_thousands_int)

let value_from_currency = function
  | Settings.Usd -> String "USD"
  | Eur -> String "EUR"
  | Cad -> String "CAD"
  | Aud -> String "AUD"
  | Gbp -> String "GBP"

let currency_from_value = function
  | String "USD" -> Settings.Usd
  | String "EUR" -> Eur
  | String "CAD" -> Cad
  | String "AUD" -> Aud
  | String "GBP" -> Gbp
  | _ -> Usd

let preferred_currency ctx =
  Values.unwrap ctx (Var [Settings.preferred_currency_key])
  |> currency_from_value

let preferred_currency_info ctx =
  preferred_currency ctx |> Settings.currency_info_from_currency
