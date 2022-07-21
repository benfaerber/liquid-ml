open Base
open Syntax
open Values
open Liquid_std_helpers

let abs ctx params =
  let do_abs n = (if Float.(<) n 0. then n *. -1. else n) |> ok_num in
  match unwrap_all ctx params with
  | Number n :: _ -> do_abs n
  | String s :: _ -> s |> Float.of_string |> do_abs
  | other -> errc "abs accepts a number" other

let at_least = pick_at_by_op Float.(<)
let at_most = pick_at_by_op Float.(>)

let ceil ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> Float.round_up n |> ok_num
  | String s :: _ -> s |> Float.of_string |> Float.round_up |> ok_num
  | other -> errc "ceil takes a number" other

let divided_by ctx params =
  match unwrap_all ctx params with
  | Number _ :: Number 0. :: _ ->
    err "Cannot divide by zero!"
  | Number a :: Number b :: _ ->
    a /. b |> ok_num
  | other -> errc "divided_by accepts 2 numbers" other


let floor ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> Float.round_down n |> ok_num
  | String s :: _ -> s |> Float.of_string |> Float.round_down |> ok_num
  | other -> errc "floor accepts a number or a string containing a number" other


let minus = apply_op Float.(-)
let modulo = apply_op Float.(%)
let plus = apply_op Float.(+)
let times = apply_op Float.( * )

let round ctx params =
  match unwrap_all ctx params with
  | Number a :: Number fplaces :: _ -> (
    let places = Float.to_int fplaces in
    Float.round_decimal a ~decimal_digits:places |> ok_num
  )
  | Number a :: _ -> Float.round_nearest a |> ok_num
  | other -> errc "round accepts a number and an option number of decimal places" other

let weight_with_unit ctx params =
  match unwrap_all ctx params with
  | Number n :: String u :: _ ->
    let weight_unit = parse_weight_unit u |> weight_unit_as_string in
    let literal = Values.string_from_value ctx (Number n) ^ " " ^ weight_unit in
    literal |> ok_str
  | other -> errc "weight_with_unit accepts a number and an optional unit name" other

let money ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> format_money_symbol (Settings_ctx.preferred_currency_info ctx) n |> ok_str
  | other -> errc "money accepts a number" other

let money_with_currency ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> format_money_currency (Settings_ctx.preferred_currency_info ctx) n |> ok_str
  | other -> errc "money_with_currency accepts a number" other

let money_without_currency ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> format_money_number n |> ok_str
  | other -> errc "money_without_currency accepts a number" other

let money_without_trailing_zeros ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> format_money_symbol_no_zeros (Settings_ctx.preferred_currency_info ctx) n |> ok_str
  | other -> errc "money_without_trailing_zeros accepts a number" other


let function_from_id = function
  | "abs" -> Some abs
  | "at_least" -> Some at_least
  | "at_most" -> Some at_most
  | "ceil" -> Some ceil
  | "divided_by" -> Some divided_by
  | "floor" -> Some floor
  | "minus" -> Some minus
  | "modulo" -> Some modulo
  | "plus" -> Some plus
  | "round" -> Some round
  | "times" -> Some times
  | "weight_with_unit" -> Some weight_with_unit
  | "money" -> Some money
  | "money_with_currency" -> Some money_with_currency
  | "money_without_currency" -> Some money_without_currency
  | "money_without_trailing_zeros" -> Some money_without_trailing_zeros
  | _ -> None