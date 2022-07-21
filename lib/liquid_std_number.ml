open Base
open Syntax
open Values
open Liquid_std_helpers

let abs ctx params =
  let do_abs n = Number (if Float.(<) n 0. then n *. -1. else n) in
  match unwrap_all ctx params with
  | Number n :: _ -> do_abs n
  | String s :: _ -> (
    let n = Float.of_string s in
    do_abs n
  )
  | other -> raise (errc "abs accepts a number" other)

let at_least = pick_at_by_op Float.(<)
let at_most = pick_at_by_op Float.(>)

let ceil ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> Number (Float.round_up n)
  | String s :: _ -> Number (s |> Float.of_string |> Float.round_up)
  | other -> raise (errc "ceil takes a number" other)

let divided_by ctx params =
  match unwrap_all ctx params with
  | Number _ :: Number 0. :: _ ->
    raise (err "Cannot divide by zero!")
  | Number a :: Number b :: _ ->
    Number (a /. b)
  | other -> raise (errc "divided_by accepts 2 numbers" other)


let floor ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> Number (Float.round_down n)
  | String s :: _ -> Number (s |> Float.of_string |> Float.round_down)
  | other -> raise (errc "floor accepts a number or a string containing a number" other)


let minus = apply_op Float.(-)
let modulo = apply_op Float.(%)
let plus = apply_op Float.(+)
let times = apply_op Float.( * )

let round ctx params =
  match unwrap_all ctx params with
  | Number a :: Number fplaces :: _ -> (
    let places = Float.to_int fplaces in
    Number (Float.round_decimal a ~decimal_digits:places)
  )
  | Number a :: _ -> Number (Float.round_nearest a)
  | other -> raise (errc "round accepts a number and an option number of decimal places" other)

let weight_with_unit ctx params =
  match unwrap_all ctx params with
  | Number n :: String u :: _ ->
    let weight_unit = parse_weight_unit u |> weight_unit_as_string in
    let literal = Values.string_from_value ctx (Number n) ^ " " ^ weight_unit in
    String literal
  | other -> raise (errc "weight_with_unit accepts a number and an optional unit name" other)

let money ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> String (format_money_symbol Global.preferred_currency n)
  | other -> raise (errc "money accepts a number" other)

let money_with_currency ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> String (format_money_currency Global.preferred_currency n)
  | other -> raise (errc "money_with_currency accepts a number" other)

let money_without_currency ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> String (format_money_number n)
  | other -> raise (errc "money_without_currency accepts a number" other)

let money_without_trailing_zeros ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> String (format_money_symbol_no_zeros Global.preferred_currency n)
  | other -> raise (errc "money_without_trailing_zeros accepts a number" other)


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