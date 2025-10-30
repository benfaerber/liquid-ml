open Base
open Liquid_syntax
open Syntax
open Values
open Helpers

let date ctx params =
  let do_date fmat  = function
    | "now" ->  String (Date.now_as_string fmat)
    | other -> String (Date.format_date_string other fmat)
  in

  match unwrap_all ctx params with
  | String date_str :: String fmat :: _ -> do_date fmat date_str |> ok
  | String date_str :: _ -> do_date "%m/%d/%Y" date_str |> ok
  | Date date :: String fmat :: _ -> String (Date.as_string date fmat) |> ok
  | [Date date] -> String (Date.as_string date "%m/%d/%Y") |> ok
  | other -> errc "date accepts a string or a date and an optional format string" other

let default ctx params =
  match unwrap_all ctx params with
  | a :: b :: Bool allow_false :: _ ->
    let comp = if allow_false then Values.is_not_nil else Values.is_truthy in
    if comp ctx a then Ok a else Ok b
  | a :: b :: _ -> if Values.is_truthy ctx a then Ok a else Ok b
  | other -> errc "default accepts 2 values and an optional named argument allow_false" other

(* TODO: Look into liquid int/float distinction *)


let json ctx params =
  match unwrap_all ctx params with
  | v :: _ -> String (Values.json_from_value ctx v) |> ok
  | other -> errc "json conversion failed!" other

let function_from_id = function
  | "date" -> Some date
  | "default" -> Some default
  | "json" -> Some json
  | _ -> None
