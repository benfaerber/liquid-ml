open Base
open Syntax
open Values
open Liquid_std_helpers

let template ctx params =
  match unwrap_all ctx params with
  | Number _ :: _ -> String "NYI"
  | _ -> raise (Failure "Invalid use")

(* TODO: Date, Compare Ocaml date handling with Ruby date handling  *)

let date ctx params =
  let do_date date_str fmat =
  match date_str with
  | "now" ->  String (Date.now_as_string fmat)
    | _ -> String (Date.format_date_string date_str fmat)
  in

  match unwrap_all ctx params with
  | String date_str :: String fmat :: _ -> do_date date_str fmat
  | String date_str :: _ -> do_date date_str "%m/%d/%Y"
  | Date date :: String fmat :: _ -> String (Date.date_as_string date fmat)
  | [Date date] -> String (Date.date_as_string date "%m/%d/%Y")
  | other -> raise (errc "date accepts a string or a date and an optional format string" other)

let default ctx params =
  match unwrap_all ctx params with
  | a :: b :: Bool allow_false :: _ -> (
    let comp = if allow_false then Values.is_not_nil else Values.is_truthy in
    if comp ctx a then a else b
  )
  | a :: b :: _ -> if Values.is_truthy ctx a then a else b
  | other -> raise (errc "default accepts 2 values and an optional named argument allow_false" other)

(* TODO: Look into liquid int/float distinction *)


let json ctx params =
  match unwrap_all ctx params with
  | v :: _ -> String (Values.json_from_value ctx v)
  | other -> raise (errc "json conversion failed!" other)


let base_function_from_id = function
  | "date" -> Some date
  | "default" -> Some default
  | "json" -> Some json
  | _ -> None

let rec first_successful name =
  function
  | getter :: other_getters -> (
    match getter name with
    | Some f -> Some f
    | _ -> first_successful name other_getters
  )
  | _ -> None


let function_from_id name =
  let sources =
    [ Liquid_std_number.function_from_id
    ; Liquid_std_string.function_from_id
    ; Liquid_std_list.function_from_id
    ; base_function_from_id ]
  in

  match first_successful name sources with
  | Some func -> func
  | _ -> raise (Failure (Core.sprintf "Unknown function %s!" name))
