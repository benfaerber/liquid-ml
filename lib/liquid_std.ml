open Base
open Syntax
open Tools
open Values

let fi = Float.to_int
let identity _ params = List.hd_exn params

let template ctx params =
  match unwrap_all ctx params with
  | Number _ :: _ -> String "NYI"
  | _ -> raise (Failure "Invalid use")

let abs ctx params =
  let do_abs n = Number (if Float.(<) n 0. then n *. -1. else n) in
  match unwrap_all ctx params with
  | Number n :: _ -> do_abs n
  | String s :: _ -> (
    let n = Float.of_string s in
    do_abs n
  )
  | _ -> raise (Failure "Invalid abs use")


let append ctx params =
  match unwrap_all ctx params with
  | String base :: String addition :: _ ->
    String (base ^ addition)
  | _ -> raise (Failure "Invalid use")

let pick_at_by_op op ctx params =
  match unwrap_all ctx params with
  | Number a :: Number b :: _ -> Number (if op a b then a else b)
  | _ -> raise (Failure "Invalid use")

let at_least = pick_at_by_op Float.(<)
let at_most = pick_at_by_op Float.(>)

let capitalize ctx params =
  match unwrap_all ctx params with
  | String s :: _ ->
    let capped = s |> first_letter |> String.capitalize in
    let tl = String.sub s ~pos:1 ~len:(String.length s - 1) in
    String (capped ^ tl)
  | _ -> raise (Failure "Invalid capitalize use")

let ceil ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> Number (Float.round_up n)
  | String s :: _ -> Number (s |> Float.of_string |> Float.round_up)
  | _ -> raise (Failure "Invalid use")

(* TODO: Compact *)

let concat ctx params =
  match unwrap_all ctx params with
  | List a :: List b :: _ -> List (a @ b)
  | _ -> raise (Failure "Invalid use")

(* TODO: Date *)

let default ctx params =
  match unwrap_all ctx params with
  | a :: b :: _ -> (match a with Nil -> b | _ -> a)
  | _ -> raise (Failure "Invalid use")

let slice ctx params =
  match unwrap_all ctx params with
  | String s :: Number fstart :: Number fstop :: _ ->
    let start = fi fstart and stop = fi fstop in
    let ns = String.sub s ~pos:start ~len:(start + stop) in
    String ns
  | String s :: Number findex :: _ ->
    let index = fi findex in
    let ns = String.sub s ~pos:index ~len:1 in
    String ns
  | _ -> raise (Failure "Invalid use")

let remove ctx params =
  match unwrap_all ctx params with
  | String haystack :: String needle :: _ ->
    let exp = ~/needle in
    let res = Re2.rewrite_exn exp haystack ~template:"" in
    Debug.dump res;
    String res
  | _ -> raise (Failure "Invalid use")

let split ctx params =
  match unwrap_all ctx params with
  | String s :: String delim :: _ ->
    let literal = Str.split (Str.regexp delim) s |> List.map ~f:(fun x -> String x) in
    List literal
  | _ -> raise (Failure "Invalid use")

let function_from_id = function
  | "abs" -> abs
  | "append" -> append
  | "at_least" -> at_least
  | "at_most" -> at_most
  | "capitalize" -> capitalize
  | "ceil" -> ceil
  | "concat" -> concat
  | "default" -> default
  | "slice" -> slice
  | "split" -> split
  | "remove" -> remove
  | other -> Failure (Core.sprintf "Unknown function %s!" other) |> raise