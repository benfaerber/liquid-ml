open Base
open Syntax
open Tools
open Values

let fi = Float.to_int
let identity _ params = List.hd_exn params

type whitespace_remover = Beginning | End | Both
let remove_whitespace remover text =
  let exp = match remover with
    | Beginning -> ~/"^\\s+"
    | End -> ~/"\\s+$"
    | Both -> ~/"^\\s+|\\s+$" in

  Re2.rewrite_exn exp ~template:"" text

let pick_at_by_op op ctx params =
  match unwrap_all ctx params with
  | Number a :: Number b :: _ -> Number (if op a b then a else b)
  | _ -> raise (Failure "Invalid use")


let apply_op op ctx params =
  match unwrap_all ctx params with
  | Number a :: Number b :: _ ->
    Number (op a b)
  | _ -> raise (Failure "Invalid use")

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
(* TODO: Add support for "default: true, allow_false: true" *)
let default ctx params =
  match unwrap_all ctx params with
  | a :: b :: _ -> (match a with Nil -> b | _ -> a)
  | _ -> raise (Failure "Invalid use")

let divided_by ctx params =
  match unwrap_all ctx params with
  | Number _ :: Number 0. :: _ ->
    raise (Failure "Cannot divide by zero!")
  | Number a :: Number b :: _ ->
    Number (a /. b)
  | _ -> raise (Failure "Invalid use")

let downcase ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (s |> String.lowercase)
  | _ -> raise (Failure "Invalid use")

(* TODO: Escape *)
let escape ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String ("NYI " ^ s)
  | _ -> raise (Failure "Invalid use")

let escape_one ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String ("NYI " ^ s)
  | _ -> raise (Failure "Invalid use")

(* TODO: Add support for dot notation (parser sugar?) *)
let first ctx params =
  match unwrap_all ctx params with
  | List (hd :: _) :: _ -> hd
  | _ -> raise (Failure "Invalid use")

let floor ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> Number (Float.round_down n)
  | String s :: _ -> Number (s |> Float.of_string |> Float.round_down)
  | _ -> raise (Failure "Invalid use")

let join ctx params =
  match unwrap_all ctx params with
  | List lst :: String delim :: _ ->
    let vs = List.map (unwrap_all ctx lst) ~f:(string_from_value ctx) in
    String (String.concat ~sep:delim vs)
  | _ -> raise (Failure "Invalid use")

(* TODO: dot notation *)
let last ctx params =
  match unwrap_all ctx params with
  | List [] :: _ -> Nil
  | List lst :: _ -> lst |> List.rev |> List.hd_exn
  | _ -> raise (Failure "Invalid use")


let lstrip ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (remove_whitespace Beginning s)
  | _ -> raise (Failure "Invalid use")

let rstrip ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (remove_whitespace End s)
  | _ -> raise (Failure "Invalid use")

let strip ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (remove_whitespace Both s)
  | _ -> raise (Failure "Invalid use")


(* TODO: Map *)

let minus = apply_op Float.(-)
let modulo = apply_op Float.(%)

(* TODO: Something wierd going on *)
let newline_to_br ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let nl = ~/"\n" in
    let r = Re2.rewrite_exn nl ~template:"<br />\n" s in
    String r
  )
  | _ -> raise (Failure "Invalid use")

let plus = apply_op Float.(+)

let prepend ctx params =
  match unwrap_all ctx params with
  | String base :: String addition :: _ ->
    String (addition ^ base)
  | _ -> raise (Failure "Invalid use")

let remove ctx params =
  match unwrap_all ctx params with
  | String haystack :: String needle :: _ ->
    let exp = ~/needle in
    let res = Re2.rewrite_exn exp haystack ~template:"" in
    Debug.dump res;
    String res
  | _ -> raise (Failure "Invalid use")

let remove_first ctx params =
  match unwrap_all ctx params with
  | String haystack :: String needle :: _ ->
    let exp = Re2.create_exn needle in
    let first_chunk = Re2.find_first_exn exp haystack in
    let wo_first_chunk = remove_prefix haystack first_chunk in
    let repped_first = Re2.rewrite_exn exp ~template:"" first_chunk in
    String (repped_first ^ wo_first_chunk)
  | _ -> raise (Failure "Invalid use")


let replace ctx params =
  match unwrap_all ctx params with
  | String haystack :: String find_needle :: String replace_needle :: _ ->
    let exp = ~/find_needle in
    let res = Re2.rewrite_exn exp haystack ~template:replace_needle in
    Debug.dump res;
    String res
  | _ -> raise (Failure "Invalid use")

let replace_first ctx params =
  match unwrap_all ctx params with
  | String haystack :: String find_needle :: String replace_needle :: _ ->
    let exp = Re2.create_exn find_needle in
    let first_chunk = Re2.find_first_exn exp haystack in
    let wo_first_chunk = remove_prefix haystack first_chunk in
    let repped_first = Re2.rewrite_exn exp ~template:replace_needle first_chunk in
    String (repped_first ^ wo_first_chunk)
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
  | "divided_by" -> divided_by
  | "downcase" -> downcase
  | "escape" -> escape
  | "escape_one" -> escape_one
  | "first" -> first
  | "floor" -> floor
  | "join" -> join
  | "last" -> last
  | "lstrip" -> lstrip
  | "minus" -> minus
  | "modulo" -> modulo
  | "newline_to_br" -> newline_to_br
  | "plus" -> plus
  | "prepend" -> prepend
  | "remove" -> remove
  | "remove_first" -> remove_first
  | "replace" -> replace
  | "replace_first" -> replace_first

  | "rstrip" -> rstrip
  | "strip" -> strip
  | "slice" -> slice
  | "split" -> split
  | other -> Failure (Core.sprintf "Unknown function %s!" other) |> raise