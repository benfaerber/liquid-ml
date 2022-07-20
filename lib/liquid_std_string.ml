open Base
open Syntax
open Tools
open Values
open Liquid_std_helpers
open Liquid_std_encode

let append ctx params =
  match unwrap_all ctx params with
  | String base :: String addition :: _ ->
    String (base ^ addition)
  | other -> raise (errc "append accepts 2 strings" other)

let capitalize ctx params =
  match unwrap_all ctx params with
  | String s :: _ ->
    let capped = s |> first_letter |> String.capitalize in
    let tl = String.sub s ~pos:1 ~len:(String.length s - 1) in
    String (capped ^ tl)
  | other -> raise (errc "capitalize accepts a string" other)


let downcase ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (s |> String.lowercase)
  | other -> raise (errc "downcase accepts a string" other)

let escape ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (encode_text s)
  | other -> raise (errc "escape accepts a string" other)

let escape_once ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    if has_encoded_text s |> not then
      String (encode_text s)
    else
      String s
  )
  | other -> raise (errc "escape_once accepts a string" other)

let lstrip ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (remove_whitespace Beginning s)
  | other -> raise (errc "lstrip accepts a string" other)

let rstrip ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (remove_whitespace End s)
  | other -> raise (errc "rstrip accepts a string" other)

let strip ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (remove_whitespace Both s)
  | other -> raise (errc "strip accepts a string" other)


let newline_to_br ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let nl = ~/"\n" in
    let r = Re2.rewrite_exn nl ~template:"<br />\n" s in
    String r
  )
  | other -> raise (errc "newline_to_br accepts a string" other)


let prepend ctx params =
  match unwrap_all ctx params with
  | String base :: String addition :: _ ->
    String (addition ^ base)
  | other -> raise (errc "prepend accepts 2 strings" other)

let remove ctx params =
  match unwrap_all ctx params with
  | String haystack :: String needle :: _ ->
    let exp = ~/needle in
    let res = Re2.rewrite_exn exp haystack ~template:"" in
    String res
  | other -> raise (errc "remove accepts 2 strings" other)

let remove_first ctx params =
  match unwrap_all ctx params with
  | String haystack :: String needle :: _ ->
    let exp = Re2.create_exn needle in
    let first_chunk = Re2.find_first_exn exp haystack in
    let wo_first_chunk = remove_prefix haystack first_chunk in
    let repped_first = Re2.rewrite_exn exp ~template:"" first_chunk in
    String (repped_first ^ wo_first_chunk)
  | other -> raise (errc "remove_fist accepts 2 strings" other)


let replace ctx params =
  match unwrap_all ctx params with
  | String haystack :: String find_needle :: String replace_needle :: _ ->
    let exp = ~/find_needle in
    let res = Re2.rewrite_exn exp haystack ~template:replace_needle in
    String res
  | other -> raise (errc "replace accepts 3 strings" other)

let replace_first ctx params =
  match unwrap_all ctx params with
  | String haystack :: String find_needle :: String replace_needle :: _ ->
    let exp = Re2.create_exn find_needle in
    let first_chunk = Re2.find_first_exn exp haystack in
    let wo_first_chunk = remove_prefix haystack first_chunk in
    let repped_first = Re2.rewrite_exn exp ~template:replace_needle first_chunk in
    String (repped_first ^ wo_first_chunk)
  | other -> raise (errc "replace_first accepts 3 strings" other)

let strip_html ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let exp = ~/"<.+?/?>" in
    let r = Re2.rewrite_exn exp ~template:"" s in
    String (r)
  )
  | other -> raise (errc "string_html accepts a string" other)

let strip_newlines ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let exp = ~/"\n" in
    let r = Re2.rewrite_exn exp ~template:"" s in
    String (r)
  )
  | other -> raise (errc "strip_newlines accepts a string" other)


let truncate ctx params =
  let do_truncate finisher s chars =
    if String.length s > chars then
      String ((String.sub s ~pos:0 ~len:chars) ^ finisher)
    else
      String (s)
  in

  match unwrap_all ctx params with
  | String s :: Number fchars :: String finisher :: _ -> do_truncate finisher s (Float.to_int fchars)
  | String s :: Number fchars :: _ -> do_truncate "..." s (Float.to_int fchars)
  | other -> raise (errc "truncate accepts a string, a number and an optional finisher value (string)" other)

let truncatewords ctx params =
  let do_truncwords finisher s count =
    let words = String.split s ~on:' ' in
    if List.length words > count then
      let picked_words = List.sub words ~pos:0 ~len:count |> join_by_space in
      String (picked_words ^ finisher)
    else
      String (s)
  in

  match unwrap_all ctx params with
  | String s :: Number fcount :: String finisher :: _ -> do_truncwords finisher s (Float.to_int fcount)
  | String s :: Number fcount :: _ -> do_truncwords "..." s (Float.to_int fcount)
  | other -> raise (errc "truncatewords accepts a string, a number and an optional finisher value (string)" other)

let split ctx params =
  match unwrap_all ctx params with
  | String s :: String delim :: _ ->
    let literal = Str.split (Str.regexp delim) s |> List.map ~f:(fun x -> String x) in
    List literal
  | other -> raise (errc "split accepts a string and a delimiter (string)" other)


let upcase ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (s |> String.capitalize)
  | other -> raise (errc "upcase accepts a string" other)


let url_encode ctx params =
  match unwrap_all ctx params with
  | String url :: _ -> String (encode_url url)
  | other -> raise (errc "url_encode accepts a string" other)

let url_decode ctx params =
  match unwrap_all ctx params with
  | String url :: _ -> String (decode_url url)
  | other -> raise (errc "url_decode accepts a string" other)




let function_from_id = function
  | "append" -> Some append
  | "capitalize" -> Some capitalize
  | "downcase" -> Some downcase
  | "escape" -> Some escape
  | "escape_once" -> Some escape_once
  | "lstrip" -> Some lstrip
  | "newline_to_br" -> Some newline_to_br
  | "prepend" -> Some prepend
  | "remove" -> Some remove
  | "remove_first" -> Some remove_first
  | "replace" -> Some replace
  | "replace_first" -> Some replace_first
  | "rstrip" -> Some rstrip
  | "strip" -> Some strip
  | "strip_html" -> Some strip_html
  | "strip_newlines" -> Some strip_newlines
  | "split" -> Some split
  | "truncate" -> Some truncate
  | "truncatewords" -> Some truncatewords
  | "upcase" -> Some upcase
  | "url_decode" -> Some url_decode
  | "url_encode" -> Some url_encode
  | _ -> None