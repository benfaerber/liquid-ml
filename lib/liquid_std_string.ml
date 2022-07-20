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

let base64_decode ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (Base64.decode_exn s)
  | other -> raise (errc "base64_decode accepts a string" other)

let base64_encode ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (Base64.encode_exn s)
  | other -> raise (errc "base64_encode accepts a string" other)

let base64_url_safe_decode ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (Base64.decode_exn s |> decode_url)
  | other -> raise (errc "base64_decode accepts a string" other)

let base64_url_safe_encode ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (Base64.encode_exn s |> encode_url)
  | other -> raise (errc "base64_encode accepts a string" other)

(* The function is called camelcase but in the docs returns pascal case ??? Wierd *)
(* TODO: does camelcase make a work that is uppercase lower? Ie my-URL -> myUrl *)
let camelcase ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let wsexp = ~/"\\s|-|_" in
    let words = Re2.rewrite_exn wsexp ~template:" " s |> String.split ~on:' ' in
    let cammed = List.map words ~f:capitalize_first_letter |> join in
    String cammed
  )
  | other -> raise (errc "camelcase accepts a string" other)

let capitalize ctx params =
  match unwrap_all ctx params with
  | String s :: _ ->
    String (capitalize_first_letter s)
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

(* handlelize aka kebab-case *)
let handleize ctx params =
  match unwrap_all ctx params with
  | String s :: _ ->
    let trimmed = remove_whitespace Both s in
    let special_chars = ~/"[^a-zA-z0-9]+" in
    let words = Re2.rewrite_exn special_chars ~template:" " trimmed |> String.split ~on:' ' in
    let cleaned = List.filter words ~f:(fun t -> t != "") in
    let kebab = List.map cleaned ~f:String.lowercase |> String.concat ~sep:"-" in
    String kebab
  | other -> raise (errc "handleize accepts a string" other)

let hmac_sha1 ctx params =
  match unwrap_all ctx params with
  | String message :: String hash :: _ ->
    let enc = message ^ hash |> Sha1.string |> Sha1.to_hex in
    String enc
  | other -> raise (errc "hmac_sha1 accepts a string and a hash (string)" other)

let hmac_sha256 ctx params =
  match unwrap_all ctx params with
  | String message :: String hash :: _ ->
    let enc = message ^ hash |> Sha1.string |> Sha1.to_hex in
    String enc
  | other -> raise (errc "hmac_sha256 accepts a string and a hash (string)" other)

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


let md5 ctx params =
  match unwrap_all ctx params with
  | String s :: _ ->
    let enc = s |> Md5_lib.string |> Md5_lib.to_hex in
    String enc
  | other -> raise (errc "md5 accepts a string" other)


let newline_to_br ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let nl = ~/"\n" in
    let r = Re2.rewrite_exn nl ~template:"<br />\n" s in
    String r
  )
  | other -> raise (errc "newline_to_br accepts a string" other)


let pluralize ctx params =
  match unwrap_all ctx params with
  | Number n :: String singular :: String plural :: _ ->
    let l = if n = 1. then singular else plural in
    String l
  | other -> raise (errc "pluralize accepts a number, a singular string and a plural string" other)


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
    let s = String.substr_replace_first haystack ~pattern:needle ~with_:"" in
    String s
  | other -> raise (errc "remove_fist accepts 2 strings" other)


let remove_last ctx params =
  match unwrap_all ctx params with
  | String haystack :: String needle :: _ ->
    let rhaystack, rneedle = String.rev haystack, String.rev needle in
    let s = String.substr_replace_first rhaystack ~pattern:rneedle ~with_:"" in
    String (String.rev s)
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
  | "base64_decode" -> Some base64_decode
  | "base64_encode" -> Some base64_encode
  | "base64_url_safe_decode" -> Some base64_url_safe_decode
  | "base64_url_safe_encode" -> Some base64_url_safe_encode
  | "camelcase" -> Some camelcase
  | "capitalize" -> Some capitalize
  | "downcase" -> Some downcase
  | "escape" -> Some escape
  | "escape_once" -> Some escape_once
  | "handleize" | "handle" -> Some handleize
  | "hmac_sha1" -> Some hmac_sha1
  | "hmac_sha256" -> Some hmac_sha256
  | "lstrip" -> Some lstrip
  | "newline_to_br" -> Some newline_to_br
  | "prepend" -> Some prepend
  | "pluralize" -> Some pluralize
  | "remove" -> Some remove
  | "remove_first" -> Some remove_first
  | "remove_last" -> Some remove_last
  | "replace" -> Some replace
  | "replace_first" -> Some replace_first
  | "rstrip" -> Some rstrip
  | "strip" -> Some strip
  | "strip_html" -> Some strip_html
  | "strip_newlines" -> Some strip_newlines
  | "split" -> Some split
  | "md5" -> Some md5
  | "truncate" -> Some truncate
  | "truncatewords" -> Some truncatewords
  | "upcase" -> Some upcase
  | "url_decode" -> Some url_decode
  | "url_encode" -> Some url_encode
  | _ -> None