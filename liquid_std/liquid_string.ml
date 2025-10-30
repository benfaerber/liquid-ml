open Base
open Liquid_syntax
open Syntax
open Tools
open Helpers
open Encoder

let append _ = function
  | String base :: String addition :: _ -> String (base ^ addition) |> ok
  | other -> errc "append accepts 2 strings" other

let base64_decode _ = function
  | String s :: _ -> Base64.decode_exn s |> ok_str
  | other -> errc "base64_decode accepts a string" other

let base64_encode _ = function
  | String s :: _ -> Base64.encode_exn s |> ok_str
  | other -> errc "base64_encode accepts a string" other

let base64_url_safe_decode _ = function
  | String s :: _ -> Base64.decode_exn s |> decode_url |> ok_str
  | other -> errc "base64_decode accepts a string" other

let base64_url_safe_encode _ = function
  | String s :: _ -> Base64.encode_exn s |> encode_url |> ok_str
  | other -> errc "base64_encode accepts a string" other

(* The function is called camelcase but in the docs returns pascal case ??? Wierd *)
(* TODO: does camelcase make a work that is uppercase lower? Ie my-URL -> myUrl *)
let camelcase _ = function
  | String s :: _ ->
      let wsexp = ~/"\\s|-|_" in
      let words =
        Re2.rewrite_exn wsexp ~template:" " s |> String.split ~on:' '
      in
      let cammed = List.map words ~f:String.capitalize |> join in
      cammed |> ok_str
  | other -> errc "camelcase accepts a string" other

let capitalize _ = function
  | String s :: _ -> String.capitalize s |> ok_str
  | other -> errc "capitalize accepts a string" other

let downcase _ = function
  | String s :: _ -> s |> String.lowercase |> ok_str
  | other -> errc "downcase accepts a string" other

let escape _ = function
  | String s :: _ -> encode_text s |> ok_str
  | other -> errc "escape accepts a string" other

let escape_once _ = function
  | String s :: _ ->
      (if has_encoded_text s |> not then encode_text s else s) |> ok_str
  | other -> errc "escape_once accepts a string" other

(* handlelize aka kebab-case *)
let handleize _ = function
  | String s :: _ ->
      let trimmed = remove_whitespace Both s in
      let special_chars = ~/"[^a-zA-z0-9]+" in
      let words =
        Re2.rewrite_exn special_chars ~template:" " trimmed
        |> String.split ~on:' '
      in
      let cleaned = List.filter words ~f:(fun t -> t != "") in
      let kebab =
        List.map cleaned ~f:String.lowercase |> String.concat ~sep:"-"
      in
      kebab |> ok_str
  | other -> errc "handleize accepts a string" other

let hmac_sha1 _ = function
  | String message :: String hash :: _ ->
      let enc = message ^ hash |> Sha1.string |> Sha1.to_hex in
      enc |> ok_str
  | other -> errc "hmac_sha1 accepts a string and a hash (string)" other

let hmac_sha256 _ = function
  | String message :: String hash :: _ ->
      let enc = message ^ hash |> Sha256.string |> Sha256.to_hex in
      enc |> ok_str
  | other -> errc "hmac_sha256 accepts a string and a hash (string)" other

let lstrip _ = function
  | String s :: _ -> remove_whitespace Beginning s |> ok_str
  | other -> errc "lstrip accepts a string" other

let rstrip _ = function
  | String s :: _ -> remove_whitespace End s |> ok_str
  | other -> errc "rstrip accepts a string" other

let strip _ = function
  | String s :: _ -> remove_whitespace Both s |> ok_str
  | other -> errc "strip accepts a string" other

let md5 _ = function
  | String s :: _ ->
      let enc = s |> Md5_lib.string |> Md5_lib.to_hex in
      enc |> ok_str
  | other -> errc "md5 accepts a string" other

let newline_to_br _ = function
  | String s :: _ ->
      let nl = ~/"\n" in
      let r = Re2.rewrite_exn nl ~template:"<br />\n" s in
      r |> ok_str
  | other -> errc "newline_to_br accepts a string" other

let pluralize _ = function
  | Number n :: String singular :: String plural :: _ ->
      let l = if n = 1. then singular else plural in
      l |> ok_str
  | other ->
      errc "pluralize accepts a number, a singular string and a plural string"
        other

let prepend _ = function
  | String base :: String addition :: _ -> addition ^ base |> ok_str
  | other -> errc "prepend accepts 2 strings" other

let remove _ = function
  | String haystack :: String needle :: _ ->
      let exp = ~/needle in
      let res = Re2.rewrite_exn exp haystack ~template:"" in
      res |> ok_str
  | other -> errc "remove accepts 2 strings" other

let remove_first _ = function
  | String haystack :: String needle :: _ ->
      let s = String.substr_replace_first haystack ~pattern:needle ~with_:"" in
      s |> ok_str
  | other -> errc "remove_fist accepts 2 strings" other

let remove_last _ = function
  | String haystack :: String needle :: _ ->
      let rhaystack, rneedle = (String.rev haystack, String.rev needle) in
      let s =
        String.substr_replace_first rhaystack ~pattern:rneedle ~with_:""
      in
      String.rev s |> ok_str
  | other -> errc "remove_fist accepts 2 strings" other

let replace _ = function
  | String haystack :: String find_needle :: String replace_needle :: _ ->
      let exp = ~/find_needle in
      let res = Re2.rewrite_exn exp haystack ~template:replace_needle in
      res |> ok_str
  | other -> errc "replace accepts 3 strings" other

let replace_first _ = function
  | String haystack :: String find_needle :: String replace_needle :: _ ->
      let s =
        String.substr_replace_first haystack ~pattern:find_needle
          ~with_:replace_needle
      in
      s |> ok_str
  | other -> errc "replace_first accepts 3 strings" other

let replace_last _ = function
  | String haystack :: String find_needle :: String replace_needle :: _ ->
      let r_hay, r_find, r_rep =
        (String.rev haystack, String.rev find_needle, String.rev replace_needle)
      in
      let s = String.substr_replace_first r_hay ~pattern:r_find ~with_:r_rep in
      String.rev s |> ok_str
  | other -> errc "replace_last accepts 3 strings" other

let sha1 _ = function
  | String message :: _ ->
      let enc = message |> Sha1.string |> Sha1.to_hex in
      enc |> ok_str
  | other -> errc "sha1 accepts a string" other

let sha256 _ = function
  | String message :: _ ->
      let enc = message |> Sha256.string |> Sha256.to_hex in
      enc |> ok_str
  | other -> errc "sha256 accepts a string" other

let strip_html _ = function
  | String s :: _ ->
      let exp = ~/"<.+?/?>" in
      let r = Re2.rewrite_exn exp ~template:"" s in
      r |> ok_str
  | other -> errc "string_html accepts a string" other

let strip_newlines _ = function
  | String s :: _ ->
      let exp = ~/"\n" in
      let r = Re2.rewrite_exn exp ~template:"" s in
      r |> ok_str
  | other -> errc "strip_newlines accepts a string" other

let truncate _ params =
  let do_truncate finisher s chars =
    let finisher_len = String.length finisher in
    (if String.length s > chars then
       let truncate_len = max 0 (chars - finisher_len) in
       String.sub s ~pos:0 ~len:truncate_len ^ finisher
     else s)
    |> ok_str
  in

  match params with
  | String s :: Number fchars :: String finisher :: _ ->
      do_truncate finisher s (Float.to_int fchars)
  | String s :: Number fchars :: _ -> do_truncate "..." s (Float.to_int fchars)
  | other ->
      errc
        "truncate accepts a string, a number and an optional finisher value \
         (string)"
        other

let truncatewords _ params =
  let do_truncwords finisher s count =
    let words = String.split s ~on:' ' in
    (if List.length words > count then
       let picked_words = List.sub words ~pos:0 ~len:count |> join_by_space in
       picked_words ^ finisher
     else s)
    |> ok_str
  in

  match params with
  | String s :: Number fcount :: String finisher :: _ ->
      do_truncwords finisher s (Float.to_int fcount)
  | String s :: Number fcount :: _ ->
      do_truncwords "..." s (Float.to_int fcount)
  | other ->
      errc
        "truncatewords accepts a string, a number and an optional finisher \
         value (string)"
        other

let split _ = function
  | String s :: String delim :: _ ->
      let literal =
        Str.split (Str.regexp delim) s |> List.map ~f:(fun x -> String x)
      in
      literal |> ok_list
  | other -> errc "split accepts a string and a delimiter (string)" other

let upcase _ = function
  | String s :: _ -> s |> String.uppercase |> ok_str
  | other -> errc "upcase accepts a string" other

let url_encode _ = function
  | String url :: _ -> encode_url url |> ok_str
  | other -> errc "url_encode accepts a string" other

let url_decode _ = function
  | String url :: _ -> decode_url url |> ok_str
  | other -> errc "url_decode accepts a string" other

let url_escape _ = function
  | String url :: _ -> escape_url url |> ok_str
  | other -> errc "url_escape accepts a string" other

let url_param_escape _ = function
  | String url :: _ -> escape_param_url url |> ok_str
  | other -> errc "url_param_escape accepts a string" other

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
  | "replace_last" -> Some replace_last
  | "rstrip" -> Some rstrip
  | "strip" -> Some strip
  | "strip_html" -> Some strip_html
  | "strip_newlines" -> Some strip_newlines
  | "split" -> Some split
  | "sha1" -> Some sha1
  | "sha256" -> Some sha256
  | "md5" -> Some md5
  | "truncate" -> Some truncate
  | "truncatewords" -> Some truncatewords
  | "upcase" -> Some upcase
  | "url_decode" -> Some url_decode
  | "url_encode" -> Some url_encode
  | "url_escape" -> Some url_escape
  | "url_param_escape" -> Some url_param_escape
  | _ -> None
