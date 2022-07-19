open Base
open Syntax
open Tools
open Values

let fi = Float.to_int
let identity _ params = List.hd_exn params

let err t = Failure ("Liquid Error: " ^ t)
let errc t c = Failure (
  Core.sprintf "Liquid Error: %s, Params: %s" t
  (List.map c ~f:Debug.value_as_string |> join_by_comma)
)

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
  | other -> raise (errc "at_most/at_least accepts 2 numbers" other)


(* NOTE: For incr, decr to work, it must tolerate nil value. Maybe create incr_op func? *)
let apply_op op ctx params =
  match unwrap_all ctx params with
  | Nil :: Number b :: _ -> Number (op 0. b)
  | Number a :: Number b :: _ ->
    Number (op a b)
  | other -> raise (errc "operator accepts 2 numbers" other)

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
  | other -> raise (errc "abs accepts a number" other)


let append ctx params =
  match unwrap_all ctx params with
  | String base :: String addition :: _ ->
    String (base ^ addition)
  | other -> raise (errc "append accepts 2 strings" other)

let at_least = pick_at_by_op Float.(<)
let at_most = pick_at_by_op Float.(>)

let capitalize ctx params =
  match unwrap_all ctx params with
  | String s :: _ ->
    let capped = s |> first_letter |> String.capitalize in
    let tl = String.sub s ~pos:1 ~len:(String.length s - 1) in
    String (capped ^ tl)
  | other -> raise (errc "capitalize accepts a string" other)

let ceil ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> Number (Float.round_up n)
  | String s :: _ -> Number (s |> Float.of_string |> Float.round_up)
  | other -> raise (errc "ceil takes a number" other)

let compact ctx params =
  match unwrap_all ctx params with
  | List lst :: _ -> List (List.filter lst ~f:(Values.is_not_nil ctx))
  | other -> raise (errc "compact accepts a list" other)

let concat ctx params =
  match unwrap_all ctx params with
  | List a :: List b :: _ -> List (a @ b)
  | other -> raise (errc "concat accepts 2 lists" other)

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

(* TODO: Add support for "default: true, allow_false: true" *)
let default ctx params =
  match unwrap_all ctx params with
  | a :: b :: _ -> (match a with Nil -> b | _ -> a)
  | other -> raise (errc "Default accepts 2 values" other)

let divided_by ctx params =
  match unwrap_all ctx params with
  | Number _ :: Number 0. :: _ ->
    raise (err "Cannot divide by zero!")
  | Number a :: Number b :: _ ->
    Number (a /. b)
  | other -> raise (errc "divided_by accepts 2 numbers" other)

let downcase ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (s |> String.lowercase)
  | other -> raise (errc "downcase accepts a string" other)

(* TODO: Escape *)
let escape ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String ("NYI " ^ s)
  | _ -> raise (Failure "Invalid use")

let escape_one ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String ("NYI " ^ s)
  | _ -> raise (Failure "Invalid use")

let first ctx params =
  match unwrap_all ctx params with
  | List (hd :: _) :: _ -> hd
  | other -> raise (errc "first accepts a list" other)

let floor ctx params =
  match unwrap_all ctx params with
  | Number n :: _ -> Number (Float.round_down n)
  | String s :: _ -> Number (s |> Float.of_string |> Float.round_down)
  | other -> raise (errc "floor accepts a number or a string containing a number" other)

let join ctx params =
  match unwrap_all ctx params with
  | List lst :: String delim :: _ ->
    let vs = List.map (unwrap_all ctx lst) ~f:(string_from_value ctx) in
    String (String.concat ~sep:delim vs)
  | other -> raise (errc "join accepts a list and a delimiter (string)" other)

let last ctx params =
  match unwrap_all ctx params with
  | List [] :: _ -> Nil
  | List lst :: _ -> lst |> List.rev |> List.hd_exn
  | other -> raise (errc "last accepts a list" other)


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

let map ctx params =
  match unwrap_all ctx params with
  | List lst :: String key :: _ ->
    let get_key =
      function
      | Object obj -> (match Obj.find_opt key obj with Some x -> x | _ -> Nil)
      | _ -> raise (err "map can only be used on a list of objects")
    in

    let mapped_lst = List.map lst ~f:get_key in
    List mapped_lst
  | other -> raise (errc "map accepts a list and a key (string)" other)



let minus = apply_op Float.(-)
let modulo = apply_op Float.(%)

let newline_to_br ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let nl = ~/"\n" in
    let r = Re2.rewrite_exn nl ~template:"<br />\n" s in
    String r
  )
  | other -> raise (errc "newline_to_br accepts a string" other)

let plus = apply_op Float.(+)

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

let reverse ctx params =
  match unwrap_all ctx params with
  | List lst :: _ -> List (List.rev lst)
  | other -> raise (errc "reverse accepts a list" other)

let round ctx params =
  match unwrap_all ctx params with
  | Number a :: Number fplaces :: _ -> (
    let places = Float.to_int fplaces in
    Number (Float.round_decimal a ~decimal_digits:places)
  )
  | Number a :: _ -> Number (Float.round_nearest a)
  | _ -> raise (Failure "Invalid use")

let size ctx params =
  match unwrap_all ctx params with
  | List lst :: _ -> Number (lst |> List.length |> Int.to_float)
  | _ -> raise (Failure "Invalid use")

(* TODO: SOrt *)

let strip_html ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let exp = ~/"<.+?/?>" in
    let r = Re2.rewrite_exn exp ~template:"" s in
    String (r)
  )
  | _ -> raise (Failure "Invalid use")


let strip_newlines ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> (
    let exp = ~/"\n" in
    let r = Re2.rewrite_exn exp ~template:"" s in
    String (r)
  )
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

let times = apply_op Float.( * )

let truncate ctx params =
  let trunc finisher s chars =
    if String.length s > chars then
      String ((String.sub s ~pos:0 ~len:chars) ^ finisher)
    else
      String (s)
  in

  match unwrap_all ctx params with
  | String s :: Number fchars :: String finisher :: _ -> trunc finisher s (Float.to_int fchars)
  | String s :: Number fchars :: _ -> trunc "..." s (Float.to_int fchars)
  | _ -> raise (Failure "Invalid use")

let truncatewords ctx params =
  let trunc finisher s count =
    let words = String.split s ~on:' ' in
    if List.length words > count then
      let picked_words = List.sub words ~pos:0 ~len:count |> join_by_space in
      String (picked_words ^ finisher)
    else
      String (s)
  in

  match unwrap_all ctx params with
  | String s :: Number fcount :: String finisher :: _ -> trunc finisher s (Float.to_int fcount)
  | String s :: Number fcount :: _ -> trunc "..." s (Float.to_int fcount)
  | _ -> raise (Failure "Invalid use")

let split ctx params =
  match unwrap_all ctx params with
  | String s :: String delim :: _ ->
    let literal = Str.split (Str.regexp delim) s |> List.map ~f:(fun x -> String x) in
    List literal
  | _ -> raise (Failure "Invalid use")

let uniq ctx params =
  match unwrap_all ctx params with
  | List lst :: _ ->
    let folder acc curr = if Tools.contains acc curr then acc else acc @ [curr] in
    let rl = List.fold_left lst ~init:[] ~f:folder in
    List (rl)
  | _ -> raise (Failure "Invalid use")

let upcase ctx params =
  match unwrap_all ctx params with
  | String s :: _ -> String (s |> String.capitalize)
  | _ -> raise (Failure "Invalid use")


type enc_type = Encode | Decode
let encode_decode_url enc_type url =
  let reps =
    [ ("%", "%25")
    ; (":", "%3A")
    ; ("/", "%2F")
    ; ("?", "%3F")
    ; ("#", "%23")
    ; ("[", "%5B")
    ; ("]", "%5D")
    ; ("@", "%40")
    ; ("!", "%21")
    ; ("$", "%24")
    ; ("&", "%26")
    ; ("\'", "%27")
    ; ("(", "%28")
    ; (")", "%29")
    ; ("*", "%2A")
    ; ("+", "%2B")
    ; (",", "%2C")
    ; (";", "%3B")
    ; ("=", "%3D")
    ; (" ", "+")
    ]
  in

  let folder acc (d, e) =
    let p, w = match enc_type with Encode -> (d, e) | Decode -> (e, d) in
    String.substr_replace_all acc ~pattern:p ~with_:w
  in
  List.fold reps ~init:url ~f:folder
let url_encode ctx params =
  match unwrap_all ctx params with
  | String url :: _ -> String (encode_decode_url Encode url)
  | _ -> raise (Failure "Invalid use")

let url_decode ctx params =
  match unwrap_all ctx params with
  | String url :: _ -> String (encode_decode_url Decode url)
  | _ -> raise (Failure "Invalid use")


let where ctx params =
  let do_where lst test_key check =
    let fr = function
      | Object obj -> (
        match Obj.find_opt test_key obj with
        | Some value -> check value
        | _ -> false
      )
      | _ -> false
    in

    let filtered_lst = List.filter lst ~f:fr in
    List filtered_lst
  in

  match unwrap_all ctx params with
  | List lst :: String key :: test_value :: _ ->
    do_where lst key (Values.eq ctx test_value)
  | List lst :: String key :: _ ->
    do_where lst key (Values.is_truthy ctx)
  | _ -> raise (Failure "Invalid Use")

let function_from_id = function
  | "abs" -> abs
  | "append" -> append
  | "at_least" -> at_least
  | "at_most" -> at_most
  | "compact" -> compact
  | "capitalize" -> capitalize
  | "ceil" -> ceil
  | "concat" -> concat
  | "date" -> date
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
  | "map" -> map
  | "minus" -> minus
  | "modulo" -> modulo
  | "newline_to_br" -> newline_to_br
  | "plus" -> plus
  | "prepend" -> prepend
  | "remove" -> remove
  | "remove_first" -> remove_first
  | "replace" -> replace
  | "replace_first" -> replace_first
  | "reverse" -> reverse
  | "round" -> round
  | "rstrip" -> rstrip
  | "size" -> size
  | "strip" -> strip
  | "strip_html" -> strip_html
  | "strip_newlines" -> strip_newlines
  | "slice" -> slice
  | "split" -> split
  | "truncate" -> truncate
  | "truncatewords" -> truncatewords
  | "uniq" -> uniq
  | "upcase" -> upcase
  | "url_decode" -> url_decode
  | "url_encode" -> url_encode
  | "where" -> where
  | other -> Failure (Core.sprintf "Unknown function %s!" other) |> raise