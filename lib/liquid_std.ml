open Base
open Syntax
open Tools
open Values
open Liquid_std_helpers
open Liquid_std_encode

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

let default ctx params =
  match unwrap_all ctx params with
  | a :: b :: Bool allow_false :: _ -> (
    let comp = if allow_false then Values.is_not_nil else Values.is_truthy in
    if comp ctx a then a else b
  )
  | a :: b :: _ -> if Values.is_truthy ctx a then a else b
  | other -> raise (errc "default accepts 2 values and an optional named argument allow_false" other)

(* TODO: Look into liquid int/float distinction *)
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
  let joiner lst delim =
    let vs = List.map (unwrap_all ctx lst) ~f:(string_from_value ctx) in
    String (String.concat ~sep:delim vs)
  in

  match unwrap_all ctx params with
  | List lst :: String delim :: _ -> joiner lst delim
  | List lst :: _ -> joiner lst " "
  | other -> raise (errc "join accepts a list and a delimiter (string)" other)

let json ctx params =
  match unwrap_all ctx params with
  | v :: _ -> String (Values.json_from_value ctx v)
  | other -> raise (errc "json conversion failed!" other)

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
    let mapped = extract_key_from_object_list lst key in
    List mapped
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
  | other -> raise (errc "round accepts a number and an option number of decimal places" other)

let size ctx params =
  match unwrap_all ctx params with
  | List lst :: _ -> Number (lst |> List.length |> Int.to_float)
  | String s :: _ -> Number (s |> String.length |> Int.to_float)
  | other -> raise (errc "size accepts a list or a string" other)

(* TODO: SOrt *)

let sort ctx params =
  match unwrap_all ctx params with
  | List lst :: String key :: _ ->
    let mapped = extract_key_from_object_list lst key in
    let sorted = List.sort mapped ~compare:Values.compare_value in
    List sorted
  | List lst :: _ ->
    let sorted = List.sort lst ~compare:Values.compare_value in
    List sorted
  | other -> raise (errc "sort accepts a list an optional object key" other)

let sort_natural ctx params =
  let comp_key = function
    | String t ->
      let whitespace_exp = ~/"\\s|-|_" in
      Re2.rewrite_exn whitespace_exp ~template:"" t
      |> String.lowercase
    | v -> Values.string_from_value ctx v
  in

  let compare a b = String.compare (comp_key a) (comp_key b) in

  match unwrap_all ctx params with
  | List lst :: String key :: _ ->
    let mapped = extract_key_from_object_list lst key in
    let sorted = List.sort mapped ~compare in
    List sorted
  | List lst :: _ ->
    let sorted = List.sort lst ~compare in
    List sorted
  | other -> raise (errc "sort_natural accepts a list an optional object key" other)


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

(* TODO: Implement negative indexs *)
let slice ctx params =
  let do_slice slicer lengther lst fstart fstop =
    let start, stop = fi fstart, fi fstop in
    if start >= 0 then
      slicer lst ~pos:start ~len:(start + stop)
    else
      let cstart = lengther lst + start in
      slicer lst ~pos:cstart ~len:stop
  in

  let do_slice_string = do_slice String.sub String.length in
  let do_slice_list = do_slice List.sub List.length in

  match unwrap_all ctx params with
  | String s :: Number fstart :: Number fstop :: _ ->
    String (do_slice_string s fstart fstop)
  | String s :: Number findex :: _ ->
    String (do_slice_string s findex 1.)
  | List lst :: Number fstart :: Number fstop :: _ ->
    List (do_slice_list lst fstart fstop)
  | List lst :: Number findex :: _ ->
    List (do_slice_list lst findex 1.)
  | other -> raise (errc "slice accepts a string or list as well as a start index and optional stop index" other)

let times = apply_op Float.( * )

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

let uniq ctx params =
  match unwrap_all ctx params with
  | List lst :: _ ->
    let folder acc curr = if Tools.contains acc curr then acc else acc @ [curr] in
    let rl = List.fold_left lst ~init:[] ~f:folder in
    List (rl)
  | other -> raise (errc "uniq accepts a list" other)

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


let where ctx params =
  let do_where lst test_key check =
    let filterer = function
      | Object obj -> (
        match Obj.find_opt test_key obj with
        | Some value -> check value
        | _ -> false
      )
      | _ -> false
    in

    let filtered_lst = List.filter lst ~f:filterer in
    List filtered_lst
  in

  match unwrap_all ctx params with
  | List lst :: String key :: test_value :: _ ->
    do_where lst key (Values.eq ctx test_value)
  | List lst :: String key :: _ ->
    do_where lst key (Values.is_truthy ctx)
  | other -> raise (errc "where accepts a list, a key (string) and an optional test value (any)" other)

let weight_with_unit ctx params =
  match unwrap_all ctx params with
  | Number n :: String u :: _ ->
    let weight_unit = parse_weight_unit u |> weight_unit_as_string in
    let literal = Values.string_from_value ctx (Number n) ^ " " ^ weight_unit in
    String literal
  | other -> raise (errc "weight_with_unit accepts a number and an optional unit name" other)

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
  | "escape_once" -> escape_once
  | "first" -> first
  | "floor" -> floor
  | "join" -> join
  | "json" -> json
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
  | "sort" -> sort
  | "sort_natural" -> sort_natural
  | "times" -> times
  | "truncate" -> truncate
  | "truncatewords" -> truncatewords
  | "uniq" -> uniq
  | "upcase" -> upcase
  | "url_decode" -> url_decode
  | "url_encode" -> url_encode
  | "where" -> where
  | "weight_with_unit" -> weight_with_unit
  | other -> Failure (Core.sprintf "Unknown function %s!" other) |> raise