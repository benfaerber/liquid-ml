open Base
open Liquid_syntax

open Syntax
open Tools
open Values
open Helpers

let compact ctx = function
  | List lst :: _ -> List.filter lst ~f:(Values.is_not_nil ctx) |> ok_list
  | other -> errc "compact accepts a list" other

let concat _ = function
  | List a :: List b :: _ -> a @ b |> ok_list
  | other -> errc "concat accepts 2 lists" other

let first _ = function
  | List (hd :: _) :: _ -> hd |> ok
  | other -> errc "first accepts a list" other


let join ctx params =
  let joiner lst delim =
    let vs = List.map (unwrap_all ctx lst) ~f:(string_from_value ctx) in
    String.concat ~sep:delim vs |> ok_str
  in

  match params with
  | List lst :: String delim :: _ -> joiner lst delim
  | List lst :: _ -> joiner lst " "
  | other -> errc "join accepts a list and a delimiter (string)" other


let last _ = function
  | List [] :: _ -> Nil |> ok
  | List lst :: _ -> lst |> List.rev |> List.hd_exn |> ok
  | other -> errc "last accepts a list" other

let map _ = function
  | List lst :: String key :: _ ->
    let mapped = extract_key_from_object_list lst key in
    mapped |> ok_list
  | other -> errc "map accepts a list and a key (string)" other


let reverse _ = function
  | List lst :: _ -> List.rev lst |> ok_list
  | other -> errc "reverse accepts a list" other


let size _  = function
  | List lst :: _ -> lst |> List.length |> Int.to_float |> ok_num
  | String s :: _ -> s |> String.length |> Int.to_float |> ok_num
  | other -> errc "size accepts a list or a string" other

let sort _ = function
  | List lst :: String key :: _ ->
    let mapped = extract_key_from_object_list lst key in
    let sorted = List.sort mapped ~compare:Values.compare_value in
    sorted |> ok_list
  | List lst :: _ ->
    let sorted = List.sort lst ~compare:Values.compare_value in
    sorted |> ok_list
  | other -> errc "sort accepts a list an optional object key" other

let sort_natural ctx params =
  let comp_key = function
    | String t ->
      let whitespace_exp = ~/"\\s|-|_" in
      Re2.rewrite_exn whitespace_exp ~template:"" t
      |> String.lowercase
    | v -> Values.string_from_value ctx v
  in

  let compare a b = String.compare (comp_key a) (comp_key b) in

  match params with
  | List lst :: String key :: _ ->
    let mapped = extract_key_from_object_list lst key in
    let sorted = List.sort mapped ~compare in
    sorted |> ok_list
  | List lst :: _ ->
    let sorted = List.sort lst ~compare in
    sorted |> ok_list
  | other -> errc "sort_natural accepts a list an optional object key" other


(* TODO: Implement negative indexs *)
let slice _ params =
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

  match params with
  | String s :: Number fstart :: Number fstop :: _ ->
    do_slice_string s fstart fstop |> ok_str
  | String s :: Number findex :: _ ->
    do_slice_string s findex 1. |> ok_str
  | List lst :: Number fstart :: Number fstop :: _ ->
    do_slice_list lst fstart fstop |> ok_list
  | List lst :: Number findex :: _ ->
    do_slice_list lst findex 1. |> ok_list
  | other -> errc "slice accepts a string or list as well as a start index and optional stop index" other


let uniq _ = function
  | List lst :: _ ->
    let folder acc curr = if Tools.contains acc curr then acc else acc @ [curr] in
    let rl = List.fold_left lst ~init:[] ~f:folder in
    rl |> ok_list
  | other -> errc "uniq accepts a list" other

let where ctx params =
  let do_where lst test_key check =
    let filterer = function
      | Object obj -> (
        match Object.find_opt test_key obj with
        | Some value -> check value
        | _ -> false
      )
      | _ -> false
    in

    let filtered_lst = List.filter lst ~f:filterer in
    filtered_lst |> ok_list
  in

  match params with
  | List lst :: String key :: test_value :: _ ->
    do_where lst key (Values.eq ctx test_value)
  | List lst :: String key :: _ ->
    do_where lst key (Values.is_truthy ctx)
  | other -> errc "where accepts a list, a key (string) and an optional test value (any)" other


let function_from_id = function
  | "compact" -> Some compact
  | "concat" -> Some concat
  | "first" -> Some first
  | "join" -> Some join
  | "last" -> Some last
  | "map" -> Some map
  | "reverse" -> Some reverse
  | "size" -> Some size
  | "slice" -> Some slice
  | "sort" -> Some sort
  | "sort_natural" -> Some sort_natural
  | "uniq" -> Some uniq
  | "where" -> Some where
  | _ -> None