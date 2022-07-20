open Base
open Syntax
open Tools
open Values
open Liquid_std_helpers

let compact ctx params =
  match unwrap_all ctx params with
  | List lst :: _ -> List (List.filter lst ~f:(Values.is_not_nil ctx))
  | other -> raise (errc "compact accepts a list" other)

let concat ctx params =
  match unwrap_all ctx params with
  | List a :: List b :: _ -> List (a @ b)
  | other -> raise (errc "concat accepts 2 lists" other)

let first ctx params =
  match unwrap_all ctx params with
  | List (hd :: _) :: _ -> hd
  | other -> raise (errc "first accepts a list" other)


let join ctx params =
  let joiner lst delim =
    let vs = List.map (unwrap_all ctx lst) ~f:(string_from_value ctx) in
    String (String.concat ~sep:delim vs)
  in

  match unwrap_all ctx params with
  | List lst :: String delim :: _ -> joiner lst delim
  | List lst :: _ -> joiner lst " "
  | other -> raise (errc "join accepts a list and a delimiter (string)" other)


let last ctx params =
  match unwrap_all ctx params with
  | List [] :: _ -> Nil
  | List lst :: _ -> lst |> List.rev |> List.hd_exn
  | other -> raise (errc "last accepts a list" other)

let map ctx params =
  match unwrap_all ctx params with
  | List lst :: String key :: _ ->
    let mapped = extract_key_from_object_list lst key in
    List mapped
  | other -> raise (errc "map accepts a list and a key (string)" other)


let reverse ctx params =
  match unwrap_all ctx params with
  | List lst :: _ -> List (List.rev lst)
  | other -> raise (errc "reverse accepts a list" other)


let size ctx params =
  match unwrap_all ctx params with
  | List lst :: _ -> Number (lst |> List.length |> Int.to_float)
  | String s :: _ -> Number (s |> String.length |> Int.to_float)
  | other -> raise (errc "size accepts a list or a string" other)

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


let uniq ctx params =
  match unwrap_all ctx params with
  | List lst :: _ ->
    let folder acc curr = if Tools.contains acc curr then acc else acc @ [curr] in
    let rl = List.fold_left lst ~init:[] ~f:folder in
    List (rl)
  | other -> raise (errc "uniq accepts a list" other)

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