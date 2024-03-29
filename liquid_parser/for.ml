open Base
open Liquid_syntax
open Tools
open Syntax
open Parser_tools

type for_type = ForLoop | TableRow

let parse_forlike for_type block_parser tokens =
  let kw, is_tablerow = if for_type = ForLoop then Tokens.LexFor, false else TableRow, true in
  let do_for lid lex_val tl ~params =
    let id = join lid in
    let value = lex_value_to_value lex_val in

    let ts = [kw] @ tl in
    let bounds = Bounds.find_bounds ts 0 in
    let chunks = bounds |> Bounds.bounds_to_chunks ts in
    let stop_point = Bounds.stop_point_from_bounds bounds in
    let rest = sub_list_suffix ts stop_point in

    match List.length chunks with
    | 1 ->
      let for_loop = For (id, value, params, block_parser tl, None) in
      Some (for_loop, rest)
    | 2 ->
      let bchunk = nth chunks 0 in
      let body_chunk = List.tl_exn bchunk in
      let else_chunk = List.tl_exn (nth chunks 1) in
      Debug.print_lex_tokens_with_index else_chunk;
      let for_loop = For (id, value, params, block_parser body_chunk, Some (block_parser else_chunk)) in
      Some (for_loop, rest)
    | _ -> raise (Failure "A for loop can only have a body and an else statement")
  in

  match tokens with
  | st :: LexValue (LexId id) :: In :: LexValue lex_val :: EOS :: tl when st = kw ->
    do_for id lex_val tl ~params:for_params_default
  | st :: LexValue (LexId id) :: In :: LexValue lex_val :: tl when st = kw -> (
    let (params, rest) = scan_until_eos tl in
    let param_folder found = function
      | LexValue (LexId ["reversed"]) :: ptl ->
        Next ({ limit = found.limit; offset = found.offset; reved = true; cols = found.cols; is_tablerow = is_tablerow }, ptl)
      | LexValue (LexId ["offset"]) :: Colon :: LexValue (LexNumber n) :: ptl ->
        Next ({ limit = found.limit; offset = Float.to_int n; reved = found.reved; cols = found.cols; is_tablerow = is_tablerow }, ptl)
      | LexValue (LexId ["limit"]) :: Colon :: LexValue (LexNumber n) :: ptl ->
        Next ({ limit = Float.to_int n; offset = found.offset; reved = found.reved; cols = found.cols; is_tablerow = is_tablerow }, ptl)
      | LexValue (LexId ["cols"]) :: Colon :: LexValue (LexNumber n) :: ptl ->
        Next ({ limit = found.limit; offset = found.offset; reved = found.reved; cols = Float.to_int n; is_tablerow = is_tablerow }, ptl)
      | _ -> Stop (found) in

    let params = unfold for_params_default params param_folder in

    do_for id lex_val rest ~params:params
  )
  | _ -> None

let parse_for = parse_forlike ForLoop
let parse_tablerow = parse_forlike TableRow

let parse_paginate block_parser = function
  | Tokens.Paginate :: LexValue (LexId id) :: By :: LexValue (LexNumber num) :: tl -> (
    let (body, rest) = parse_single_body Tokens.Paginate tl in
    let page = Paginate (id, Float.to_int num, block_parser body) in
    Some (page, rest)
  )
  | _ -> None
