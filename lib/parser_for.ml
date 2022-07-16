open Base
open Keyword
open Tools
open Syntax
open Parser_tools

let parse_for block_parser tokens =
  let do_for id lex_val tl ~params =
    let value = lex_value_to_value lex_val in

    let ts = [Keyword.For] @ tl in
    let bounds = Bounds.find_bounds ts 0 in
    let chunks = bounds |> Bounds.bounds_to_chunks ts in
    let stop_point = Bounds.stop_point_from_bounds bounds in
    let rest = sub_list_suffix ts stop_point in

    match List.length chunks with
    | 1 ->
      let for_loop = For (List.hd_exn id, value, params, block_parser tl, None) in
      Some (for_loop, rest)
    | 2 ->
      let bchunk = nth chunks 0 in
      let body_chunk = List.tl_exn bchunk in
      let else_chunk = List.tl_exn (nth chunks 1) in
      Debug.print_lex_tokens_with_index else_chunk;
      let for_loop = For (List.hd_exn id, value, params, block_parser body_chunk, Some (block_parser else_chunk)) in
      Some (for_loop, rest)
    | _ -> raise (Failure "A for loop can only have a body and an else statement")
  in

  match tokens with
  | Keyword.For :: LexValue (LexId id) :: In :: LexValue lex_val :: EOS :: tl ->
    do_for id lex_val tl ~params:for_params_default
  | Keyword.For :: LexValue (LexId id) :: In :: LexValue lex_val :: tl -> (
    let (params, rest) = scan_until_eos tl in
    let param_folder found = function
      | LexValue (LexId ["reversed"]) :: ptl ->
        Next ({ limit = found.limit; offset = found.offset; reved = Bool true }, ptl)
      | LexValue (LexId ["offset"]) :: Colon :: LexValue v :: ptl ->
        Next ({ limit = found.limit; offset = lex_value_to_value v; reved = found.reved }, ptl)
      | LexValue (LexId ["limit"]) :: Colon :: LexValue v :: ptl ->
        Next ({ limit = lex_value_to_value v; offset = found.offset; reved = found.reved }, ptl)
      | _ -> Stop (found) in

    let params = unfold for_params_default params param_folder in

    do_for id lex_val rest ~params:params
  )
  | _ -> None
