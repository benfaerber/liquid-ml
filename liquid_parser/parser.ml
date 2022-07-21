open Base
open Liquid_syntax
open Keyword
open Tools
open Syntax
open Parser_tools

type parse_result = (ast * lex_token list) option

let is_parsable_test =
  function
  | If :: _ | Case :: _ | Unless :: _ -> true
  | _ -> false


let parse_test block_parser = function
  | tokens when is_parsable_test tokens ->
    let bounds = Bounds.find_bounds tokens 0 in
    let chunks = bounds |> Bounds.bounds_to_chunks tokens in
    let stop_point = Bounds.stop_point_from_bounds bounds in
    let rest = sub_list_suffix tokens stop_point in
    Some (Test.parse_test_chain block_parser chunks, rest)
  | _ -> None

let parse_capture block_parser = function
  | Keyword.Capture :: LexValue (LexId id) :: EOS :: tl ->
    let (body, rest) = parse_single_body Capture tl in
    let capture = Capture (join id, block_parser body) in
    Some (capture, rest)
  | _ -> None


let parse_cycle _ all_tokens =
  let parse_state name tokens =
    let (state, rest) = scan_until_eos tokens in
    let rec aux acc = function
      | LexValue (LexString s) :: Comma :: tl -> aux (acc @ [s]) tl
      | LexValue (LexString s) :: _ -> acc @ [s]
      | _ -> []
    in
    Some (Cycle (name, aux [] state), rest)
  in

  match all_tokens with
  | Keyword.Cycle :: LexValue (LexString name) :: Colon :: tl -> parse_state (Some name) tl
  | Cycle :: tl -> parse_state None tl
  | _ -> None

let parse_other _ = function
  | LexText t :: tl -> Some (Text t, tl)
  | Newline :: tl -> Some (Text "\n", tl)
  | EOS :: tl -> Some (Nothing, tl)
  | LexBreak :: tl -> Some(Break, tl)
  | LexContinue :: tl -> Some(Continue, tl)
  | _ -> None

let parse_for = For.parse_for
let parse_tablerow = For.parse_tablerow
let parse_paginate = For.parse_paginate
let parse_theme = Theme.parse_theme
let parse_expression = Expression.parse_expression
let parse_assignment = Expression.parse_assignment

let rec first_successful block_parser tokens =
  function
  | parser :: other_parsers -> (
    match parser block_parser tokens with
    | Some (got, rest) -> Some (got, rest)
    | None -> first_successful block_parser tokens other_parsers
  )
  | _ -> None


let rec parse init_tokens =
  let folder block tokens =
    match parse_one tokens with
    | Some (got, rest) ->
      Next (block @ (if got != Nothing then [got] else []), rest)
    | None ->
      Stop (block)
  in

  Block (unfold [] init_tokens folder)
and parse_one tokens =
  let parsers =
    [ parse_assignment
    ; parse_test
    ; parse_capture
    ; parse_for
    ; parse_tablerow
    ; parse_cycle
    ; parse_theme
    ; parse_paginate
    ; parse_expression
    ; parse_other ] in

  first_successful parse tokens parsers
