open Base
open Keyword
open Tools
open Syntax
open Parser_tools

type parse_result = (ast * lex_token list) option

let parse_expression _ = function
  | LexExpression exp :: tl -> Some (Expression (Parser_expression.parse_expression exp), tl)
  | _ -> None

let parse_assignment _ tokens =
  let add_exp id modifier =
    Func (modifier, [Value (Var id); Value (Number 1.)])
  in

  match tokens with
  | Assign :: LexValue (LexId id) :: Equals :: assign_tl ->
    let (raw_exp, tl) = scan_until_eos assign_tl in
    let exp = Parser_expression.parse_expression raw_exp in
    Some (Assignment (id, exp), tl)
  | Increment :: LexValue (LexId id) :: tl ->
    Some (Assignment (id, add_exp id "plus"), tl)
  | Decrement :: LexValue (LexId id) :: tl ->
    Some (Assignment (id, add_exp id "minus"), tl)
  | _ -> None

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
    Some (Parser_test.parse_test_chain block_parser chunks, rest)
  | _ -> None

let parse_capture block_parser = function
  | Keyword.Capture :: LexValue (LexId id) :: EOS :: tl ->
    let tokens = [Keyword.Capture] @ tl in
    let bounds = Bounds.find_bounds tokens 0 in
    let stop_point = Bounds.stop_point_from_bounds bounds in
    let body = List.sub tl ~pos:0 ~len:(stop_point) in
    let rest = sub_list_suffix tokens stop_point in

    let capture = Capture (id, block_parser body) in
    Some (capture, rest)
  | _ -> None

let parse_other _ = function
  | LexText t :: tl -> Some (Text t, tl)
  | Newline :: tl -> Some (Text "\n", tl)
  | EOS :: tl -> Some (Nothing, tl)
  | _ -> None

let parse_block_with_parser parser init_tokens =
  let folder block tokens =
    match parser tokens with
    | Some (got, rest) ->
      Next (block @ (if got != Nothing then [got] else []), rest)
    | None ->
      Stop (block)
  in

  Block (unfold [] init_tokens folder)

let rec parse_one tokens =
  let block_parser = parse_block_with_parser parse_one in
  let parsers = [parse_assignment; parse_expression; parse_test; parse_capture; parse_other] in
  let found_parser =
    List.find parsers ~f:(
      fun parser -> match parser block_parser tokens with Some(_) -> true | None -> false
    ) in

  match found_parser with
  | Some(parser) -> (
      match parser block_parser tokens with
      | Some (got, rest) -> Some (got, rest)
      | None -> None
    )
  | None -> None

let parse_block tokens = parse_block_with_parser parse_one tokens

let log_tokens = true

let test_liquid_block liq =
  Debug.print_line ();
  Stdio.print_endline liq;
  Debug.print_line ();
  let tokens = liq |> Preprocessor.preprocess |> Lexer.lex_text in
  (* Stdio.print_endline "Tokens:";
     Debug.print_lex_tokens_with_index tokens; *)
  Debug.print_line ();
  let res = parse_block tokens in
  Stdio.print_endline "Parse Result:";
  Debug.print_ast res;
;;

let test_liquid_file filename =
  filename |> File.read |> test_liquid_block

let test () =
  test_liquid_block "{% assign animal = \"horse\" | capitilize %}";
  test_liquid_block "hello there";
  test_liquid_file "liquid/if_else_test.liquid";