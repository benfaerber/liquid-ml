open Base
open Keyword
open Tools
open Syntax
open Parser_tools

let lex_to_equation a op b =
  Equation (lex_value_to_value a, op, lex_value_to_value b)

let combine_condition p1 p2 = function
  | LexAnd -> Combine (And, p1, p2)
  | LexOr -> Combine (Or, p1, p2)

let build_condition tokens =
  let is_unless = List.hd_exn tokens = Unless in
  match tokens with
  | Else :: _ -> AlwaysTrue
  | If :: statement
  | ElseIf :: statement
  | Unless :: statement -> (
    let rec aux acc pool =
      match pool with
      | [LexValue a1; Operator op1; LexValue b1] ->
        lex_to_equation a1 op1 b1
      | LexValue a1 :: Operator op1 :: LexValue b1 :: LexCombiner c :: LexValue a2 :: Operator op2 :: LexValue b2 :: tl ->
        let part_1 = lex_to_equation a1 op1 b1 in
        let part_2 = lex_to_equation a2 op2 b2 in
        let compound = combine_condition part_1 part_2 c in
        aux compound tl
      | LexCombiner c :: LexValue a1 :: Operator op1 :: LexValue b1 :: tl ->
        let part_1 = lex_to_equation a1 op1 b1 in
        let compound = combine_condition acc part_1 c in
        aux compound tl
      | [] -> acc
      | _ -> raise (Failure ("Invalid condition"))
    in
    let res = aux AlwaysTrue statement in
    if is_unless then Not res else res
  )
  | _ -> raise (Failure "Invalid token list")


let parse_test_statement block_parser chunk =
  let (cond_tokens, body_tokens) = chunk |> scan_until_eos in
  let condition = build_condition cond_tokens in
  (condition, block_parser body_tokens)

let parse_when_statement block_parser case_id =
  function
  | When :: LexValue value :: tl ->
    let condition = Equation (Var case_id, Eq, lex_value_to_value value) in
    (condition, block_parser tl)
  | Else :: tl ->
    (AlwaysTrue, block_parser tl)
  | _ -> raise (Failure "This is not a when statement")

let parse_when_statements block_parser case_id chunks =
  let when_statements = List.map chunks ~f:(parse_when_statement block_parser case_id) in
  let rec unfold_into_test = function
    | (cond, body) :: tl -> Some (Test (cond, body, unfold_into_test tl))
    | [] -> None
  in

  unfold_into_test when_statements

let parse_test_chain block_parser chunks =
  let rec aux pool =
    match pool with
    | fs :: tl -> (
      match fs with
      | Case :: LexValue LexId(case_id) :: EOS :: _ ->
        parse_when_statements block_parser case_id tl
      | _ ->
        let (condition, body) = parse_test_statement block_parser fs in
        Some (Test (condition, body, aux tl))
    )
    | [] -> None
  in

  match aux chunks with
  | Some c -> c
  | None -> raise (Failure "Failed to build if chain")
