open Base
open Keyword
open Tools
open Syntax

let lex_to_equation a op b =
  Equation (lex_value_to_value a, op, lex_value_to_value b)

let combine_condition p1 p2 = function
  | LexAnd -> Combine (And, [p1; p2])
  | LexOr -> Combine (Or, [p1; p2])

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

let scan_until_eos tokens =
  let rec aux acc = function
    | hd :: _ when hd = EOS -> acc
    | hd :: tl -> aux (acc @ [hd]) tl
    | [] -> acc
  in
  let res = aux [] tokens in
  (res, remove_list_prefix tokens res)

let to_exp_value v = Value (lex_value_to_value v)
let to_exp_values lst = List.map lst ~f:to_exp_value

let prev = Value Previous
let parse_expression full_tokens =
  let rec aux tokens =
    let (prefix, tail) =
      match tokens with
      | LexValue v :: tl -> (to_exp_value v), tl
      | _ -> prev, tokens in

    let make_func id params tl =
      [Func (id, [prefix] @ to_exp_values params)] @ aux tl in

    match tail with
    | [Keyword.Expression ex] -> aux ex
    | [LexValue v] ->
      [Value (lex_value_to_value v)]
    | Pipe :: LexValue (LexId id) :: Colon :: LexValue p1 :: Comma :: LexValue p2 :: Comma :: LexValue p3 :: tl ->
      make_func id [p1; p2; p3] tl
    | Pipe :: LexValue (LexId id) :: Colon :: LexValue p1 :: Comma :: LexValue p2 :: tl ->
      make_func id [p1; p2] tl
    | Pipe :: LexValue (LexId id) :: Colon :: LexValue p1 :: tl ->
      make_func id [p1] tl
    | Pipe :: LexValue (LexId id) :: tl ->
      make_func id [] tl
    | _ -> []
    in aux full_tokens


let parse_test_statement chunk =
  let (cond_tokens, body_tokens) = chunk |> scan_until_eos in
  let condition = build_condition cond_tokens in
  (condition, InProgress body_tokens)

let parse_when_statement case_id =
  function
  | When :: LexValue value :: tl ->
    let condition = Equation (Var case_id, Eq, lex_value_to_value value) in
    (condition, InProgress tl)
  | Else :: tl ->
    (AlwaysTrue, InProgress tl)
  | _ -> raise (Failure "This is not a when statement")

let parse_when_statements case_id chunks =
  let when_statements = List.map chunks ~f:(parse_when_statement case_id) in
  let rec unfold_into_test = function
    | (cond, body) :: tl -> Some (Test (cond, body, unfold_into_test tl))
    | [] -> None
  in

  unfold_into_test when_statements

let build_test_chain chunks =
  let rec aux pool =
    match pool with
    | fs :: tl -> (
      match fs with
      | Case :: LexValue LexId(case_id) :: EOS :: _ ->
        parse_when_statements case_id tl
      | _ ->
        let (condition, body) = parse_test_statement fs in
        Some (Test (condition, body, aux tl))
    )
    | [] -> None
  in

  match aux chunks with
  | Some c -> c
  | None -> raise (Failure "Failed to build if chain")


let lex_file fname =
  fname
  |> File.read
  |> Preprocessor.preprocess
  |> Lexer.lex_text

let log_tokens = true

let test_assign () =
  Stdio.print_endline "Attempting to parse block_test:";
  let tokens = lex_file "liquid/block_test.liquid" in
  Stdio.print_endline "---";
  if log_tokens then
    tokens |> Debug.lex_tokens_as_string_with_index |> Stdio.print_endline;

  let test_expressions = [
    "\"horse\"";
    "\"horse\" | capitalize";
    "\"a, b, c, d\" | split: \", \" ";
    "var | slice: 1, 2 | upcase | join: \"---\"";
    "beatles | upcase"
  ] in

  let exps = List.map test_expressions ~f:(fun e -> e, (e |> Lexer.lex_line_tokens |> parse_expression)) in
  List.iter exps ~f:(fun (exp, tks) ->
    Stdio.print_endline exp;
    List.iter tks ~f:Debug.print_expression;
    Stdio.print_endline "";
  );
;;

let test_conditions () =
  let tokens = lex_file "liquid/if_else_test.liquid" in

  if log_tokens then
    tokens |> Debug.lex_tokens_as_string_with_index |> Stdio.print_endline;
  Debug.print_line ();

  let bounds = Bounds.find_bounds tokens 2 in

  (* bounds
  |> bounds_to_chunks tokens
  |> List.map (fun t -> Debug.lex_tokens_as_string t)
  |> List.iter (Stdio.printf "ENTRY:\n%s------------------------\n\n");
  Debug.print_line (); *)
  bounds
  |> Bounds.bounds_to_chunks tokens
  |> build_test_chain
  |> Debug.print_ast;

  (* "x == 99 and t > 3 or pet == \"dot\""
  |> Lexer.lex_line_tokens
  |> build_condition
  |> ignore; *)
;;

type test_options = TestAssign | TestConditions
let to_test = TestAssign
let test () =
  match to_test with
  | TestAssign -> test_assign ()
  | TestConditions -> test_conditions ()
  ;

  Stdio.print_endline "" |> ignore