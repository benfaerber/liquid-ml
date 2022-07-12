open Base
open Keyword
open Tools
open Syntax

type parse_result = (ast * lex_token list) option

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
    | hd :: tl when hd = EOS -> acc, tl
    | hd :: tl -> aux (acc @ [hd]) tl
    | [] -> acc, []
  in aux [] tokens

let to_exp_value v = Value (lex_value_to_value v)
let to_exp_values lst = List.map lst ~f:to_exp_value

let parse_lex_expression full_tokens =
  let rec aux tokens =
    let (prefix, tail) =
      match tokens with
      | LexValue v :: tl -> (to_exp_value v), tl
      | _ -> Value Skip, tokens in

    let make_func id params tl =
      [Func (id, [prefix] @ to_exp_values params)] @ aux tl in

    match tail with
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
    in

  let func_list = aux full_tokens in

  let remove_skip = function
    | Value Skip -> []
    | e -> [e] in

  let rec unfold_into_func = function
    | Func (id, (Value Skip :: other_params)) :: tl ->
      Func (id, (tl |> unfold_into_func |> remove_skip)  @ other_params)
    | Func (id, params) :: tl ->
      Func (id, (tl |> unfold_into_func |> remove_skip) @ params)
    | _ -> Value Skip
  in

  unfold_into_func func_list

let parse_expression = function
  | LexExpression exp :: tl -> Some (Expression (parse_lex_expression exp), tl)
  | _ -> None

let parse_assignment tokens =
  let add_exp id modifier =
    Func (modifier, [Value (Var id); Value (Number 1.)])
  in

  match tokens with
  | Assign :: LexValue (LexId id) :: Equals :: assign_tl ->
    let (raw_exp, tl) = scan_until_eos assign_tl in
    let exp = parse_lex_expression raw_exp in
    Some (Assignment (id, exp), tl)
  | Increment :: LexValue (LexId id) :: tl ->
    Some (Assignment (id, add_exp id "plus"), tl)
  | Decrement :: LexValue (LexId id) :: tl ->
    Some (Assignment (id, add_exp id "minus"), tl)
  | _ -> None

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

let parse_test_chain chunks =
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

let is_parsable_test =
  function
  | If :: _ | Case :: _ | Unless :: _ -> true
  | _ -> false


let parse_test = function
  | tokens when is_parsable_test tokens ->
    let bounds = Bounds.find_bounds tokens 0 in
    let chunks = bounds |> Bounds.bounds_to_chunks tokens in
    let stop_point = Bounds.stop_point_from_bounds bounds in
    let rest = sub_list_suffix tokens stop_point in
    Some (parse_test_chain chunks, rest)
  | _ -> None

let parse_capture = function
    | Keyword.Capture :: LexValue (LexId id) :: tl ->
      let tokens = [Keyword.Capture] @ tl in
      let bounds = Bounds.find_bounds tokens 0 in
      let stop_point = Bounds.stop_point_from_bounds bounds in
      let body = List.sub tokens ~pos:0 ~len:(stop_point) in
      let rest = sub_list_suffix tokens stop_point in

      let capture = Capture (id, InProgress body) in
      Some (capture, rest)
    | _ -> None

let parse_other  = function
    | LexText t :: tl -> Some (Text t, tl)
    | Newline :: tl -> Some (Text "\n", tl)
    | EOS :: tl -> Some (Nothing, tl)
    | _ -> None

let parse_one tokens =
  let parsers = [parse_assignment; parse_expression; parse_test; parse_capture; parse_other] in
  let found_parser =
    List.find parsers ~f:(
      fun parser -> match parser tokens with Some(_) -> true | None -> false
    ) in

  match found_parser with
  | Some(parser) -> (
    match parser tokens with
    | Some (got, rest) -> Some (got, rest)
    | None -> None
  )
  | None -> None

let parse_block init_tokens =
  let folder block tokens =
    Debug.dump (tokens);
    Debug.print_line ();
    match parse_one tokens with
    | Some (got, rest) ->
      Next (block @ (if got != Nothing then [got] else []), rest)
    | None ->
      Stop (block)
  in

  Block (unfold [] init_tokens folder)


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