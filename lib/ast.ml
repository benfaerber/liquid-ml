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
  | _ :: statement -> (
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
      | _ -> acc
    in
    let res = aux AlwaysTrue statement in
    if is_unless then Not res else res
  )
  | _ -> raise (Failure "Invalid token list")


let scan_until_newline tokens =
  let rec aux acc = function
    | hd :: _ when hd = Newline -> acc
    | hd :: tl -> aux (acc @ [hd]) tl
    | [] -> acc
  in
  let res = aux [] tokens in
  (res, remove_list_prefix tokens res)

let build_test_statement chunk =
  let (cond_tokens, body_tokens) = chunk |> scan_until_newline in
  let condition = build_condition cond_tokens in
  (condition, InProgress body_tokens)

let build_test_chain chunks =
  let rec aux pool =
    match pool with
    | fs :: tl ->
      let (condition, body) = build_test_statement fs in
      Some (Test (condition, body, aux tl))
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

let test () =
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

  Stdio.print_endline "";