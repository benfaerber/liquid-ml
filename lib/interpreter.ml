open Base
open Syntax

let interpret_expression _ = String ("NYI")

let interpret_equation _ _ = true

let not b = if b then false else true

let rec interpret_condition ctx cond =
  match cond with
  | AlwaysTrue -> true
  | Not inner -> not (interpret_condition ctx inner)
  | Equation eq -> interpret_equation ctx eq
  | Combine (And, l, r) -> interpret_condition ctx l && interpret_condition ctx r
  | Combine (Or, l, r) -> interpret_condition ctx l || interpret_condition ctx r

let rec interpret ctx str ast =
  let no_match = ctx, str in
  match ast with
  | Block cmds -> (
    Debug.print_line();
    Debug.print_ast (Block cmds);
    List.fold_left cmds ~init:(ctx, str) ~f:(fun (acc_ctx, acc_str) cmd -> interpret acc_ctx acc_str cmd)
  )
  | Assignment (id, exp) -> ctx @ [(id, interpret_expression exp)], ""
  | Test (cond, body, else_body) -> (
    if interpret_condition ctx cond then
      interpret ctx str body
    else
      (match else_body with Some eb -> interpret ctx str eb | None -> no_match)
  )
  | Text t -> ctx, str ^ t
  | Capture (id, _) -> ctx @ [(id, String ("NYI"))], str
  | _ -> no_match

let interpret_file filename =
  let ast =
    filename
    |> File.read
    |> Preprocessor.preprocess
    |> Lexer.lex_text
    |> Parser.parse_block in


  Debug.print_ast ast;
  Debug.print_line();

  let default_ctx = [] in
  let default_str = "" in

  let (final_ctx, final_str) = interpret default_ctx default_str ast in
  Debug.print_variable_context final_ctx;
  Stdio.print_endline "Render:";
  Stdio.print_endline final_str;
  ()

let test () =
  interpret_file "liquid/interpreter_test.liquid"