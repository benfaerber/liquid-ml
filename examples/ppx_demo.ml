open Liquid_syntax
open Syntax

(* Demonstration of auto-generated show functions *)

let demo_operator () =
  let op = Eq in
  Stdio.print_endline ("Operator: " ^ show_operator op)

let demo_lex_token () =
  let tokens = [If; Space; LexValue (LexBool true); EndIf] in
  Stdio.print_endline "Tokens:";
  List.iter (fun tok -> Stdio.print_endline ("  " ^ show_lex_token tok)) tokens

let demo_value () =
  let values = [
    Bool true;
    String "hello";
    Number 42.5;
    Var ["user"; "name"];
    List [Number 1.; Number 2.; Number 3.];
    Nil
  ] in
  Stdio.print_endline "\nValues:";
  List.iter (fun v -> Stdio.print_endline ("  " ^ show_value v)) values

let demo_condition () =
  let cond = Equation (Number 5., Gt, Number 3.) in
  Stdio.print_endline ("\nCondition: " ^ show_condition cond)

let demo_expression () =
  let expr = Func ("upcase", [Value (String "hello")]) in
  Stdio.print_endline ("\nExpression: " ^ show_expression expr)

let demo_ast () =
  let ast = Block [
    Text "Hello ";
    Expression (Value (Var ["name"]));
    Text "!";
  ] in
  Stdio.print_endline ("\nAST: " ^ show_ast ast)

let () =
  Stdio.print_endline "=== PPX Deriving Demo ===\n";
  demo_operator ();
  demo_lex_token ();
  demo_value ();
  demo_condition ();
  demo_expression ();
  demo_ast ();
  Stdio.print_endline "\n=== Demo Complete ==="
