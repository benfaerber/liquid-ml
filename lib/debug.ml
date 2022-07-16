open Base
open Keyword
open Syntax
open Tools

let block_token_as_string = function
  | StatementStart -> "StatementStart"
  | StatementEnd -> "StatementEnd"
  | ExpressionStart -> "ExpressionStart"
  | ExpressionEnd -> "ExpressionEnd"
  | LiquidStart -> "LiquidStart"
  | RawText(oth) -> oth


let operator_as_string = function
  | Eq -> "Eq" | Gte -> "Gte" | Gt -> "Gt"
  | Lte -> "Lte" | Lt -> "Lt" | Ne -> "Ne"
  | Contains -> "Contains"

let lex_value_as_string = function
  | LexBool(b) -> "Bool(" ^ (if b then "True" else "False") ^ ")"
  | LexString(s) -> "String(" ^ s ^ ")"
  | LexNumber(f) -> Core.sprintf "Num(%f)" f
  | LexId(id) -> "Id(" ^ join_by_arrow id ^ ")"
  | LexRange(s, e) -> "Range(" ^ (Int.to_string s) ^ ", " ^ (Int.to_string e) ^ ")"
  | LexNil | LexBlank -> "Nil"

let lex_combiner_as_string = function LexAnd -> "And" | LexOr -> "Or"

let newline_as_token = true
let rec lex_token_as_string = function
  | If -> "If" | EndIf -> "EndIf"
  | Unless -> "Unless" | EndUnless -> "EndUnless"
  | For -> "For" | EndFor -> "EndFor"
  | Case -> "Case" | EndCase -> "EndCase"
  | Capture -> "Capture" | EndCapture -> "EndCapture"
  | Paginate -> "Paginate" | EndPaginate -> "EndPaginate"
  | TableRow -> "TableRow" | EndTableRow -> "EndTableRow"
  | Raw -> "Raw" | EndRaw -> "EndRaw"
  | Else -> "Else"
  | ElseIf -> "ElseIf"
  | When -> "When"
  | Break -> "Break" | Continue -> "Continue"
  | Cycle -> "Cycle"
  | In -> "In"
  | Assign -> "Assign" | Increment -> "Increment" | Decrement -> "Decrement"
  | Pipe -> "Pipe" | Colon -> "Colon" | Equals -> "Equals" | Comma -> "Comma"
  | LexCombiner c -> lex_combiner_as_string c
  | Space -> "Space"
  | Newline -> if newline_as_token then "\\n\n" else "\n"
  | Operator op -> operator_as_string op
  | LexValue v -> lex_value_as_string v
  | LexText t -> if eq t "\n" then "\n" else "Text(" ^ t ^ ")"
  | EOS -> "EOS"
  | LexExpression(e) ->
    "Expression<\n  " ^ join_by_space (List.map e ~f:lex_token_as_string) ^ "\n>"
  | _ -> "Unknown"

let block_tokens_as_string bts = join_by_space (List.map bts ~f:block_token_as_string)
let print_block_tokens bts = bts |> block_tokens_as_string |> Stdio.print_endline

let lex_tokens_as_string ts = join_by_space (List.map ts ~f:lex_token_as_string)
let print_lex_tokens ts = ts |> lex_tokens_as_string |> Stdio.print_endline

let lex_tokens_as_string_with_index ts =
  join_by_space (List.mapi ts ~f:(
    fun i t -> (i |> Int.to_string) ^ ": " ^ lex_token_as_string t
  ))
let print_lex_tokens_with_index ts = ts |> lex_tokens_as_string_with_index |> Stdio.print_endline

let combiner_as_string = function
  | And -> "And"
  | Or -> "Or"


let value_as_string = function
  | Bool(b) -> "Bool(" ^ (if b then "True" else "False") ^ ")"
  | String(s) -> "String(" ^ s ^ ")"
  | Number(f) -> Core.sprintf "Num(%f)" f
  | Var(v) -> "Var(" ^ join_by_arrow v ^ ")"
  | Nil -> "Nil"
  | Skip -> "Skip"
  | _ -> "Unknown"


let rec condition_as_string =
  let rec aux = function
  | Equation (a, op, b) -> (value_as_string a) ^ " " ^ (operator_as_string op) ^ " " ^ (value_as_string b)
  | AlwaysTrue -> "Always True"
  | Not x -> "Not(\n" ^ (condition_as_string x) ^ "\n)"
  | Combine (combiner, conditions) ->
    (combiner_as_string combiner) ^ "(\n  " ^ (join_by_space (List.map conditions ~f:aux)) ^ "\n)"
  in aux

let print_condition c = c |> condition_as_string |> Stdio.print_endline

let rec expression_as_string = function
  | Value v -> value_as_string v
  | Func (n, e) -> "f:" ^ n ^ "(\n  " ^ (join_by_space (List.map e ~f:expression_as_string)) ^ "\n)"

let print_expression e = e |> expression_as_string |> Stdio.print_endline

let tab l =
  List.map (range l) ~f:(fun _ -> "  ") |> join

let show_in_progress = false
let ast_as_string =
  let rec aux depth a =
    let t = tab depth in
    let result = match a with
    | Test (condition, child, next_child_opt) -> (
      let child_text =
        Core.sprintf "\nCondition( %s )\n{  %s\n}\n" (condition |> condition_as_string) (aux (depth+1) child) in

      let next_child_text =
        match next_child_opt with
        | Some next_child ->
          Core.sprintf "{ %s }\n" (aux (depth+1) next_child)
        | None -> "" in

      child_text ^ next_child_text
    )
    | For (name, value, params, body, else_block) -> (
      let vars = Core.sprintf "(l=%s,o=%s,r=%s)" (value_as_string params.limit) (value_as_string params.offset) (value_as_string params.reved) in
      match else_block with
      | Some eb ->
        Core.sprintf "For(%s in %s %s)\n{  %s\n}\n{  %s}" name (value_as_string value) vars (aux (depth+1) body) (aux (depth+1) eb)
      | None ->
        Core.sprintf "For(%s in %s %s)\n{  %s\n}\n" name (value_as_string value) vars (aux (depth+1) body)
    )
    | InProgress tokens -> if show_in_progress then lex_tokens_as_string tokens else "InProgress"
    | Expression exp -> "Exp(" ^ expression_as_string exp ^ ")"
    | Assignment (name, exp) -> Core.sprintf "Assign(%s: %s)" name (expression_as_string exp)
    | Text t -> if String.strip t = "" then "" else Core.sprintf "t(%s)" t
    | Capture (id, body) -> Core.sprintf "Capture(%s: %s)" id (aux (depth+1) body)
    | Block items -> "Block(\n" ^ (List.map items ~f:(aux (depth+1)) |> join_by_space) ^ ")"
    | Break -> "Break"
    | Continue -> "Continue"
    | _ -> "Other" in

    if String.strip result = "" then "" else
    "\n" ^ t ^ result
    in aux 0

let print_ast ast = ast |> ast_as_string |> Stdio.print_endline

let print_line () = Stdio.print_endline "----------------------------------------------------------"

let dump x =
  x |> Batteries.dump |> Stdio.print_endline

let parse_result_as_string = function
  | Some (ast, _) -> Core.sprintf "Ast:\n%s" (ast_as_string ast)
  | None -> Core.sprintf "Parse Result None"

let parse_result_with_rest_as_string = function
  | Some (ast, tokens) -> Core.sprintf "Ast:\n%s\nRest:\n%s" (ast_as_string ast) (lex_tokens_as_string tokens)
  | None -> Core.sprintf "Parse Result None"

let print_parse_result pr = pr |> parse_result_as_string |> Stdio.print_endline
let print_parse_result_with_rest pr = pr |> parse_result_with_rest_as_string |> Stdio.print_endline