open Base
open Keyword
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

let newline_as_token = false
let rec lex_token_as_string = function
  | If -> "If" | EndIf -> "EndIf"
  | Unless -> "Unless" | EndUnless -> "EndUnless"
  | For -> "For" | EndFor -> "EndFor"
  | Case -> "Case" | EndCase -> "EndCase"
  | Capture -> "Capture" | EndCapture -> "EndCapture"
  | Paginate -> "Paginate" | EndPaginate -> "EndPaginate"
  | TableRow -> "TableRow" | EndTableRow -> "EndTableRow"
  | Else -> "Else"
  | ElseIf -> "Else If"
  | When -> "When"
  | Break -> "Break" | Continue -> "Continue"
  | Cycle -> "Cycle"
  | In -> "In"
  | Assign -> "Assign" | Increment -> "Increment" | Decrement -> "Decrement"
  | Pipe -> "Pipe" | Colon -> "Colon" | Equals -> "Equals" | Comma -> "Comma"
  | Space -> "Space"
  | Newline -> if newline_as_token then "\n\\n" else "\n"
  | Operator op -> operator_as_string op
  | Bool(b) -> "Bool(" ^ (if b then "True" else "False") ^ ")"
  | String(s) -> "String(" ^ s ^ ")"
  | Number(f) -> Core.sprintf "Num(%f)" f
  | Id(id) -> "Id(" ^ id ^ ")"
  | Text(t) -> if eq t "\n" then "\n" else "Text(" ^ t ^ ")"
  | Range(s, e) -> "Range(" ^ (Int.to_string s) ^ ", " ^ (Int.to_string e) ^ ")"
  | Expression(e) ->
    "Expression<\n  " ^ join_by_space (List.map e ~f:lex_token_as_string) ^ "\n>"
  | _ -> "Unknown"

let block_tokens_as_string bts = join_by_space (List.map bts ~f:block_token_as_string)
let lex_tokens_as_string ts = join_by_space (List.map ts ~f:lex_token_as_string)
let lex_tokens_as_string_with_index ts =
  join_by_space (List.mapi ts ~f:(
    fun i t -> (i |> Int.to_string) ^ ": " ^ lex_token_as_string t
  ))
