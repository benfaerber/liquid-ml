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

let pair p op cl =
  match p with
  | Open _ -> op
  | Close _ -> cl
  | _ -> raise(Failure "Invalid pair")

let pair_tag_as_string p = function
  | If -> pair p "If" "EndIf"
  | Unless -> pair p "Unless" "EndUnless"
  | For -> pair p "For" "EndFor"
  | Case -> pair p "Case" "EndCase"
  | Capture -> pair p "Capture" "EndCapture"
  | Paginate -> pair p "Paginate" "EndPaginate"
  | TableRow -> pair p "TableRow" "EndTableRow"

let token_as_string = function
| Open pt -> pair_tag_as_string (Open pt) pt
| Close pt -> pair_tag_as_string (Close pt) pt
| Else -> "Else"
| When -> "When"
| Break -> "Break" | Continue -> "Continue"
| Cycle -> "Cycle"
| In -> "In"
| Assign -> "Assign" | Increment -> "Increment" | Decrement -> "Decrement"
| Pipe -> "Pipe" | Colon -> "Colon" | Equals -> "Equals" | Comma -> "Comma"
| Space -> "Space" | Newline -> "NewLine"
| Operator op -> operator_as_string op
| Bool(b) -> "Bool(" ^ (if b then "True" else "False") ^ ")"
| String(s) -> "String(" ^ s ^ ")"
| Number(f) -> Core.sprintf "Num(%f)" f
| Id(id) -> "Id(" ^ id ^ ")"
| _ -> "Unknown"

let block_tokens_as_string bts = String.concat ~sep:" " (List.map bts ~f:block_token_as_string)
let tokens_as_string ts = String.concat ~sep:" " (List.map ts ~f:token_as_string)

let block_as_string = function
| Text(t) -> if eq t "\n" then "\n" else Core.sprintf "(( %s ))" t
| Statement(t) -> Core.sprintf "{# %s #}" (tokens_as_string t)
| Expression(t) -> Core.sprintf "{{ %s }}" (tokens_as_string t)
| Liquid(t) -> Core.sprintf "{0liquid\n%s\n0}" (tokens_as_string t)

let blocks_as_string bs = String.concat ~sep:" " (List.map bs ~f:block_as_string)