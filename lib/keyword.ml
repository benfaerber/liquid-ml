open Base
open Tools

type blockToken =
  | StatementStart
  | StatementEnd
  | ExpressionStart
  | ExpressionEnd
  | LiquidStart
  | Other of string


type block =
  | Text of string
  | Statement of string
  | Expression of string
  | Liquid of string


type token =
  | If | Else | EndIf
  | Unless | EndUnless
  | Case | EndCase | When
  | For | EndFor
  | Capture | EndCapture
  | Break | Continue
  | Cycle | TableRow | EndTableRow
  | Paginate | EndPaginate
  | In | Contains | By
  | Assign | Increment | Decrement
  | Eq | Gte | Gt | Lte | Lt | Ne
  | Pipe | Colon | Assignment | Comma
  | Space | Newline
  | Bool of bool
  | String of string
  | Number of float
  | Id of string


let lex_keyword text =
  let keywords =
    [ ("if", If)
    ; ("else", Else)
    ; ("endif", EndIf)
    ; ("unless", Unless)
    ; ("endunless", EndUnless)
    ; ("case", Case)
    ; ("endcase", EndCase)
    ; ("when", When)

    ; ("for", For)
    ; ("endfor", EndFor)
    ; ("capture", Capture)
    ; ("endcapture", EndCapture)

    ; ("break", Break)
    ; ("continue", Continue)
    ; ("cycle", Cycle)
    ; ("tablerow", TableRow)
    ; ("endtablerow", EndTableRow)

    ; ("in", In)
    ; ("contains", Contains)
    ; ("by", By)

    ; ("assign", Assign)
    ; ("increment", Increment)
    ; ("decrement", Decrement)

    ; ("==", Eq)
    ; (">=", Gte)
    ; (">", Gt)
    ; ("<=", Lte)
    ; ("<", Lt)
    ; ("!=", Ne)

    ; (":", Colon)
    ; ("|", Pipe)
    ; ("=", Assignment)
    ; (",", Comma)
    ; (" ", Space)
    ; ("\n", Newline)
  ] in

  let found_keyword =
    List.find keywords ~f:(fun (check_literal, _) -> starts_with text check_literal) in

  match found_keyword with
  | Some (literal, token) -> (
    let trimmed = String.sub text ~pos:(String.length literal) ~len:(String.length text - String.length literal) in
    (Some (token), trimmed))
  | None -> (None, text)


let block_token_as_string = function
  | StatementStart -> "StatementStart"
  | StatementEnd -> "StatementEnd"
  | ExpressionStart -> "ExpressionStart"
  | ExpressionEnd -> "ExpressionEnd"
  | LiquidStart -> "LiquidStart"
  | Other(oth) -> oth


let block_as_string = function
  | Text(t) -> Core.sprintf "(( %s ))" t
  | Statement(t) -> Core.sprintf "{0 %s 0}" t
  | Expression(t) -> Core.sprintf "{{ %s }}" t
  | Liquid(t) -> Core.sprintf "{0liq %s 0}" t

let token_as_string = function
  | If -> "If" | Else -> "Else" | EndIf -> "Endif"
  | Unless -> "Unless" | EndUnless -> "EndUnless"
  | Case -> "Case" | EndCase -> "EndCase" | When -> "When"
  | For -> "For" | EndFor -> "EndFor"
  | Capture -> "Capture" | EndCapture -> "EndCapture"
  | Break -> "Break" | Continue -> "Continue"
  | Cycle -> "Cycle" | TableRow -> "TableRow" | EndTableRow -> "EndTableRow"
  | Paginate -> "Paginate" | EndPaginate -> "EndPaginate"
  | In -> "In" | Contains -> "Contains"
  | Assign -> "Assign" | Increment -> "Increment" | Decrement -> "Decrement"
  | Eq -> "Eq" | Gte -> "Gte" | Gt -> "Gt" | Lte -> "Lte" | Lt -> "Lt" | Ne -> "Ne"
  | Pipe -> "Pipe" | Colon -> "Colon" | Assignment -> "Assignment" | Comma -> "Comma"
  | Space -> "Space" | Newline -> "NewLine"
  | Bool(b) -> "Bool(" ^ (if b then "True" else "False") ^ ")"
  | String(s) -> "String(" ^ s ^ ")"
  | Number(f) -> Core.sprintf "Num(%f)" f
  | Id(id) -> "Id(" ^ id ^ ")"
  | _ -> "Unknown"

let block_tokens_as_string bts = String.concat ~sep:"; " (List.map bts ~f:block_token_as_string)
let tokens_as_string ts = String.concat ~sep:"; " (List.map ts ~f:token_as_string)