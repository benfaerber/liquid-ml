open Base
open Tools

type block_token =
  | StatementStart
  | StatementEnd
  | ExpressionStart
  | ExpressionEnd
  | LiquidStart
  | RawText of string

type operator =
  Eq | Gte | Gt | Lte | Lt | Ne | Contains

type lex_value =
  | LexBool of bool
  | LexString of string
  | LexNumber of float
  | LexId of string
  | LexRange of int * int
  | LexNil
  | LexBlank

type lex_token =
  | If | EndIf
  | Unless | EndUnless
  | Case | EndCase
  | For | EndFor
  | Capture | EndCapture
  | Paginate | EndPaginate
  | TableRow | EndTableRow
  | ElseIf
  | Else
  | When
  | Break | Continue
  | Cycle
  | In | By
  | Assign | Increment | Decrement
  | Pipe | Colon | Equals | Comma
  | DotDot
  | Space | Newline
  | Operator of operator
  | And | Or
  | Text of string
  | Value of lex_value
  | Expression of lex_token list

let lex_keyword text =
  let keywords =
    [ ("if", If)
    ; ("else", Else)
    ; ("elsif", ElseIf)
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
    ; ("by", By)

    ; ("assign", Assign)
    ; ("increment", Increment)
    ; ("decrement", Decrement)

    ; ("==", Operator Eq)
    ; (">=", Operator Gte)
    ; (">", Operator Gt)
    ; ("<=", Operator Lte)
    ; ("<", Operator Lt)
    ; ("!=", Operator Ne)
    ; ("<>", Operator Ne)
    ; ("contains", Operator Contains)
    ; ("and", And)
    ; ("or", Or)

    ; (":", Colon)
    ; ("|", Pipe)
    ; ("=", Equals)
    ; (",", Comma)
    ; (" ", Space)
    ; ("..", DotDot)
    ; ("\n", Newline)

    ; ("nil", Value LexNil)
    ; ("blank", Value LexBlank)
  ] in

  let found_keyword =
    List.find keywords ~f:(fun (check_literal, _) -> starts_with text check_literal) in

  match found_keyword with
  | Some (literal, token) -> (
    let trimmed = remove_prefix text literal in
    (Some token, trimmed))
  | None -> (None, text)


let block_token_of_string = function
  | "{%" -> StatementStart
  | "%}" -> StatementEnd
  | "{{" -> ExpressionStart
  | "}}" -> ExpressionEnd
  | other -> RawText(other)

let is_block_token_string = fun x ->
  match block_token_of_string x with
  | StatementStart | StatementEnd | ExpressionStart | ExpressionEnd -> true
  | _ -> false
