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

type tag_position = Open | Close

type pair_tag =
  | If
  | Unless
  | Case
  | For
  | Capture
  | Paginate
  | TableRow

type token =
  | Open of pair_tag
  | Close of pair_tag
  | Else
  | When
  | Break | Continue
  | Cycle
  | In | By
  | Assign | Increment | Decrement
  | Pipe | Colon | Equals | Comma
  | Space | Newline
  | Operator of operator
  | Bool of bool
  | String of string
  | Number of float
  | Id of string

type block =
  | Text of string
  | Statement of token list
  | Expression of token list
  | Liquid of token list


let lex_keyword text =
  let keywords =
    [ ("if", Open If)
    ; ("else", Else)
    ; ("endif", Close If)
    ; ("unless", Open Unless)
    ; ("endunless", Close Unless)
    ; ("case", Open Case)
    ; ("endcase", Close Case)
    ; ("when", When)

    ; ("for", Open For)
    ; ("endfor", Close For)
    ; ("capture", Open Capture)
    ; ("endcapture", Close Capture)

    ; ("break", Break)
    ; ("continue", Continue)
    ; ("cycle", Cycle)
    ; ("tablerow", Open TableRow)
    ; ("endtablerow", Close TableRow)

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

    ; (":", Colon)
    ; ("|", Pipe)
    ; ("=", Equals)
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
