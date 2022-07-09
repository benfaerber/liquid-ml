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

type token =
  | If of tag_position
  | Else
  | Unless of tag_position
  | Case of tag_position
  | When
  | For of tag_position
  | Capture of tag_position
  | Paginate of tag_position
  | TableRow of tag_position
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
    [ ("if", If Open)
    ; ("else", Else)
    ; ("endif", If Close)
    ; ("unless", Unless Open)
    ; ("endunless", Unless Close)
    ; ("case", Case Open)
    ; ("endcase", Case Close)
    ; ("when", When)

    ; ("for", For Open)
    ; ("endfor", For Close)
    ; ("capture", Capture Open)
    ; ("endcapture", Capture Close)

    ; ("break", Break)
    ; ("continue", Continue)
    ; ("cycle", Cycle)
    ; ("tablerow", TableRow Open)
    ; ("endtablerow", TableRow Close)

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
