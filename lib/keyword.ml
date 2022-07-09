open Base
open Tools

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
