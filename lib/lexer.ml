open Base

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


let range x =
  Batteries.(--) 0 x |> Batteries.List.of_enum

let sub_prefix text x = String.sub text ~pos:0 ~len:x
let sub_suffix text x = String.sub text ~pos:x ~len:(String.length text)
let first_letter text = sub_prefix text 1

let starts_with text prefix =
  if String.length text > String.length prefix then (
    let rprefix = sub_prefix text (String.length prefix) in
    (Caml.(=)) rprefix prefix
  ) else false

type ('acc, 'curr) loop =
  | Next of ('acc * 'curr)
  | Stop of 'acc

let rec unfold acc curr func =
  match func acc curr with
  | Next((nacc, ncurr)) -> unfold nacc ncurr func
  | Stop(acc) -> acc


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
  | Space
  | Bool of bool
  | String of string
  | Number of int
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
  ] in

  let found_keyword =
    List.find keywords ~f:(fun (check_literal, _) -> starts_with text check_literal) in

  match found_keyword with
  | Some (literal, token) -> (
    let trimmed = String.sub text ~pos:(String.length literal) ~len:(String.length text - String.length literal) in
    (Some (token), trimmed))
  | None -> (None, text)


let lex_bool _ = None, ""

let lex_string _ = None, ""

let lex_number _ = None, ""

let lex_id _ = None, ""

let lex_token text =
  let lexers = [lex_keyword; lex_bool; lex_string; lex_number; lex_id] in
  let found_lexer =
    List.find lexers ~f:(
      fun lexer -> match lexer text with Some(_), _ -> true | None, _ -> false
    ) in

  match found_lexer with
  | Some(lexer) -> lexer text
  | None -> (None, text)

let block_token_of_string = function
  | "{%" -> StatementStart
  | "%}" -> StatementEnd
  | "{{" -> ExpressionStart
  | "}}" -> ExpressionEnd
  | other -> Other(other)

let lex_block_tokens text =
  let folder acc index =
    if index + 2 < (String.length text) then
      let chunk = String.sub text ~pos:index ~len:2 in
      match chunk with
      | "{%" | "%}" | "{{" | "}}" -> Next (acc @ [block_token_of_string chunk], index+2)
      | other -> (
        match acc |> List.rev with
        | Other("liquid") :: StatementStart :: hds -> Next ((List.rev hds) @ [LiquidStart], index+6)
        | Other(tl) :: hds -> Next (List.rev hds @ [Other(tl ^ first_letter other)], index+1)
        | _ -> Next (acc @ [Other(first_letter other)], index+1)
      )
    else
      Stop(acc)
  in

  unfold [] 0 folder

let lex_tokens text =
  let folder acc index =
    let chunk = String.sub text ~pos:index ~len:(String.length text - index) in

    if starts_with chunk "assign" then
      Next (acc @ [Assign], index+7)
    else
      Stop (acc)
  in

  unfold [] 0 folder

let test () =
  (* "liquid/block_test.liquid"
  |> File.read
  |> lex_block_tokens
  |> Batteries.dump
  |> Stdio.printf "%s"; *)

  (* (String.sub "just a test" ~pos:4 ~len: 5) |> Stdio.print_endline;; *)
  "assign animal = \"sheep\"" |> lex_token |> Batteries.dump |> Stdio.printf "%s";;

  Stdio.print_string "\n"
