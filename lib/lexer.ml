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


let lex_bool text =
  let literal_true = "true" in
  let literal_false = "false" in

  if starts_with text literal_true then
    Some(Keyword.Bool(true)), remove_prefix text literal_true
  else if starts_with text literal_false then
    Some(Keyword.Bool(false)), remove_prefix text literal_false
  else
    None, text


let lex_number text =
  let rec lex_digit_group_aux t acc =
    let chunk = String.sub t ~pos:(List.length acc) ~len:1 in
    match chunk with
    | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      lex_digit_group_aux t (acc @ [chunk])
    | _ -> acc
  in

  let lex_digit_group t = lex_digit_group_aux t [] |> String.concat ~sep:"" in

  let to_num v = Some (Keyword.Number(v |> Float.of_string)) in

  let (neg_literal, t_text) =
    if starts_with text "-" then
      ("-", remove_prefix text "-")
    else ("", text) in

  match lex_digit_group t_text with
  | "" -> None, t_text
  | first_group -> (
    let t_first_group = neg_literal ^ first_group in
    let decimal_part = remove_prefix t_text first_group in
    if starts_with decimal_part "." then (
      let second_group_part = remove_prefix decimal_part "." in
      match lex_digit_group second_group_part with
      | "" -> to_num(t_first_group), second_group_part
      | second_group -> to_num(t_first_group ^ "." ^ second_group), remove_prefix second_group_part second_group
    ) else (
      to_num(t_first_group), decimal_part
    )
  )


let lex_string text =
  if starts_with text "\"" then
    let folder acc index =
      match String.sub text ~pos:(index+1) ~len:2 with
      | "\\\"" -> Next (acc ^ "\\\"", index + 2)
      | other -> (
        match first_letter other with
        | "\"" -> Stop (acc)
        | other_letter -> Next (acc ^ other_letter, index + 1))
    in

    let string_literal = unfold "" 0 folder in
    let complete_literal = "\"" ^ string_literal ^ "\"" in

    Stdio.printf "(%s) \n" complete_literal;
    Some (Keyword.String(string_literal)), remove_prefix text complete_literal
  else
    None, text


let lex_id _ = None, ""

let lex_token text =
  let lexers = [Keyword.lex_keyword; lex_bool; lex_string; lex_number; lex_id] in
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
      Next (acc @ [Keyword.Assign], index+7)
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
