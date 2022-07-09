open Base
open Tools
open Keyword

let lex_bool text =
  let literal_true = "true" in
  let literal_false = "false" in

  if starts_with text literal_true then
    Some(Bool(true)), remove_prefix text literal_true
  else if starts_with text literal_false then
    Some(Bool(false)), remove_prefix text literal_false
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

  let to_num v = Some (Number(v |> Float.of_string)) in

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

    Some (String(string_literal)), remove_prefix text complete_literal
  else
    None, text


let lex_id text =
  let alpha = "abcdefghijklmnopqrstuvwxyz" in
  let alpha_upper = alpha |> String.uppercase in
  let digits = "0123456679" in
  let valid_first_letters = alpha ^ alpha_upper ^ "_" |> String.to_list  in
  let valid_letters = valid_first_letters @ (digits |> String.to_list) in

  let contains lst item =
    List.mem lst item ~equal:(Caml.(=))
  in

  let to_char x = x.[0] in

  if contains valid_first_letters (first_letter text |> to_char) then (
    let folder acc index =
      let letter = String.sub text ~pos:(index) ~len:1 in
      if contains valid_letters (letter |> to_char) then (
        Next ((acc ^ letter, index + 1))
      ) else (
        Stop (acc)
      )
    in

    let id_literal = first_letter text ^ (unfold "" 1 folder) in
    (Some (Id(id_literal)), remove_prefix text id_literal)
  ) else (
    (None, text)
  )


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
  let t_text = text ^ " " in
  let folder acc index =
    let chunk = String.sub t_text ~pos:index ~len:(String.length t_text - index) in
    match lex_token chunk with
    | Some(t), rest -> Next (acc @ [t], String.length t_text - String.length rest)
    | None, _ -> Stop (acc)
  in

  let raw_list = unfold [] 0 folder in
  let wo_spaces = List.filter raw_list ~f:(fun token ->
    match token with
    | Space -> false
    | _ -> true
  ) in

  wo_spaces

let test () =
  let block_tokens =
    "liquid/block_test.liquid"
    |> File.read
    |> lex_block_tokens in

  Stdio.print_endline "Block Tokens: ";
  block_tokens |> block_tokens_as_string |> Stdio.print_endline;
  (* let closing = Ast.build_tree block_tokens in *)
  (* closing |> Batteries.dump |> Stdio.print_endline; *)

  (* "liquid/block_test.liquid"
  |> File.read
  |> lex_block_tokens
  |> Batteries.dump
  |> Stdio.printf "%s"; *)

  (* (String.sub "just a test" ~pos:4 ~len: 5) |> Stdio.print_endline;; *)
  (* "_da_apple -12.12 \"aniaml\" = \"sheep\"" |> lex_tokens |> Batteries.dump |> Stdio.printf "%s";; *)

  Stdio.print_string "\n"
