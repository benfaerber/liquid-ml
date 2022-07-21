open Base
open Tools
open Keyword

let lex_bool text =
  let literal_true = "true" in
  let literal_false = "false" in

  if starts_with text literal_true then
    Some(LexValue (LexBool true), remove_prefix text literal_true)
  else if starts_with text literal_false then
    Some(LexValue (LexBool false), remove_prefix text literal_false)
  else
    None


let lex_digit_group text =
  let rec lex_digit_group_aux t acc =
    let chunk = String.sub t ~pos:(List.length acc) ~len:1 in
    match chunk with
    | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      lex_digit_group_aux t (acc @ [chunk])
    | _ -> acc
  in

  lex_digit_group_aux text [] |> join

let lex_number text =
  let to_num v t = Some (LexValue (LexNumber(v |> Float.of_string)), t) in

  let (neg_literal, t_text) =
    if starts_with text "-" then
      ("-", remove_prefix text "-")
    else ("", text) in

  match lex_digit_group t_text with
  | "" -> None
  | first_group -> (
    let t_first_group = neg_literal ^ first_group in
    let decimal_part = remove_prefix t_text first_group in
    if starts_with decimal_part "." then
      let second_group_part = remove_prefix decimal_part "." in
      match lex_digit_group second_group_part with
      | "" -> to_num t_first_group second_group_part
      | second_group -> to_num (t_first_group ^ "." ^ second_group) (remove_prefix second_group_part second_group)
    else
      to_num t_first_group decimal_part
  )

let has_prefix_or_fail text prefix func =
  if starts_with text prefix then
    remove_prefix text prefix |> func
  else
    None

let lex_range text =
  let (popen, pclose) = "(", ")" in
  let dotdot = ".." in
  has_prefix_or_fail text popen (fun wo_paren ->
    match lex_digit_group wo_paren with
    | "" -> None
    | first_number ->
      let after_first = remove_prefix wo_paren first_number in
      has_prefix_or_fail after_first dotdot (fun wo_dot ->
        match lex_digit_group wo_dot with
        | "" -> None
        | second_number ->
          let after_second = remove_prefix wo_dot second_number in
          if starts_with after_second pclose then
            let range = LexValue (LexRange (Int.of_string first_number, Int.of_string second_number)) in
            Some (range, (remove_prefix after_second pclose))
          else
            None
      )
  )


let lex_delimited_string delim escaped_delim text =
  if starts_with text delim then
    let d_len = String.length escaped_delim in
    let folder acc index =
      match String.sub text ~pos:(index+1) ~len:d_len with
      | e when e = escaped_delim -> Next (acc ^ escaped_delim, index + d_len)
      | other -> (
        match first_letter other with
        | e when e = delim -> Stop (acc)
        | other_letter -> Next (acc ^ other_letter, index + 1))
    in

    let string_literal = unfold "" 0 folder in
    let complete_literal = delim ^ string_literal ^ delim in

    Some (LexValue (LexString string_literal), remove_prefix text complete_literal)
  else
    None

let lex_string text =
  let double_quote = lex_delimited_string "\"" "\\\"" in
  let single_quote = lex_delimited_string "\'" "\\\'" in

  match (double_quote text, single_quote text) with
  | (Some (r, rest), _) -> Some (r, rest)
  | (_, Some (r, rest)) -> Some (r, rest)
  | _ -> None


let rec first_successful (text: string) =
  function
  | lexer :: other_lexers -> (
    match lexer text with
    | Some (got, rest) -> Some (got, rest)
    | _ -> first_successful text other_lexers
  )
  | _ -> None

let (~/) = Re2.create_exn
let lex_id text =
  let id_exp = ~/"^[a-zA-Z_](?:(?:[a-zA-Z0-9_\\-\\.]|((\\[(\"|\')?.+)(\"|\')?\\]))+)?" in
  if Re2.matches id_exp text then
    let literal = Re2.find_first_exn id_exp text in
    let bracket_group_exp = ~/"\\[(?:\"|')?(.+?)(?:\"|')?\\]" in
    let wo_bg =
      match Re2.find_all bracket_group_exp text with
      | Ok mats ->
        List.fold mats ~init:literal ~f:(fun acc m -> (
          let is_string = starts_with m "[\"" || starts_with m "['" in
          let pos = if is_string then 2 else 1 in
          let dot_notation = "." ^ String.sub m ~pos:pos ~len:(String.length m - (pos * 2)) in
          String.substr_replace_first acc ~pattern:m ~with_:dot_notation
        ))
      | Error _ -> literal in

    let pieces = String.split wo_bg ~on:'.' in
    Some (LexValue (LexId pieces), remove_prefix text literal)
  else
    None

let lex_token text =
  let lexers = [lex_keyword; lex_range; lex_bool; lex_string; lex_number; lex_id] in
  first_successful text lexers


let lex_block_tokens text =
  let folder acc index =
    let curr = String.sub text ~pos:index ~len:(String.length text - index) in
    if Preprocessor.is_raw curr then
      let raw_text = Preprocessor.until_end_raw curr in
      let raw_body = Preprocessor.trim_raw_tags raw_text in
      Next (acc @ [RawText raw_body], index+(String.length raw_text))
    else
      if index + 2 < (String.length text) then
        let chunk = String.sub text ~pos:index ~len:2 in
        match chunk with
        | c when is_block_token_string c ->
          Next (acc @ [block_token_of_string chunk], index+(String.length c))
        | other -> (
          match acc |> List.rev with
          | RawText("liquid") :: StatementStart :: hds ->
            Next ((List.rev hds) @ [LiquidStart], index+1)
          | RawText(tl) :: hds ->
            Next (List.rev hds @ [RawText(tl ^ first_letter other)], index+1)
          | _ ->
            Next (acc @ [RawText(first_letter other)], index+1)
        )
      else
        Stop(acc)
  in

  unfold [] 0 folder

let lex_line_tokens text =
  let t_text = text ^ " " in
  let folder acc index =
    let chunk = String.sub t_text ~pos:index ~len:(String.length t_text - index) in
    match lex_token chunk with
    | Some(Newline, rest) -> Next (acc @ [EOS; Newline], String.length t_text - String.length rest)
    | Some(t, rest) -> Next (acc @ [t], String.length t_text - String.length rest)
    | _ -> Stop (acc)
  in

  let raw_list = unfold [] 0 folder in
  let wo_spaces = List.filter raw_list ~f:(fun token -> token != Space) in

  wo_spaces

let echo_to_expression tokens =
  let rec aux acc pool =
    match pool with
    | LexValue (LexId ["echo"]) :: LexValue (LexString t) :: tl -> aux (acc @ [LexExpression [LexText t]]) tl
    | LexValue (LexId ["echo"]) :: LexValue (LexId id) :: tl -> aux (acc @ [LexExpression [LexValue (LexId id)]]) tl
    | hd :: tl -> aux (acc @ [hd]) tl
    | [] -> acc
  in aux [] tokens


let lex_all_tokens (block_tokens: block_token list) =
  let folder acc index =

    let max = List.length block_tokens in
    if index > max then
      Stop (acc)
    else begin
      let sub = List.sub block_tokens ~pos:index ~len:(max - index) in
      match sub with
      | StatementStart :: RawText(body) :: StatementEnd :: _ ->
        Next (acc @ lex_line_tokens body @ [EOS], index+3)
      | ExpressionStart :: RawText(body) :: ExpressionEnd :: _ ->
        Next (acc @ [LexExpression (lex_line_tokens body)], index+3)
      | LiquidStart :: RawText(body) :: StatementEnd :: _ ->
        let liq = body |> Preprocessor.remove_liquid_comments |> lex_line_tokens in
        Next (acc @ liq, index+3)
      | RawText (other) :: _ ->
        Next (acc @ [Newline; LexText (other)], index + 1)
      | _ -> Stop (acc)
    end
  in

  let base_lex = unfold [] 0 folder in
  base_lex |> echo_to_expression


let lex text =
  text
  |> lex_block_tokens
  |> lex_all_tokens