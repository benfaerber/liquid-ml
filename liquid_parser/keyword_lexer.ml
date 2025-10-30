open Base
open Liquid_syntax
open Syntax
open Tools

let is_operator = function
  | "==" | ">=" | ">" | "<=" | "<" | "!=" | "<>" -> true
  | _ -> false

let lex_keyword text =
  let keywords =
    [
      ("elsif", ElseIf);
      ("else", Else);
      ("if", If);
      ("endif", EndIf);
      ("unless", Unless);
      ("endunless", EndUnless);
      ("case", Case);
      ("endcase", EndCase);
      ("raw", Raw);
      ("endraw", EndRaw);
      ("when", When);
      ("with", LexWith);
      ("form", LexForm);
      ("endform", LexEndForm);
      ("style", LexStyle);
      ("endstyle", LexEndStyle);
      ("for", LexFor);
      ("endfor", LexEndFor);
      ("capture", Capture);
      ("endcapture", EndCapture);
      ("break", LexBreak);
      ("continue", LexContinue);
      ("cycle", Cycle);
      ("tablerow", TableRow);
      ("endtablerow", EndTableRow);
      ("paginate", Paginate);
      ("endpaginate", EndPaginate);
      ("assign", Assign);
      ("increment", Increment);
      ("decrement", Decrement);
      ("include", LexInclude);
      ("layout", LexLayout);
      ("render", LexRender);
      ("section", LexSection);
      ("in", In);
      ("by", By);
      ("as", LexAs);
      ("==", Operator Eq);
      (">=", Operator Gte);
      (">", Operator Gt);
      ("<=", Operator Lte);
      ("<", Operator Lt);
      ("!=", Operator Ne);
      ("<>", Operator Ne);
      ("contains", Operator Contains);
      ("and", LexCombiner LexAnd);
      ("or", LexCombiner LexOr);
      (":", Colon);
      ("|", Pipe);
      ("=", Equals);
      (",", Comma);
      (" ", Space);
      ("\n", Newline);
      ("nil", LexValue LexNil);
      ("blank", LexValue LexBlank);
      ("none", LexNone);
    ]
  in

  if text = " " || text = "\n" then None
  else
    (* words need a space, newline or eos (example: aspen != Keyword(As) Id(Pen)) *)
    let finder (check_literal, _) =
      if String.length check_literal = 1 || is_operator check_literal then
        starts_with text check_literal
      else
        starts_with text (check_literal ^ " ")
        || starts_with text (check_literal ^ "\n")
        || text = check_literal ^ " "
    in

    let found_keyword = List.find keywords ~f:finder in
    match found_keyword with
    | Some (literal, token) ->
        let trimmed = remove_prefix text literal in
        Some (token, trimmed)
    | None -> None

let block_token_of_string = function
  | "{%" -> StatementStart White
  | "%}" -> StatementEnd White
  | "{{" -> ExpressionStart White
  | "}}" -> ExpressionEnd White
  | "{%-" -> StatementStart Trim
  | "-%}" -> StatementEnd Trim
  | "{{-" -> ExpressionStart Trim
  | "-}}" -> ExpressionEnd Trim
  | other -> RawText other

let is_block_token_string t =
  match block_token_of_string t with
  | StatementStart _ | StatementEnd _ | ExpressionStart _ | ExpressionEnd _ ->
      true
  | _ -> false

let is_block_token_whitespace_string t =
  String.is_substring t ~substring:"-" && is_block_token_string t
