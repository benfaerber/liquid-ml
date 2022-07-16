open Keyword
open Syntax

let scan_until_eos tokens =
  let rec aux acc = function
    | hd :: tl when hd = Keyword.EOS -> acc, tl
    | hd :: tl -> aux (acc @ [hd]) tl
    | [] -> acc, []
  in aux [] tokens

let parse_variable_context tokens =
  let rec aux acc = function
    | LexValue (LexId id) :: Colon :: LexValue (v) :: Comma :: tl
    | LexValue (LexId id) :: LexAs :: LexValue (v) :: Comma :: tl ->
      let var_cxt = [{ variable = Var id; value = lex_value_to_value v }] in
      aux (acc @ var_cxt) tl
    | LexValue (LexId id) :: Colon :: LexValue (v) :: tl
    | LexValue (LexId id) :: LexAs :: LexValue (v) :: tl ->
      let var_cxt = [{ variable = Var id; value = lex_value_to_value v }] in
      acc @ var_cxt, tl
    | _ -> [], []
  in

  let (got, rest) = aux [] tokens in
  if got = [] then None else Some (got, rest)
