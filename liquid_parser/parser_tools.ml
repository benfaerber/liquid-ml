open Base
open Liquid_syntax
open Keyword
open Syntax
open Tools

let scan_until_eos tokens =
  let rec aux acc = function
    | hd :: tl when hd = Keyword.EOS -> acc, tl
    | hd :: tl -> aux (acc @ [hd]) tl
    | [] -> acc, []
  in aux [] tokens

let parse_variable_context tokens =
  let add acc id v = acc |> Ctx.add (List.hd_exn id) v in
  let rec aux acc = function
    | LexValue (LexId id) :: Colon :: LexValue (v) :: Comma :: tl ->
      let render_ctx = add acc id (lex_value_to_value v) in
      aux render_ctx tl
    | LexValue (LexId id) :: LexAs :: LexValue (LexId alias) :: Comma :: tl ->
      let render_ctx = add acc alias (Var id) in
      aux render_ctx tl
    | LexValue (LexId id) :: Colon :: LexValue (v) :: tl ->
      let render_ctx = add acc id (lex_value_to_value v) in
      render_ctx, tl
    | LexValue (LexId id) :: LexAs :: LexValue (LexId alias) :: tl ->
      let render_ctx = add acc alias (Var id) in
      render_ctx, tl
    | _ -> acc, []
  in

  let (got, rest) = aux Ctx.empty tokens in
  if Ctx.is_empty got then None else Some (got, rest)

let parse_single_body tag all_tokens =
  let tokens = [tag] @ all_tokens in
  let bounds = Bounds.find_bounds tokens 0 in
  let stop_point = Bounds.stop_point_from_bounds bounds in
  let body = List.sub all_tokens ~pos:0 ~len:stop_point in
  let rest = sub_list_suffix tokens stop_point in

  (body, rest)