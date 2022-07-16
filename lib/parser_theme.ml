open Base
open Tools
open Keyword
open Syntax
open Parser_tools

let parse_render = function
  | LexValue (LexString filename) :: tokens -> (
    match tokens with
    | EOS :: tl ->
      Some (Render (filename, [], None), tl)
    | For :: LexValue (list) :: LexAs :: LexValue (LexId var) :: EOS :: tl ->
      let render = Render (filename, [context_var var], None) in
      let iter_list = lex_value_to_value list in
      let iter = For (var, iter_list, for_params_default, Block [render], None) in
      Some (iter, tl)
    | LexWith :: tl
    | Comma :: tl -> (
      match parse_variable_context tl with
      | Some (got, rest) -> Some (Render (filename, got, None), rest)
      | _ -> None
    )
    | _ -> None
  )
  | _ -> None

let parse_form block_parser all_tokens =
  match all_tokens with
  | LexValue (LexString filename) :: tokens -> (
    let ts = [LexForm] @ tokens in
    let bounds = Bounds.find_bounds ts 0 in
    let stop_point = Bounds.stop_point_from_bounds bounds in
    let body = List.sub tokens ~pos:0 ~len:stop_point in
    let pbody = Some (block_parser body) in
    let rest = sub_list_suffix ts stop_point in

    let formname = "form_" ^ filename in
    match tokens with
    | EOS :: _ ->
      Some  (Render (formname, [], pbody), rest)
    | Comma :: LexValue (LexId id) :: EOS :: _ ->
      Some  (Render (formname, [context_var id], pbody), rest)
    | Comma :: LexValue (LexId id) :: Comma :: tl -> (
      match parse_variable_context tl with
      | Some (ctx, _) ->
        Some (Render (formname, [context_var id] @ ctx, pbody), rest)
      | _ -> None
    )
    | Comma :: tl -> (
      match parse_variable_context tl with
      | Some (ctx, _) ->
        Some (Render (formname, ctx, pbody), rest)
      | _ -> None
    )
    | _ -> None
  )
  | _ -> None

let parse_style block_parser = function
  | EOS :: tl ->
    let tokens = [LexStyle] @ tl in
    let bounds = Bounds.find_bounds tokens 0 in
    let stop_point = Bounds.stop_point_from_bounds bounds in
    let body = List.sub tl ~pos:0 ~len:stop_point in
    let rest = sub_list_suffix tokens stop_point in

    let render = Render ("style_tag", [], Some (block_parser body)) in
    Some (render, rest)
  | _ -> None


let parse_theme block_parser = function
  | LexLayout :: LexValue (LexString s) :: tl -> Some (Layout (Some s), tl)
  | LexLayout :: LexNone :: tl -> Some(Layout (None), tl)
  | LexSection :: LexValue (LexString s) :: tl -> Some (Section s, tl)
  | LexInclude :: LexValue (LexString s) :: tl -> Some (Include s, tl)
  | LexRender :: tl -> parse_render tl
  | LexForm :: tl -> parse_form block_parser tl
  | LexStyle :: tl -> parse_style block_parser tl
  | _ -> None
