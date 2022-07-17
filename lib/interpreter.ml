open Base
open Syntax
open Tools

let notifier t = [(["notifier_" ^ t], String ("notifier_" ^ t))]
let has_notifier ctx t = Values.context_has ctx ["notifier_" ^ t]
(* CTX Funcname exps *)
let interpret_function _ _ params = List.hd_exn params

let rec interpret_expression ctx = function
  | Value v -> v
  | Func (name, exps) -> (
    let params = List.map exps ~f:(interpret_expression ctx) in
    interpret_function ctx name params
  )

let interpret_equation ctx eq =
  match eq with
  | a, Keyword.Eq, b -> Values.eq ctx a b
  | a, Gte, b -> Values.gte ctx a b
  | a, Gt, b -> Values.gt ctx a b
  | a, Lte, b -> Values.lte ctx a b
  | a, Lt, b -> Values.lt ctx a b
  | a, Ne, b -> Values.ne ctx a b
  | a, Contains, b -> Values.contains ctx a b

let not b = if b then false else true

let rec interpret_condition ctx cond =
  match cond with
  | AlwaysTrue -> true
  | Not inner -> not (interpret_condition ctx inner)
  | Equation eq -> interpret_equation ctx eq
  | Combine (And, l, r) -> interpret_condition ctx l && interpret_condition ctx r
  | Combine (Or, l, r) -> interpret_condition ctx l || interpret_condition ctx r

let num_int n = (Number (n |> Int.to_float))

let rec interpret ctx str ast =
  match ast with
  | Block cmds -> interpret_while ctx str cmds
  | Assignment (id, exp) -> ctx @ [(id, interpret_expression ctx exp)], str
  | Test (cond, body, else_body) -> interpret_test ctx str cond body else_body
  | For (id, value, params, body, else_body) -> interpret_for ctx str id value params body else_body
  | Text t -> ctx, str ^ t
  | Expression exp -> (
    let value = interpret_expression ctx exp in
    ctx, str ^ (Values.string_from_value ctx value)
  )
  | Break -> ctx @ (notifier "break"), str
  | Continue -> ctx @ (notifier "continue"), str
  | Capture (id, body) -> (
    let (_, rendered) = interpret ctx str body in
    ctx @ [(id, String rendered)], str
  )
  | _ -> ctx, str

and interpret_while ctx str cmds =
  match cmds with
  | [cmd] -> interpret ctx str cmd
  | hd :: tl ->
    let (nctx, nstr) = interpret ctx str hd in
    let has = has_notifier nctx in
    if has "break" then
      ctx @ (notifier "break"), str
    else if has "continue" then
      ctx @ (notifier "continue"), str
    else
      interpret_while nctx nstr tl
  | _ -> ctx, str

and interpret_else ctx str = function
  | Some eb -> interpret ctx str eb
  | None -> ctx, str

and interpret_test ctx str cond body else_body =
  if interpret_condition ctx cond then
    interpret ctx str body
  else
    interpret_else ctx str else_body

and interpret_for ctx str alias packed_iterable params body else_body =
  let iterable = Values.unwrap ctx packed_iterable in

  let loop (acc_ctx, acc_str) curr =
    (* TODO: Add forloop.parent *)
    let loop_ctx = acc_ctx @ [(alias, curr)] in
    match body with
    | Block b -> (
      let (inner_ctx, rendered) = interpret_while loop_ctx "" b in
      let res = (acc_ctx, acc_str ^ rendered) in
      if has_notifier inner_ctx "break" then
        Done res
      else
        Forward res
    )
    | _ -> Done (acc_ctx, acc_str)
  in

  match iterable with
  | List l -> (
    if List.length l != 0 then begin
      let len =
        if List.length l < (params.limit - params.offset) then
          List.length l - params.offset
        else
          params.limit - params.offset in

      let limited = List.sub l ~pos:(params.offset) ~len:len in
      let r = if params.reved then List.rev limited else limited in

      fold_until r (ctx, str) loop
    end else
      interpret_else ctx str else_body
  )
  | _ -> interpret_else ctx str else_body


let interpret_file filename =
  let raw_text =
    filename
    |> File.read
    |> Preprocessor.preprocess
  in

  let tokens = raw_text |> Lexer.lex_text in
  Debug.print_lex_tokens_with_index tokens;
  Debug.print_line ();
  let ast = tokens |> Parser.parse_block in

  Debug.print_ast ast;
  Debug.print_line();

  let test_list = List ([Number 1.; Number 2.; Number 3.; Number 4.]) in
  let default_ctx = [(["test_list"], test_list)] in
  let default_str = "" in

  let (final_ctx, final_str) = interpret default_ctx default_str ast in
  Debug.print_variable_context final_ctx;
  Stdio.print_endline "Render:";
  Debug.print_rendered final_str;
  ()

let test () =
  interpret_file "liquid/interpreter_test.liquid"