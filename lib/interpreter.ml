open Base
open Syntax
open Tools

let notifier t = Ctx.add ["notifier_" ^ t] (String ("notifier_" ^ t))
let has_notifier t = Ctx.mem ["notifier_" ^ t]
(* CTX Funcname exps *)
let interpret_function ctx name params =
  let func = Liquid_std.function_from_id name in
  func ctx params

let rec interpret_expression ctx = function
  | Value v -> Values.unwrap ctx v
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
  | IsTruthy v -> Values.is_truthy ctx v

let num_int n = (Number (n |> Int.to_float))

let make_forloop_ctx ctx index length =
  let fl = "forloop" in
  let add_fl k v = Ctx.add [fl; k] v in
  ctx
    |> add_fl "index" (num_int (index + 1))
    |> add_fl "length" (num_int length)
    |> add_fl "first" (Bool (index = 0))
    |> add_fl "index0" (num_int index)
    |> add_fl "last" (Bool (index = length - 1))
    |> add_fl "rindex" (num_int (length - index))
    |> add_fl "rindex0" (num_int (length - index - 1))

let rec interpret ctx str ast =
  match ast with
  | Block cmds -> interpret_while ctx str cmds
  | Assignment (id, exp) -> ctx |> Ctx.add id (interpret_expression ctx exp), str
  | Test (cond, body, else_body) -> interpret_test ctx str cond body else_body
  | For (id, value, params, body, else_body) -> interpret_for ctx str id value params body else_body
  | Cycle (group, values) -> interpret_cycle ctx str group values
  | Text t -> ctx, str ^ t
  | Expression exp -> (
    let value = interpret_expression ctx exp in
    ctx, str ^ (Values.string_from_value ctx value)
  )
  | Break -> notifier "break" ctx, str
  | Continue -> notifier "continue" ctx, str
  | Capture (id, body) -> (
    let (_, rendered) = interpret ctx "" body in
    Ctx.add id (String rendered) ctx, str
  )
  | _ -> ctx, str

and interpret_while ctx str cmds =
  match cmds with
  | [cmd] -> interpret ctx str cmd
  | hd :: tl ->
    let (nctx, nstr) = interpret ctx str hd in
    if has_notifier "break" nctx then
      notifier "break" ctx, str
    else if has_notifier "continue" nctx then
      notifier "continue" ctx, str
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
    (* TODO: Add forloop parent var *)
    let loop_ctx = Ctx.add alias curr acc_ctx in
    (* Debug.print_variable_context loop_ctx; *)
    match body with
    | Block b -> (
      let (inner_ctx, rendered) = interpret_while loop_ctx "" b in
      let r_str = acc_str ^ rendered in
      if has_notifier "break" inner_ctx then
        Done (ctx, r_str)
      else
        let find_int k = Ctx.find ["forloop"; k] acc_ctx |> Values.unwrap_int acc_ctx in
        let index = find_int "index" in
        let length = find_int "length" in
        let nacc = make_forloop_ctx acc_ctx index length in
        Forward (nacc, r_str)
    )
    | _ -> Done (ctx, acc_str)
  in

  match iterable with
  | List l when List.length l != 0 -> (
    let len = List.length l in
    let trim_len =
      if len < (params.limit - params.offset) then
        len - params.offset
      else
        params.limit - params.offset in

    let limited = List.sub l ~pos:params.offset ~len:trim_len in
    let r = if params.reved then List.rev limited else limited in

    let forlen = List.length r in
    let forloop_ctx = make_forloop_ctx ctx 0 forlen in
    let (_, r_str) = fold_until r (forloop_ctx, str) loop in
    ctx, r_str
  )
  | _ -> interpret_else ctx str else_body
and interpret_cycle ctx str _ values =
  let index = Ctx.find ["forloop"; "index"] ctx |> Values.unwrap_int ctx in
  let vlen = List.length values in
  let vindex = index % vlen in
  let curr = nth values vindex in

  ctx, str ^ curr

let does_log = false
let interpret_file filename =
  let raw_text =
    filename
    |> File.read
    |> Preprocessor.preprocess
  in

  let tokens = raw_text |> Lexer.lex_text in
  if does_log then Debug.print_lex_tokens_with_index tokens;
  if does_log then Debug.print_line ();
  let ast = tokens |> Parser.parse_block in

  if does_log then Debug.print_ast ast;
  if does_log then Debug.print_line();

  let _ = List ([Number 1.; Number 2.; Number 3.; Number 4.]) in
  let default_ctx = Ctx.empty in
  let default_str = "" in

  let (final_ctx, final_str) = interpret default_ctx default_str ast in
  if does_log then Debug.print_variable_context final_ctx;
  if does_log then Stdio.print_endline "Render:";
  if does_log then Debug.print_rendered final_str;
  ()

let test () =
(* interpret_file "liquid/interpreter_test.liquid" *)
  interpret_file "liquid/std_test.liquid"