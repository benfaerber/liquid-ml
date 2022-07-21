open Base
open Syntax
open Tools

let nlit t = "*notifier_" ^ t
let notifier t = Ctx.add (nlit t) (String (nlit t))
let has_notifier t = Ctx.mem (nlit t)


let ast_from_file filename =
  let filepath = Core.sprintf "liquid/%s.liquid" filename in
  let raw_text = File.read filepath in
  let ast =
    raw_text
    |> Preprocessor.preprocess
    |> Lexer.lex
    |> Parser.parse
  in
  ast

(* CTX Funcname exps *)
let interpret_function ctx name params =
  let std_lib_func = Liquid_std.function_from_id name in
  match std_lib_func ctx params with
  | Ok res -> res
  | Error err -> (
    match Settings_ctx.error_policy ctx with
    | Settings.Strict -> Invalid_argument err |> raise
    | Warn -> Stdio.print_endline err; Nil
    | Silent -> Nil
  )

let rec interpret_expression ctx = function
  | Value v -> Values.unwrap ctx v
  | Func (name, exps) -> (
    let params = List.map exps ~f:(interpret_expression ctx) in
    interpret_function ctx name params
  )

let interpret_equation ctx = function
  | a, Keyword.Eq, b -> Values.eq ctx a b
  | a, Gte, b -> Values.gte ctx a b
  | a, Gt, b -> Values.gt ctx a b
  | a, Lte, b -> Values.lte ctx a b
  | a, Lt, b -> Values.lt ctx a b
  | a, Ne, b -> Values.ne ctx a b
  | a, Contains, b -> Values.contains ctx a b

let rec interpret_condition ctx = function
  | Always b -> b
  | Not inner -> interpret_condition ctx inner |> not
  | Equation eq -> interpret_equation ctx eq
  | Combine (And, l, r) -> interpret_condition ctx l && interpret_condition ctx r
  | Combine (Or, l, r) -> interpret_condition ctx l || interpret_condition ctx r
  | IsTruthy v -> Values.is_truthy ctx v



let rec interpret ctx str = function
  | Block cmds -> interpret_block ctx str cmds
  | Assignment (id, exp) -> (
    if starts_with id Settings.increment then
      interpret_increment ctx str ~id ~exp
    else
      ctx |> Ctx.add id (interpret_expression ctx exp), str
  )
  | Test (cond, body, else_body) -> interpret_test ctx str ~cond ~body ~else_body
  | For (alias, iterable, params, body, else_body) -> interpret_for ctx str ~alias ~iterable ~params ~body ~else_body
  | Cycle (group, values) -> interpret_cycle ctx str ~group ~values
  | Render (filename, render_ctx, body) -> interpret_render ctx str ~filename ~render_ctx ~body
  | Include filename -> interpret_include ctx str ~filename
  | Section section_name -> (
    let section_path = "sections/" ^ section_name in
    interpret_render ctx str ~filename:section_path ~render_ctx:Ctx.empty ~body:None
  )
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
  | _ -> raise (Failure "NYI")

and interpret_block ctx str = function
  | [cmd] -> interpret ctx str cmd
  | hd :: tl ->
    let (nctx, nstr) = interpret ctx str hd in
    if has_notifier "break" nctx then
      notifier "break" ctx, str
    else if has_notifier "continue" nctx then
      notifier "continue" ctx, str
    else
      interpret_block nctx nstr tl
  | _ -> ctx, str

and interpret_else ctx str = function
  | Some eb -> interpret ctx str eb
  | None -> ctx, str

and interpret_test ctx str ~cond ~body ~else_body =
  let pre_state = save_state ctx in
  let (rctx, rstr) =
    if interpret_condition ctx cond then
      interpret ctx str body
    else
      interpret_else ctx str else_body
    in

  (rewind rctx pre_state, rstr)

and interpret_for ctx str ~alias ~iterable ~params ~body ~else_body =
  let uiter = Values.unwrap ctx iterable in
  let pre_state = save_state ctx in
  let loop (acc_ctx, acc_str) curr ~last =
    (* TODO: Add forloop parent var *)
    let loop_ctx = Ctx.add alias curr acc_ctx in
    match body with
    | Block b -> (
      let (inner_ctx, rendered) = interpret_block loop_ctx "" b in
      let r_str = acc_str ^ rendered in
      if has_notifier "break" inner_ctx then
        Done (rewind inner_ctx pre_state, r_str)
      else
        let find_int k =
          match Ctx.find Settings.forloop acc_ctx with
          | Object obj -> (
            match Obj.find_opt k obj with
            | Some (Number n) -> Float.to_int n
            | _ -> raise (Failure "Failed to find int")
          )
          | _ -> raise (Failure "Failed to find forloop object")
        in
        let index = find_int "index" in
        let length = find_int "length" in
        let nacc = Interpreter_objects.make_forloop_ctx inner_ctx index length in

        if last then Forward (rewind nacc pre_state, r_str)
        else Forward (nacc, r_str)
    )
    | _ -> raise (Failure "A body must be a block")
  in

  match uiter with
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
    let forloop_ctx = Interpreter_objects.make_forloop_ctx ctx 0 forlen in
    fold_until r (forloop_ctx, str) loop
  )
  | _ -> interpret_else ctx str else_body
and interpret_cycle ctx str ~group ~values =
  let gname = unwrap_or group "default" in
  let var_id = Core.sprintf "%s:%s" gname (join_by_underscore values) in
  let cycle = var_from Settings.cycle |> Values.unwrap_object ctx in
  let index =
    Values.unwrap_object_value_or cycle var_id (Number 0.)
    |> Values.unwrap_int ctx in

  let curr = nth values (index % List.length values) in
  let nindex = Values.num_int (index + 1) in
  let ncycle = cycle |> Obj.add var_id nindex in

  let nctx = ctx |> Ctx.add Settings.cycle (Object ncycle) in

  nctx, str ^ curr

and interpret_increment ctx str ~id ~exp =
  match exp with
  | Value (String modifier) -> (
    let var_id = String.split ~on:'.' id |> List.tl_exn |> join_by_underscore in
    let incr = var_from Settings.increment |> Values.unwrap_object ctx in
    let (def, offset) = if modifier = "plus" then (-1., 1) else (0., -1) in

    let ival =
      Values.unwrap_object_value_or incr var_id (Number def)
      |> Values.unwrap_int ctx in

    let nval = Values.num_int (ival + offset) in
    let nincr = incr |> Obj.add var_id nval in

    let nctx = ctx |> Ctx.add Settings.increment (Object nincr) in

    nctx, str ^ (Values.string_from_value ctx nval)
  )
  | _ -> raise (Failure "invalid increment")


and interpret_include ctx str ~filename =
  let ast = ast_from_file filename in
  interpret ctx str ast

and interpret_render ctx str ~filename ~render_ctx ~body =
  File.write "logs/body.txt" (Batteries.dump body);
  let ast = ast_from_file filename in
  let val_ctx = Values.unwrap_render_context ~outer_ctx:ctx ~render_ctx in
  let (_, rendered_text) = interpret val_ctx "" ast in
  ctx, str ^ rendered_text

let make_ctx (settings: Settings.t) =
  Ctx.empty
  |> Ctx.add Settings.cycle (Object Obj.empty)
  |> Ctx.add Settings.increment (Object Obj.empty)
  |> Ctx.add "collection" Test_data.test_collection
  |> Settings_ctx.add settings

let start settings ast =
  let ctx = make_ctx settings in
  let (_, text) = interpret ctx "" ast in
  text