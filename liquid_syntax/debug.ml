open Base

open Syntax
open Tools

let dump x =
  x |> Batteries.dump |> Stdio.print_endline

let remove_nl text =
  let exp = Re2.create_exn "\n" in
  Re2.rewrite_exn exp ~template:"" text

let add_br text =
  let exp = Re2.create_exn "<br>" in
  Re2.rewrite_exn exp ~template:"\n" text

let newline_as_token = true

let block_tokens_as_string bts = join_by_space (List.map bts ~f:show_block_token)
let print_block_tokens bts = bts |> block_tokens_as_string |> Stdio.print_endline

let lex_tokens_as_string ts = join_by_space (List.map ts ~f:show_lex_token)
let print_lex_tokens ts = ts |> lex_tokens_as_string |> Stdio.print_endline

let lex_tokens_as_string_with_index ts =
  join_by_space (List.mapi ts ~f:(
    fun i t -> (i |> Int.to_string) ^ ": " ^ show_lex_token t
  ))
let print_lex_tokens_with_index ts = ts |> lex_tokens_as_string_with_index |> Stdio.print_endline

let id_as_string = join_by_arrow

and object_as_string obj =
  let seq = Syntax.Object.to_seq obj in
  let mapped = Stdlib.Seq.map (fun (id, v) -> Core.sprintf "%s=%s\n" id (show_value v |> remove_nl |> add_br)) seq in
  let built = Stdlib.Seq.fold_left (fun acc curr -> acc ^ curr  ^ ", ") "" mapped in
  if String.length built > 2 then
    remove_suffix built ", "
  else built


let variable_context_as_string m =
  let seq = Syntax.Ctx.to_seq m in
  let mapped = Stdlib.Seq.map (fun (id, v) -> Core.sprintf "%s=%s\n" id (show_value v |> remove_nl |> add_br)) seq in
  let built = Stdlib.Seq.fold_left (fun acc curr -> acc ^ ", " ^ curr) "" mapped in
  built

let print_variable_context m = m |> variable_context_as_string |> Stdio.print_endline


let rec condition_as_string =
  let rec aux = function
  | Equation (a, op, b) -> (show_value a) ^ " " ^ (show_operator op) ^ " " ^ (show_value b)
  | Always b -> Core.sprintf "Always (%b)" b
  | IsTruthy v -> "IsTruthy(" ^ (show_value v) ^ ")"
  | Not x -> "Not(\n" ^ (condition_as_string x) ^ "\n)"
  | Combine (c, l, r) ->
    Core.sprintf "%s(\n%s, %s\n)" (show_combiner c) (aux l) (aux r)
  in aux

let print_condition c = c |> condition_as_string |> Stdio.print_endline

let rec expression_as_string = function
  | Value v -> show_value v
  | Func (n, e) -> "f:" ^ n ^ "(\n  " ^ (join_by_space (List.map e ~f:expression_as_string)) ^ "\n)"

let print_expression e = e |> expression_as_string |> Stdio.print_endline

let tab l =
  List.map (range l) ~f:(fun _ -> "  ") |> join

let show_ast = show_ast;;

let show_in_progress = false

let print_ast ast = ast |> show_ast |> Stdio.print_endline

let print_line () = Stdio.print_endline "----------------------------------------------------------"

let parse_result_as_string = function
  | Some (ast, _) -> Core.sprintf "Ast:\n%s" (show_ast ast)
  | None -> Core.sprintf "Parse Result None"

let parse_result_with_rest_as_string = function
  | Some (ast, tokens) -> Core.sprintf "Ast:\n%s\nRest:\n%s" (show_ast ast) (lex_tokens_as_string tokens)
  | None -> Core.sprintf "Parse Result None"

let print_parse_result pr = pr |> parse_result_as_string |> Stdio.print_endline
let print_parse_result_with_rest pr = pr |> parse_result_with_rest_as_string |> Stdio.print_endline

let remove_double_nl text =
  let exp = Re2.create_exn "\n\n" in
  Re2.rewrite_exn exp ~template:"" text

let print_rendered r =
  r |> remove_double_nl |> Stdio.print_endline
