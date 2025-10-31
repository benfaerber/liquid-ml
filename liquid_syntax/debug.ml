open Base
open Syntax
open Tools

(* Use auto-generated show functions where possible *)
let ws_control_as_string = show_whitespace_control
let block_token_as_string = show_block_token
let operator_as_string = show_operator
let lex_value_as_string = show_lex_value
let lex_combiner_as_string = show_lex_combiner
let dump x = x |> Batteries.dump |> Stdio.print_endline

let remove_nl text =
  let exp = Re2.create_exn "\n" in
  Re2.rewrite_exn exp ~template:"" text

let add_br text =
  let exp = Re2.create_exn "<br>" in
  Re2.rewrite_exn exp ~template:"\n" text

let newline_as_token = true

(* Use auto-generated function *)
let lex_token_as_string = show_lex_token

let block_tokens_as_string bts =
  join_by_space (List.map bts ~f:block_token_as_string)

let print_block_tokens bts =
  bts |> block_tokens_as_string |> Stdio.print_endline

let lex_tokens_as_string ts = join_by_space (List.map ts ~f:lex_token_as_string)
let print_lex_tokens ts = ts |> lex_tokens_as_string |> Stdio.print_endline

let lex_tokens_as_string_with_index ts =
  join_by_space
    (List.mapi ts ~f:(fun i t ->
         (i |> Int.to_string) ^ ": " ^ lex_token_as_string t))

let print_lex_tokens_with_index ts =
  ts |> lex_tokens_as_string_with_index |> Stdio.print_endline

(* Use auto-generated functions *)
let combiner_as_string = show_combiner
let id_as_string = show_id
let value_as_string = show_value
let object_as_string = show_liquid_object

(* Use auto-generated functions *)
let variable_context_as_string = show_variable_context

let print_variable_context m =
  m |> variable_context_as_string |> Stdio.print_endline

let condition_as_string = show_condition
let print_condition c = c |> condition_as_string |> Stdio.print_endline
let expression_as_string = show_expression
let print_expression e = e |> expression_as_string |> Stdio.print_endline
let tab l = List.map (range l) ~f:(fun _ -> "  ") |> join
let show_in_progress = false

let ast_as_string =
  let rec aux depth a =
    let t = tab depth in
    let result =
      match a with
      | Test (condition, child, next_child_opt) ->
          let child_text =
            Core.sprintf "\nCondition( %s )\n{  %s\n}\n"
              (condition |> condition_as_string)
              (aux (depth + 1) child)
          in

          let next_child_text =
            match next_child_opt with
            | Some next_child ->
                Core.sprintf "{ %s }\n" (aux (depth + 1) next_child)
            | None -> ""
          in

          child_text ^ next_child_text
      | For (id, value, params, body, else_block) -> (
          let vars =
            Core.sprintf "(l=%d,o=%d,r=%b,c=%d)" params.limit params.offset
              params.reved params.cols
          in
          let ft = if params.is_tablerow then "TableRow" else "For" in
          match else_block with
          | Some eb ->
              Core.sprintf "%s(%s in %s %s)\n{  %s\n}\n{  %s}" ft id
                (value_as_string value) vars
                (aux (depth + 1) body)
                (aux (depth + 1) eb)
          | None ->
              Core.sprintf "%s(%s in %s %s)\n{  %s\n}\n" ft id
                (value_as_string value) vars
                (aux (depth + 1) body))
      | Expression exp -> "Exp(" ^ expression_as_string exp ^ ")"
      | Assignment (id, exp) ->
          Core.sprintf "Assign(%s: %s)" id (expression_as_string exp)
      | Text t -> if String.strip t = "" then "" else Core.sprintf "t(%s)" t
      | Capture (id, body) ->
          Core.sprintf "Capture(%s: %s)" id (aux (depth + 1) body)
      | Block items ->
          "Block(\n"
          ^ (List.map items ~f:(aux (depth + 1)) |> join_by_space)
          ^ ")"
      | Break -> "Break"
      | Continue -> "Continue"
      | Cycle (name, items) -> (
          match name with
          | Some n -> Core.sprintf "Cycle(%s) {%s}" n (join_by_comma items)
          | _ -> Core.sprintf "Cycle {%s}" (join_by_comma items))
      | Layout t -> "Layout(" ^ (match t with Some x -> x | _ -> "None") ^ ")"
      | Include t -> "Include(" ^ t ^ ")"
      | Section t -> "Section(" ^ t ^ ")"
      | Paginate (id, num, body) ->
          Core.sprintf "Paginate(%s by %d) { %s }" (id_as_string id) num
            (aux (depth + 1) body)
      | Render (n, ctx, bl) -> (
          Core.sprintf "Render(%s: [%s])" n (variable_context_as_string ctx)
          ^
          match bl with
          | Some b -> Core.sprintf "{%s}" (aux (depth + 1) b)
          | _ -> "")
      | _ -> "Other"
    in

    if String.strip result = "" then "" else "\n" ^ t ^ result
  in
  aux 0

let print_ast ast = ast |> ast_as_string |> Stdio.print_endline

let print_line () =
  Stdio.print_endline
    "----------------------------------------------------------"

let parse_result_as_string = function
  | Some (ast, _) -> Core.sprintf "Ast:\n%s" (ast_as_string ast)
  | None -> Core.sprintf "Parse Result None"

let parse_result_with_rest_as_string = function
  | Some (ast, tokens) ->
      Core.sprintf "Ast:\n%s\nRest:\n%s" (ast_as_string ast)
        (lex_tokens_as_string tokens)
  | None -> Core.sprintf "Parse Result None"

let print_parse_result pr = pr |> parse_result_as_string |> Stdio.print_endline

let print_parse_result_with_rest pr =
  pr |> parse_result_with_rest_as_string |> Stdio.print_endline

let remove_double_nl text =
  let exp = Re2.create_exn "\n\n" in
  Re2.rewrite_exn exp ~template:"" text

let print_rendered r = r |> remove_double_nl |> Stdio.print_endline
