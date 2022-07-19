open Base
open Syntax
open Tools
open Values

let fi = Float.to_int
let identity _ params = List.hd_exn params

let err t = Failure ("Liquid Error: " ^ t)
let errc t c = Failure (
  Core.sprintf "Liquid Error: %s, Params: %s" t
  (List.map c ~f:Debug.value_as_string |> join_by_comma)
)

type whitespace_remover = Beginning | End | Both
let remove_whitespace remover text =
  let exp = match remover with
    | Beginning -> ~/"^\\s+"
    | End -> ~/"\\s+$"
    | Both -> ~/"^\\s+|\\s+$" in

  Re2.rewrite_exn exp ~template:"" text

let pick_at_by_op op ctx params =
  match unwrap_all ctx params with
  | Number a :: Number b :: _ -> Number (if op a b then a else b)
  | other -> raise (errc "at_most/at_least accepts 2 numbers" other)


(* NOTE: For incr, decr to work, it must tolerate nil value. Maybe create incr_op func? *)
let apply_op op ctx params =
  match unwrap_all ctx params with
  | Nil :: Number b :: _ -> Number (op 0. b)
  | Number a :: Number b :: _ ->
    Number (op a b)
  | other -> raise (errc "operator accepts 2 numbers" other)
