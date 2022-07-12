open Base
open Keyword
open Syntax

let to_exp_value v = Value (lex_value_to_value v)
let to_exp_values lst = List.map lst ~f:to_exp_value

let parse_expression full_tokens =
  let rec aux tokens =
    let (prefix, tail) =
      match tokens with
      | LexValue v :: tl -> (to_exp_value v), tl
      | _ -> Value Skip, tokens in

    let make_func id params tl =
      [Func (id, [prefix] @ to_exp_values params)] @ aux tl in

    match tail with
    | [LexValue v] ->
      [Value (lex_value_to_value v)]
    | Pipe :: LexValue (LexId id) :: Colon :: LexValue p1 :: Comma :: LexValue p2 :: Comma :: LexValue p3 :: tl ->
      make_func id [p1; p2; p3] tl
    | Pipe :: LexValue (LexId id) :: Colon :: LexValue p1 :: Comma :: LexValue p2 :: tl ->
      make_func id [p1; p2] tl
    | Pipe :: LexValue (LexId id) :: Colon :: LexValue p1 :: tl ->
      make_func id [p1] tl
    | Pipe :: LexValue (LexId id) :: tl ->
      make_func id [] tl
    | _ -> []
    in

  let func_list = aux full_tokens in

  let remove_skip = function
    | Value Skip -> []
    | e -> [e] in

  let rec unfold_into_func = function
    | Func (id, (Value Skip :: other_params)) :: tl ->
      Func (id, (tl |> unfold_into_func |> remove_skip)  @ other_params)
    | Func (id, params) :: tl ->
      Func (id, (tl |> unfold_into_func |> remove_skip) @ params)
    | _ -> Value Skip
  in

  unfold_into_func func_list
