open Base
open Keyword
open Syntax
open Parser_tools

let to_exp_value v = Value (lex_value_to_value v)
let to_exp_values lst = List.map lst ~f:to_exp_value

let expression_from_tokens full_tokens =
  let rec aux tokens =
    let (prefix, tail) =
      match tokens with
      | LexValue v :: tl -> (to_exp_value v), tl
      | _ -> Value Skip, tokens in

    let make_func id params tl =
      [Func (List.hd_exn id, [prefix] @ to_exp_values params)] @ aux tl in

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

  match full_tokens with
  | [LexValue v] -> Value (lex_value_to_value v)
  | _ -> (
    let func_list = aux full_tokens in

    let remove_skip = function
      | Value Skip -> []
      | e -> [e] in

    let rec unfold_into_func = function
      | Func (id, (Value Skip :: other_params)) :: tl ->
        Func (id, (tl |> unfold_into_func |> remove_skip) @ other_params)
      | Func (id, params) :: tl ->
        Func (id, (tl |> unfold_into_func |> remove_skip) @ params)
      | _ -> Value Skip
    in

    unfold_into_func (List.rev func_list)
  )

let parse_expression _ = function
  | LexExpression exp :: tl -> Some (Expression (expression_from_tokens exp), tl)
  | _ -> None

let parse_assignment _ tokens =
  let add_increment id modifier =
    let inc_id = ["increment"] @ id in
    Block [
      Assignment (
        inc_id,
        Func (modifier, [Value (Var inc_id); Value (Number 1.)])
      );
      Expression (Value (Var inc_id))
    ]
  in

  match tokens with
  | Assign :: LexValue (LexId id) :: Equals :: assign_tl ->
    let (raw_exp, tl) = scan_until_eos assign_tl in
    let exp = expression_from_tokens raw_exp in
    Some (Assignment (id, exp), tl)
  | Increment :: LexValue (LexId id) :: tl ->
    Some (add_increment id "plus", tl)
  | Decrement :: LexValue (LexId id) :: tl ->
    Some (add_increment id "minus", tl)
  | _ -> None
