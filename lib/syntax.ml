type value =
  | Bool of bool
  | String of string
  | Number of float
  | Var of string
  | List of value list
  | Nil

type syntax_token =
  | Operator of Keyword.operator
  | Value of value

type expression =
  | Value of value
  | Func of string * expression list

type condition = value * Keyword.operator * value
type loop = string * value list

type ast =
  | Capture of string * ast
  | Test of condition option * ast * (ast option)
  | For of loop
  | Expression of expression
  | Assignment of string * expression
  | InProgress of Keyword.lex_token list

let list_of_range = function
  | Keyword.Range (start, stop) -> Batteries.(--) start stop |> Batteries.List.of_enum
  | _ -> raise (Failure "This is not a range!")

let liq_list_of_range r = List (
  List.map (fun n -> Number (Int.to_float n)) (list_of_range r)
)