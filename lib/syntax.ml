type value =
  | Bool of bool
  | String of string
  | Number of float
  | Var of string list
  | List of value list
  | Nil
  | Skip

type syntax_token =
  | Operator of Keyword.operator
  | Value of value

type expression =
  | Value of value
  | Func of string * expression list

type operator_equation = value * Keyword.operator * value

type combiner = And | Or
type condition =
  | Combine of combiner * condition list
  | Equation of operator_equation
  | Not of condition
  | AlwaysTrue

type for_params =
  { limit: value
  ; offset: value
  ; reved: value
  }

type ast =
  | Capture of string * ast
  | Block of ast list
  | Test of condition * ast * (ast option)
  | For of string * value * for_params * ast * (ast option)
  | Expression of expression
  | Assignment of string * expression
  | Text of string
  | InProgress of Keyword.lex_token list
  | Break
  | Continue
  | Nothing

let list_of_range = function
  | Keyword.LexRange (start, stop) -> Batteries.(--) start stop |> Batteries.List.of_enum
  | _ -> raise (Failure "This is not a range!")

let liq_list_of_range r = List (
  List.map (fun n -> Number (Int.to_float n)) (list_of_range r)
)

let lex_value_to_value = function
  | Keyword.LexId (id) -> Var (id)
  | LexBool (b) -> Bool (b)
  | LexString (s) -> String (s)
  | LexNumber n -> Number n
  | LexRange (st, sp) -> liq_list_of_range (LexRange (st, sp))
  | LexNil | LexBlank -> Nil