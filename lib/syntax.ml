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
  ; cols: value
  ; is_tablerow: bool
  }

type variable_context =
  { variable: value
  ; value: value
  }

type id = string list

type ast =
  | Capture of id * ast
  | Block of ast list
  | Test of condition * ast * (ast option)
  | For of id * value * for_params * ast * (ast option)
  | Cycle of string option * string list
  | Expression of expression
  | Assignment of id * expression
  | Text of string
  | InProgress of Keyword.lex_token list
  | Break
  | Continue
  | Layout of string option
  | Include of string
  | Section of string
  | Render of string * variable_context list * ast option
  | Paginate of id * int * ast
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

let for_params_default =
  { limit = Number 50.
  ; offset = Number 0.
  ; reved = Bool false
  ; cols = Number 10.
  ; is_tablerow = false
  }

let context_var id =
  {variable = Var id; value = Var id }