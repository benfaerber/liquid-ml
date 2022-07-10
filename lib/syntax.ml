type operator =
  | Eq | Gte | Gt | Lte | Lt | Ne | Contains

type value =
  | Bool of bool
  | String of string
  | Number of float
  | Var of string
  | List of value list
  | Nil

type syntax_token =
  | Operator of operator
  | Value of value

type expression =
  | Value of value
  | Func of string * expression list

type condition = value * operator * value
type loop = string * value list

type ast =
  | Capture of string * ast
  | Test of condition * ast * (ast option)
  | For of loop
  | Expression of expression
  | Assignment of string * expression
