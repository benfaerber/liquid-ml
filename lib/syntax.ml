type operator =
  Eq | Gte | Gt | Lte | Lt | Ne | Contains

type value =
  | Bool of bool
  | String of string
  | Number of float
  | Var of string
  | Nil

type syntax_token =
  | Operator of operator
  | Value of value

type expression =
  | Value of value
  | Func of string * expression list

type condition = value * operator * value
type for_loop = string * value list

type ast =
  | Capture of string * ast
  | Test of condition * ast * (ast option)
  | For of bool * ast
  | Comment of string
  | Expression of expression
  | Assignment of string * expression

(*
{{ animal | capitalize | remove : "h" | slice: 0, 2 }}

The first param is the thing the filter is applied to
A filter has a name and a list of params
Operators are converted to function

Func (slice) (
  Func (remove) (
    Func (capitilize) (
      Value ( Var ("animal") )
    )
    Value ( String ("h") )
  )
  Value ( Number (0) )
  Value ( Number (2) )
)

if x == 1

Func (eq) (
  Value ( Var ("x") )
  Value ( Number (1))
)

*)
