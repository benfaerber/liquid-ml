open Base
open Tools

module LiquidObject =
  struct
    type t = string
    let compare a b =
      match (String.length a, String.length b) with
      | (ta, tb) when ta > tb -> 1
      | (ta, tb) when ta < tb -> -1
      | _ -> if a = b then 0 else -1
  end

module Obj = Caml.Map.Make(LiquidObject)

let obj_as_list obj =
  Obj.to_seq obj
  |> Caml.Seq.fold_left (fun acc curr -> acc @ [curr]) []


module VariableContext =
  struct
    type t = string
    let compare a b =
      match (String.length a, String.length b) with
      | (ta, tb) when ta > tb -> 1
      | (ta, tb) when ta < tb -> -1
      | _ -> if a = b then 0 else -1
  end

module Ctx = Caml.Map.Make(VariableContext)


let idf id = String.split id ~on:'.'

type id = string list

type value =
  | Bool of bool
  | String of string
  | Number of float
  | Var of string list
  | List of value list
  | Date of Date.t
  | Object of value Obj.t
  | Nil

type expression =
  | Value of value
  | Func of string * expression list

type operator_equation = value * Keyword.operator * value

type combiner = And | Or
type condition =
  | Combine of combiner * condition * condition
  | Equation of operator_equation
  | IsTruthy of value
  | Not of condition
  | Always of bool

type for_params =
  { limit: int
  ; offset: int
  ; reved: bool
  ; cols: int
  ; is_tablerow: bool
  }

type ast =
  | Capture of string * ast
  | Block of ast list
  | Test of condition * ast * (ast option)
  | For of string * value * for_params * ast * (ast option)
  | Cycle of string option * string list
  | Expression of expression
  | Assignment of string * expression
  | Text of string
  | Break
  | Continue
  | Layout of string option
  | Include of string
  | Section of string
  | Render of string * value Ctx.t * ast option
  | Paginate of id * int * ast
  | Nothing

let list_of_range = function
  | Keyword.LexRange (start, stop) -> Batteries.(--) start stop |> Batteries.List.of_enum
  | _ -> raise (Failure "This is not a range!")

let liq_list_of_range r = List (
  List.map (list_of_range r) ~f:(fun n -> Number (Int.to_float n))
)

let lex_value_to_value = function
  | Keyword.LexId id -> Var id
  | LexBool b -> Bool b
  | LexString s -> String s
  | LexNumber n -> Number n
  | LexRange (st, sp) -> liq_list_of_range (LexRange (st, sp))
  | LexNil -> Nil
  | LexBlank -> String ""

let for_params_default =
  { limit = 50
  ; offset = 0
  ; reved = false
  ; cols = 10
  ; is_tablerow = false
  }