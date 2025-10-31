open Base
include Tokens

module LiquidObject = struct
  type t = string

  let compare = String.compare
end

module Object = Stdlib.Map.Make (LiquidObject)

let obj_as_list obj =
  Object.to_seq obj |> Stdlib.Seq.fold_left (fun acc curr -> acc @ [ curr ]) []

module VariableContext = struct
  type t = string

  let compare = String.compare
end

module Ctx = Stdlib.Map.Make (VariableContext)

let idf id = String.split id ~on:'.'

type id = string list [@@deriving show]

type value =
  | Bool of bool
  | String of string
  | Number of float
  | Var of string list
  | List of value list
  | Date of Date.t
  | Object of liquid_object
  | Nil

and liquid_object = value Object.t

(* Custom printers for types that don't have automatic deriving *)
let pp_date fmt d =
  Stdlib.Format.fprintf fmt "Date(%s)" (Date.as_string d "%Y-%m-%d %H:%M")

let show_date d = Date.as_string d "%Y-%m-%d %H:%M"

let rec pp_liquid_object fmt obj =
  Stdlib.Format.fprintf fmt "Object{%s}"
    (Object.to_seq obj |> Stdlib.List.of_seq
    |> List.map ~f:(fun (k, v) -> k ^ "=" ^ show_value v)
    |> String.concat ~sep:", ")

and show_liquid_object obj =
  Object.to_seq obj |> Stdlib.List.of_seq
  |> List.map ~f:(fun (k, v) -> k ^ "=" ^ show_value v)
  |> String.concat ~sep:", "

and pp_value fmt = function
  | Bool b -> Stdlib.Format.fprintf fmt "Bool(%b)" b
  | String s -> Stdlib.Format.fprintf fmt "String(%s)" s
  | Number f -> Stdlib.Format.fprintf fmt "Number(%f)" f
  | Var v -> Stdlib.Format.fprintf fmt "Var(%s)" (String.concat ~sep:"." v)
  | List l ->
      Stdlib.Format.fprintf fmt "List[%s]"
        (List.map l ~f:show_value |> String.concat ~sep:", ")
  | Date d -> pp_date fmt d
  | Object obj -> pp_liquid_object fmt obj
  | Nil -> Stdlib.Format.fprintf fmt "Nil"

and show_value = function
  | Bool b -> "Bool(" ^ Bool.to_string b ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Number f -> "Number(" ^ Float.to_string f ^ ")"
  | Var v -> "Var(" ^ String.concat ~sep:"." v ^ ")"
  | List l ->
      "List[" ^ (List.map l ~f:show_value |> String.concat ~sep:", ") ^ "]"
  | Date d -> show_date d
  | Object obj -> "Object{" ^ show_liquid_object obj ^ "}"
  | Nil -> "Nil"

let pp_variable_context fmt ctx =
  Stdlib.Format.fprintf fmt "Ctx{%s}"
    (Ctx.to_seq ctx |> Stdlib.List.of_seq
    |> List.map ~f:(fun (k, v) -> k ^ "=" ^ show_value v)
    |> String.concat ~sep:", ")

let show_variable_context ctx =
  Ctx.to_seq ctx |> Stdlib.List.of_seq
  |> List.map ~f:(fun (k, v) -> k ^ "=" ^ show_value v)
  |> String.concat ~sep:", "

type variable_context = value Ctx.t

type expression = Value of value | Func of string * expression list
[@@deriving show]

let pp_operator_equation fmt (a, op, b) =
  Stdlib.Format.fprintf fmt "(%s %s %s)" (show_value a) (show_operator op)
    (show_value b)

let show_operator_equation (a, op, b) =
  "(" ^ show_value a ^ " " ^ show_operator op ^ " " ^ show_value b ^ ")"

type operator_equation = value * operator * value
type combiner = And | Or [@@deriving show]

type condition =
  | Combine of combiner * condition * condition
  | Equation of operator_equation
  | IsTruthy of value
  | Not of condition
  | Always of bool
[@@deriving show]

type for_params = {
    limit : int
  ; offset : int
  ; reved : bool
  ; cols : int
  ; is_tablerow : bool
}
[@@deriving show]

type ast =
  | Capture of string * ast
  | Block of ast list
  | Test of condition * ast * ast option
  | For of string * value * for_params * ast * ast option
  | Cycle of string option * string list
  | Expression of expression
  | Assignment of string * expression
  | Text of string
  | Break
  | Continue
  | Layout of string option
  | Include of string
  | Section of string
  | Render of string * variable_context * ast option
  | Paginate of id * int * ast
  | Nothing
[@@deriving show]

let list_of_range = function
  | LexRange (start, stop) ->
      Batteries.( -- ) start stop |> Batteries.List.of_enum
  | _ -> raise (Failure "This is not a range!")

let liq_list_of_range r =
  List (List.map (list_of_range r) ~f:(fun n -> Number (Int.to_float n)))

let lex_value_to_value = function
  | LexId id -> Var id
  | LexBool b -> Bool b
  | LexString s -> String s
  | LexNumber n -> Number n
  | LexRange (st, sp) -> liq_list_of_range (LexRange (st, sp))
  | LexNil -> Nil
  | LexBlank -> String ""

let for_params_default =
  { limit = 50; offset = 0; reved = false; cols = 10; is_tablerow = false }

type liquid_filter = variable_context -> value list -> (value, string) Result.t
type liquid_filter_lookup = string -> liquid_filter option

let liquid_int i = Number (Int.to_float i)
let liquid_float i = Number i
let pack_object obj = Object obj
