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
  | Range of value * value
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
  | Range (a, b) ->
      Stdlib.Format.fprintf fmt "Range(%s..%s)" (show_value a) (show_value b)
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
  | Range (a, b) -> "Range(" ^ show_value a ^ ".." ^ show_value b ^ ")"
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

(* Build the concrete integer list for a resolved range (inclusive). *)
let liq_list_from_bounds lo hi =
  let lst = if hi >= lo then List.range lo (hi + 1) else [] in
  List (List.map lst ~f:(fun n -> Number (Int.to_float n)))

let rec lex_value_to_value = function
  | LexId id -> Var id
  | LexBool b -> Bool b
  | LexString s -> String s
  | LexNumber n -> Number n
  (* A range's bounds are kept as values and resolved at interpret time, so
     that variable bounds like (1..n) work, not just integer literals. *)
  | LexRange (lo, hi) -> Range (lex_value_to_value lo, lex_value_to_value hi)
  | LexNil -> Nil
  | LexBlank -> String ""

(* Shopify's `for` has no implicit limit; only an explicit `limit:` caps it.
   We use a very large sentinel so `min(len, limit)` is effectively unbounded
   while avoiding any `limit - offset` overflow. *)
let for_params_default =
  { limit = Int.max_value / 2; offset = 0; reved = false; cols = 10; is_tablerow = false }

type liquid_filter = variable_context -> value list -> (value, string) Result.t
type liquid_filter_lookup = string -> liquid_filter option

let liquid_int i = Number (Int.to_float i)
let liquid_float i = Number i
let pack_object obj = Object obj
