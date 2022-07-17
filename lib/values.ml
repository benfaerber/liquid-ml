open Base
open Tools
open Syntax


let as_id s = [s]
let id_eq a b =
  List.equal (fun x y -> x = y) a b

let find ctx v =
  Debug.dump v;
  Debug.print_variable_context ctx;
  Ctx.find v ctx

let rec eq ctx va vb  =
  match (va, vb) with
  | Bool (a), Bool (b) -> a = b
  | String (a), String (b) -> a = b
  | Number (a), Number (b) -> a = b
  | List (a), List (b) -> List.equal (fun x y -> eq ctx x y) a b
  | Var (a), b -> eq ctx (find ctx a) b
  | a, Var (b) -> eq ctx a (find ctx b)
  | _ -> false

let rec gt ctx va vb  =
  match (va, vb) with
  | Bool (a), Bool (b) -> a > b
  | String (a), String (b) -> a > b
  | Number (a), Number (b) -> a > b
  | List (a), List (b) -> List.equal (fun x y -> gt ctx x y) a b
  | Var (a), b -> gt ctx (Ctx.find a ctx) b
  | a, Var (b) -> gt ctx a (Ctx.find b ctx)
  | _ -> false

let rec lt ctx va vb  =
  match (va, vb) with
  | Bool (a), Bool (b) -> a < b
  | String (a), String (b) -> a < b
  | Number (a), Number (b) -> a < b
  | List (a), List (b) -> List.equal (fun x y -> lt ctx x y) a b
  | Var (a), b -> lt ctx (Ctx.find a ctx) b
  | a, Var (b) -> lt ctx a (Ctx.find b ctx)
  | _ -> false

let list_contains ctx lst item =
  List.mem lst item ~equal:(eq ctx)

let string_contains big little =
  let exp = Re2.create_exn little in
  Re2.matches exp big

let rec contains ctx va vb =
  match (va, vb) with
  | List a, b -> list_contains ctx a b
  | String a, String b -> string_contains a b
  | Var a, b -> contains ctx (Ctx.find a ctx) b
  | a, Var b -> contains ctx a (Ctx.find b ctx)
  | _ -> false

let lte ctx a b = lt ctx a b || eq ctx a b
let gte ctx a b = gt ctx a b || eq ctx a b
let ne ctx a b = if eq ctx a b then false else true

let rec string_from_value ctx = function
  | Bool(b) -> (if b then "True" else "False")
  | String(s) -> s
  | Number(f) -> (
    if Float.round_down f = f then
      Core.sprintf "%d" (Float.to_int f)
    else
      Core.sprintf "%f" f)
  | Var(v) -> string_from_value ctx (Ctx.find v ctx)
  | Nil -> "nil"
  | _ -> "Unknown"

let unwrap ctx = function
  | Var v -> (Ctx.find v ctx)
  | other -> other

let rec unwrap_float ctx = function
  | Var v -> unwrap_float ctx (Ctx.find v ctx)
  | Number n -> n
  | _ -> raise (Failure "Failed to get number")

let unwrap_int ctx value = value |> unwrap_float ctx |> Float.to_int

let rec unwrap_bool ctx = function
  | Var v -> unwrap_bool ctx (Ctx.find v ctx)
  | Bool b -> b
  | _ -> raise (Failure "Failed to get bool")
