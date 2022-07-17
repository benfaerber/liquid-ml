open Base
open Tools
open Syntax


let id_eq a b =
  List.equal (fun x y -> x = y) a b

let rec context_get ctx id =
  match ctx with
  | (hd_id, hd_val) :: _ when id_eq hd_id id -> hd_val
  | _ :: tl -> context_get tl id
  | _ -> Nil

let rec eq ctx va vb  =
  match (va, vb) with
  | Bool (a), Bool (b) -> a = b
  | String (a), String (b) -> a = b
  | Number (a), Number (b) -> a = b
  | List (a), List (b) -> List.equal (fun x y -> eq ctx x y) a b
  | Var (a), b -> eq ctx (context_get ctx a) b
  | a, Var (b) -> eq ctx a (context_get ctx b)
  | _ -> false

let rec gt ctx va vb  =
  match (va, vb) with
  | Bool (a), Bool (b) -> a > b
  | String (a), String (b) -> a > b
  | Number (a), Number (b) -> a > b
  | List (a), List (b) -> List.equal (fun x y -> gt ctx x y) a b
  | Var (a), b -> gt ctx (context_get ctx a) b
  | a, Var (b) -> gt ctx a (context_get ctx b)
  | _ -> false

let rec lt ctx va vb  =
  match (va, vb) with
  | Bool (a), Bool (b) -> a < b
  | String (a), String (b) -> a < b
  | Number (a), Number (b) -> a < b
  | List (a), List (b) -> List.equal (fun x y -> lt ctx x y) a b
  | Var (a), b -> lt ctx (context_get ctx a) b
  | a, Var (b) -> lt ctx a (context_get ctx b)
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
  | Var a, b -> contains ctx (context_get ctx a) b
  | a, Var b -> contains ctx a (context_get ctx b)
  | _ -> false

let lte ctx a b = lt ctx a b || eq ctx a b
let gte ctx a b = gt ctx a b || eq ctx a b
let ne ctx a b = if eq ctx a b then false else true

let rec string_from_value ctx = function
  | Bool(b) -> (if b then "True" else "False")
  | String(s) -> s
  | Number(f) -> Core.sprintf "%f" f
  | Var(v) -> string_from_value ctx (context_get ctx v)
  | Nil -> ""
  | _ -> "Unknown"
