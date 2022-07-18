open Base
open Tools
open Syntax


let as_id s = [s]
let id_eq a b =
  List.equal (fun x y -> x = y) a b

let find ctx v =
  match Ctx.find_opt v ctx with
  | Some g -> g
  | _ -> Nil

let rec string_from_value ctx = function
  | Bool(b) -> (if b then "True" else "False")
  | String(s) -> s
  | Number(f) -> (
    if Float.round_down f = f then
      Core.sprintf "%d" (Float.to_int f)
    else
      Core.sprintf "%f" f)
  | Var(v) -> string_from_value ctx (find ctx v)
  | Nil -> "nil"
  | List lst -> List.map lst ~f:(string_from_value ctx) |> join_by_comma
  | _ -> "Unknown"


let make_object obj_name kv_pairs ctx =
  List.fold_left kv_pairs ~init:ctx ~f:(
    fun acc_ctx (k, v) -> Ctx.add [obj_name; k] v acc_ctx
  )


let is_calling id c =
  let last = id |> List.rev |> List.hd_exn in
  last = c

let without_last id =
  id |> List.rev |> List.tl_exn |> List.rev

let rec unwrap ctx = function
  | Var id when is_calling id "first" -> (
    match unwrap_tail ctx id with
    | List [] -> Nil
    | List lst -> List.hd_exn lst
    | _ -> raise (Failure "first can only be used on list")
  )
  | Var id when is_calling id "last" -> (
    match unwrap_tail ctx id with
    | List [] -> Nil
    | List lst -> lst |> List.rev |> List.hd_exn
    | _ -> raise (Failure "last can only be used on list")
  )
  | Var id when is_calling id "size" -> (
    match unwrap_tail ctx id with
    | List lst -> Number (List.length lst |> Int.to_float)
    | _ -> raise (Failure "first can only be used on list")
  )
  | Var id -> (
    match unwrap_obj ctx id with
    | Some v -> v
    | _ -> find ctx id
  )
  | other -> other
and unwrap_tail ctx v = Var (without_last v) |> unwrap ctx

and unwrap_obj ctx = function
  | hd_id :: tl_id -> (
    Stdio.print_endline hd_id;
    match Ctx.find_opt [hd_id] ctx with
    | Some (Object init_obj) -> (
      let folder acc id =
        match acc with
        | Object (obj) -> Stdio.print_endline id; Obj.find id obj
        | other -> other
      in

      Some (List.fold tl_id ~init:(Object init_obj) ~f:folder)
    )
    | _ -> None
  )
  | _ -> None



let unwrap_float ctx v =
  match unwrap ctx v with
  | Number n -> n
  | _ -> raise (Failure "Failed to get number")

let unwrap_int ctx value = value |> unwrap_float ctx |> Float.to_int

let unwrap_bool ctx v =
  match unwrap ctx v with
  | Bool b -> b
  | _ -> raise (Failure "Failed to get number")

let unwrap_string ctx v =
  match unwrap ctx v with
  | String s -> s
  | _ -> raise (Failure "Failed to get number")


let is_truthy ctx v =
  match unwrap ctx v with
  | Bool false | Nil -> false
  | _ -> true

let unwrap_all ctx lst = List.map lst ~f:(unwrap ctx)


let rec eq ctx va vb  =
  match (unwrap ctx va, unwrap ctx vb) with
  | Bool (a), Bool (b) -> a = b
  | String (a), String (b) -> a = b
  | Number (a), Number (b) -> a = b
  | List (a), List (b) -> List.equal (fun x y -> eq ctx x y) a b
  | _ -> false

let rec gt ctx va vb  =
  match (unwrap ctx va, unwrap ctx vb) with
  | Bool (a), Bool (b) -> a > b
  | String (a), String (b) -> a > b
  | Number (a), Number (b) -> a > b
  | List (a), List (b) -> List.equal (fun x y -> gt ctx x y) a b
  | _ -> false

let rec lt ctx va vb  =
  match (unwrap ctx va, unwrap ctx vb) with
  | Bool (a), Bool (b) -> a < b
  | String (a), String (b) -> a < b
  | Number (a), Number (b) -> a < b
  | List (a), List (b) -> List.equal (fun x y -> lt ctx x y) a b
  | _ -> false

let list_contains ctx lst item =
  List.mem lst item ~equal:(eq ctx)

let string_contains big little =
  let exp = Re2.create_exn little in
  Re2.matches exp big

let contains ctx va vb =
  match (unwrap ctx va, unwrap ctx vb) with
  | List a, b -> list_contains ctx a b
  | String a, String b -> string_contains a b
  | _ -> false

let lte ctx a b = lt ctx a b || eq ctx a b
let gte ctx a b = gt ctx a b || eq ctx a b
let ne ctx a b = if eq ctx a b then false else true
