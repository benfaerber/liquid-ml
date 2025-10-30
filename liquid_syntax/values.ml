open Base
open Tools
open Syntax

let id_eq a b =
  List.equal (fun x y -> x = y) a b

let find ctx v =
  unwrap_or (Ctx.find_opt v ctx) Nil

let without_last id =
  id |> List.rev |> List.tl_exn |> List.rev

let is_index id =
  match List.last id with
  | Some hd ->
    let digit = Re2.create_exn "^\\d+" in
    Re2.matches digit hd
  | _ -> false

let rec unwrap ctx = function
  | Var id when is_calling ctx id "first" ->
    let lst = unwrap_list ctx id in
    unwrap_or (List.hd lst) Nil
  | Var id when is_calling ctx id "last" ->
    let lst = unwrap_list ctx id in
    unwrap_or (List.last lst) Nil
  | Var id when is_calling ctx id "size" ->
    let lst = unwrap_list ctx id in
    Number (List.length lst |> Int.to_float)
  | Var id when is_index id ->
    let index = id |> List.last_exn |> Int.of_string in
    let lst = unwrap_list ctx id in
    unwrap_or (List.nth lst index) Nil
  | Var [id] -> find ctx id
  | Var id -> unwrap_chain ctx id
  | other -> other
and unwrap_tail ctx v = Var (without_last v) |> unwrap ctx
and unwrap_list ctx id =
  match unwrap_tail ctx id with
  | List lst -> lst
  | _ -> Failure ("This operator can only be used on list") |> raise

and is_calling ctx id c =
  match List.last id with
  | Some last -> (
    match unwrap_tail ctx id with
    | List _ when last = c -> true
    | _ -> false
  )
  | _ -> false

and unwrap_chain ctx id =
  let folder (acc_val, acc_ctx) hd =
    match acc_val with
    | Nil -> Ctx.find hd acc_ctx, acc_ctx
    | Object obj -> (
      let nv = unwrap_or (Object.find_opt hd obj) Nil in
      nv, Ctx.empty |> Ctx.add hd nv
    )
    | v -> (
      let nctx = Ctx.empty |> Ctx.add Settings.next v in
      let nv = unwrap nctx (Var [Settings.next; hd]) in
      nv, nctx
    )
  in

  let (v, _) = List.fold id ~init:(Nil, ctx) ~f:folder in
  v


let rec string_from_value ctx = function
| Bool b -> (if b then "true" else "false")
| String s -> s
| Number f -> (
  if Float.round_down f = f then
    f |> Float.to_int |> Int.to_string
  else
    Float.to_string f
)
| Var id -> string_from_value ctx (unwrap ctx (Var id))
| Nil -> "nil"
| List lst -> List.map lst ~f:(string_from_value ctx) |> join_by_comma
| Object obj -> json_from_value ctx (Object obj)
| Date d -> Date.as_string d "%Y-%m-%d %H:%M"

and json_from_value ctx = function
| Object obj -> (
    let olst = obj_as_list obj in
    let kv_str (k, v) = Core.sprintf "\"%s\": %s" k (json_from_value ctx v) in
    let kv_pairs = List.map olst ~f:kv_str in
    let literal = String.concat ~sep:",\n" kv_pairs in
    "{\n" ^ literal ^ "\n}"
)
| List lst ->
  let inner = List.map lst ~f:(json_from_value ctx) in
  "[" ^ join_by_comma inner ^ "]"
| String s -> "\"" ^ s ^ "\""
| Date d -> "\"" ^ Date.as_iso_string d ^ "\""
| Nil -> "null"
| other -> string_from_value ctx other

let compare_value pa pb =
  match (pa, pb) with
  | Bool a, Bool b -> Bool.compare a b
  | Number a, Number b -> Float.compare a b
  | String a, String b -> String.compare a b
  | List a, List b -> Int.compare (List.length a) (List.length b)
  | Object _, Object _ -> 0
  | _, Nil -> -1
  | Nil, _ -> 1
  | _ -> 0

let unwrap_render_context ~outer_ctx ~render_ctx =
  let seq = Syntax.Ctx.to_seq render_ctx in
  let mapped = Stdlib.Seq.map (fun (id, v) -> id, unwrap outer_ctx v) seq in
  Ctx.of_seq mapped

let unwrap_float ctx v =
  match unwrap ctx v with
  | Number n -> n
  | _ -> raise (Failure "Failed to get number")

let unwrap_int ctx value = value |> unwrap_float ctx |> Float.to_int

let unwrap_bool ctx v =
  match unwrap ctx v with
  | Bool b -> b
  | _ -> raise (Failure "Failed to get bool")

let unwrap_string ctx v =
  match unwrap ctx v with
  | String s -> s
  | _ -> raise (Failure "Failed to get string")

let unwrap_object ctx v =
  match unwrap ctx v with
  | Object obj -> obj
  | _ -> raise (Failure "Failed to get object")

let unwrap_object_value_or obj id d =
  match obj |> Object.find_opt id with
  | Some v -> v
  | _ -> d

let is_truthy ctx v =
  match unwrap ctx v with
  | Bool false | Nil -> false
  | _ -> true

let is_nil ctx v =
  match unwrap ctx v with
  | Nil -> true
  | _ -> false

let is_not_nil ctx v = is_nil ctx v |> not

let unwrap_all ctx lst = List.map lst ~f:(unwrap ctx)

let list_passes op ctx a b = List.equal (fun x y -> op ctx x y) a b

let rec eq ctx va vb  =
  match (unwrap ctx va, unwrap ctx vb) with
  | Bool a, Bool b -> a = b
  | String a, String b -> a = b
  | Number a, Number b -> a = b
  | List a, List b -> list_passes eq ctx a b
  | _ -> false

let rec gt ctx va vb  =
  match (unwrap ctx va, unwrap ctx vb) with
  | Bool a, Bool b -> a > b
  | String a, String b -> a > b
  | Number a, Number b -> a > b
  | List a, List b -> list_passes gt ctx a b
  | _ -> false

let rec lt ctx va vb  =
  match (unwrap ctx va, unwrap ctx vb) with
  | Bool a, Bool b -> a < b
  | String a, String b -> a < b
  | Number a, Number b -> a < b
  | List a, List b -> list_passes lt ctx a b
  | _ -> false

let list_contains ctx lst item =
  List.mem lst item ~equal:(eq ctx)

let string_contains haystack needle =
  String.is_substring ~substring:needle haystack

let contains ctx va vb =
  match (unwrap ctx va, unwrap ctx vb) with
  | List a, b -> list_contains ctx a b
  | String a, String b -> string_contains a b
  | _ -> false

let lte ctx a b = lt ctx a b || eq ctx a b
let gte ctx a b = gt ctx a b || eq ctx a b
let ne ctx a b = eq ctx a b |> not

let num_int n = (Number (n |> Int.to_float))
