open Base

let eq = Caml.(=)
let (=) = Caml.(=)
let (!=) = Caml.(!=)
let (>) = Caml.(>)
let (<) = Caml.(<)

let range x =
  Batteries.(--) 0 x |> Batteries.List.of_enum

let sub_prefix text x = String.sub text ~pos:0 ~len:x
let sub_suffix text x = String.sub text ~pos:x ~len:(String.length text - x)
let sub_list lst start_i end_i = List.sub lst ~pos:start_i ~len:(end_i - start_i)
let sub_list_suffix lst x = List.sub lst ~pos:x ~len:(List.length lst - x)
let first_letter text = sub_prefix text 1


let starts_with text prefix =
  if String.length text > String.length prefix then (
    let rprefix = sub_prefix text (String.length prefix) in
    rprefix = prefix
  ) else false

let ends_with text suffix =
  if String.length text > String.length suffix then (
    let rsuffix = sub_suffix text (String.length suffix) in
    rsuffix = suffix
  ) else false

let remove_prefix text prefix =
  if starts_with text prefix then
    String.sub text ~pos:(String.length prefix) ~len:(String.length text - String.length prefix)
  else
    text

let remove_suffix text suffix =
  if ends_with text suffix then
    String.sub text ~pos:0 ~len:(String.length text - String.length suffix)
  else
    text

let remove_list_prefix lst prefix =
  List.sub lst ~pos:(List.length prefix) ~len:(List.length lst - List.length prefix)

type ('acc, 'curr) unfold_notifier =
  | Next of 'acc * 'curr
  | Stop of 'acc

let rec unfold acc curr func =
  match func acc curr with
  | Next (nacc, ncurr) -> unfold nacc ncurr func
  | Stop acc -> acc

let nth lst index =
  match List.nth lst index with
  | Some(x) -> x
  | None -> raise(Failure ("Failed to get item at index"))

let first lst = nth lst 0

let join_by_space lst = String.concat ~sep:" " lst
let join_by_comma lst = String.concat ~sep:", " lst
let join_by_arrow lst = String.concat ~sep:"->" lst

let join lst = String.concat ~sep:"" lst

let contains lst item =
  List.mem lst item ~equal:(Caml.(=))

let unwrap_or value fallback = match value with Some v -> v | None -> fallback
