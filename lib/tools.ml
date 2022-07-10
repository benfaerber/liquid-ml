open Base

let eq = Caml.(=)

let range x =
  Batteries.(--) 0 x |> Batteries.List.of_enum

let sub_prefix text x = String.sub text ~pos:0 ~len:x
let sub_suffix text x = String.sub text ~pos:x ~len:(String.length text)
let first_letter text = sub_prefix text 1

let remove_prefix text prefix = String.sub text ~pos:(String.length prefix) ~len:(String.length text - String.length prefix)

let starts_with text prefix =
  if String.length text > String.length prefix then (
    let rprefix = sub_prefix text (String.length prefix) in
    (Caml.(=)) rprefix prefix
  ) else false

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
let join lst = String.concat ~sep:"" lst