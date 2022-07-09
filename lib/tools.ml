open Base

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

type ('acc, 'curr) loop =
  | Next of ('acc * 'curr)
  | Stop of 'acc

let rec unfold acc curr func =
  match func acc curr with
  | Next((nacc, ncurr)) -> unfold nacc ncurr func
  | Stop(acc) -> acc
