open Base
open Tools
open Keyword

type 'a tree =
  | Node of 'a tree

type ast = (block list) tree

let pairs = [
  (StatementStart, StatementEnd);
  (ExpressionStart, ExpressionEnd);
  (LiquidStart, StatementEnd);
]

let eq = Caml.(=)

let is_open_tag = function
 | StatementStart | ExpressionStart | LiquidStart -> true
 | _ -> false

let get_close open_tag =
  match List.find pairs ~f:(fun (op, _) -> Caml.(=) open_tag op) with
  | Some (_, cl) -> cl
  | None -> StatementEnd

let nth lst index =
  match List.nth lst index with
  | Some(x) -> x
  | None -> raise(Failure ("Failed to get item at index"))

let to_binary c = if c then 1 else 0

let find_closing (lst: blockToken list) open_index =
  let open_tag = nth lst open_index in
  let close_tag = get_close open_tag in

  let folder (a_open, a_close, _) index =
    let tag = nth lst index in

    let d_open = a_open + to_binary (eq open_tag tag) in
    let d_close = a_close + to_binary (eq close_tag tag) in

    let acc = (d_open, d_close, index) in
    if eq d_open d_close then
      Stop (acc)
    else
      Next (acc, index+1)
  in

  let (_, _, close_index) = unfold (1, 0, 0) (open_index+1) folder in
  close_index

(* let build_tree block_tokens =
    let folder acc index =
      let tag = nth block_tokens index in

      if is_open_tag tag then
        let close_index = find_closing block_tokens index in
        List.sub ~pos:index ~len:close_index
      else
        List.sub ~pos:index ~len:close_index
    in

    unfold [] 0 folder *)