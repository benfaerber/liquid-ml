open Base
open Tools
open Keyword

type grid_pointer = int * int

let pairs = [
  (StatementStart, StatementEnd);
  (ExpressionStart, ExpressionEnd);
  (LiquidStart, StatementEnd);
]

let is_open_tag = function
 | StatementStart | ExpressionStart | LiquidStart -> true
 | _ -> false

let get_close open_tag =
  match List.find pairs ~f:(fun (op, _) -> Caml.(=) open_tag op) with
  | Some (_, cl) -> cl
  | None -> StatementEnd


let to_binary c = if c then 1 else 0

let open_to_close =
  function
  | Open p -> Close p
  | _ -> raise (Failure "This isn't an open tag")

let close_to_open =
  function
  | Close p -> Open p
  | _ -> raise (Failure "This isn't a close tag")

let find_closing (lst: block list) (pointer: grid_pointer) =
  let (open_index, open_subindex) = pointer in
  let open_tag =
    match nth lst open_index with
    | Statement(tokens) | Expression(tokens) | Liquid(tokens) -> nth tokens open_subindex
    | _ -> raise (Failure "No open tag located here") in

  let close_tag = open_to_close open_tag in

  let folder acc index =
    Stdio.printf "i - %d\n" index;
    match nth lst index with
    | Statement (tokens) | Expression (tokens) | Liquid (tokens) -> (
      match List.findi tokens ~f:(fun _ t -> eq close_tag t) with
      | Some (subindex, _) -> Stop (index, subindex)
      | _ -> Next (acc, index+1)
    )
    | _ -> Next (acc, index+1)
  in

  unfold (0, 0) open_index folder