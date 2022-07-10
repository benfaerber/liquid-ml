open Base
open Keyword

let to_binary c = if c then 1 else 0

let open_to_close =
  function
  | Open p -> Close p
  | _ -> raise (Failure "This isn't an open tag")

let close_to_open =
  function
  | Close p -> Open p
  | _ -> raise (Failure "This isn't a close tag")


(* let find_closing *)