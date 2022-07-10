open Keyword
open Tools

type bound_finders = {
  start: lex_token;
  stop: lex_token;
  other: lex_token list option;
}

let if_bounds = {
  start = If;
  stop = EndIf;
  other = Some [Else; ElseIf];
}

let case_bounds = {
  start = Case;
  stop = EndCase;
  other = Some [When; Else];
}

let pair_to_bounds start stop = { start = start; stop = stop; other = None }
let unless_bounds = pair_to_bounds Unless EndUnless
let capture_bounds = pair_to_bounds Capture EndCapture
let paginate_bounds = pair_to_bounds Paginate EndPaginate
let tablerow_bounds = pair_to_bounds TableRow EndTableRow

let remove_last lst =
  match List.rev lst with
  | _ :: hds -> List.rev hds
  | [] -> []

let rec last = function
  | [] -> raise (Failure "Empty array!")
  | [x] -> x
  | _ :: tl -> last tl

let first = function
  | [] -> raise (Failure "Empty array!")
  | hd :: _ -> hd

let find_bounds tokens bounds start_point =
  let other_bounds = match bounds.other with Some x -> x | None -> [] in
  let folder (tally, found) index =
    let token = nth tokens index in
    match token with
    | _ when token = bounds.start ->
      let acc = (tally @ [index], found) in
      Next (acc, index+1)
    | _ when token = bounds.stop -> (
      let nt = remove_last tally in
      let nf =
        if first found == last tally then found @ [index] else found in
      let acc = (nt, nf) in
      match nt with
      | [] -> Stop (acc)
      | _ -> Next (acc, index+1)
    )
    | _ when contains other_bounds token ->
      let nf =
        if first found == last tally then found @ [index] else found in
      let acc = (tally, nf) in
      Next (acc, index+1)
    | _ ->
      let acc = (tally, found) in
      Next (acc, index+1)
  in

  let (_, found) = unfold ([start_point], [start_point]) (start_point+1) folder in
  found


let test () =
  let tokens =
    "liquid/if_else_test.liquid"
    |> File.read
    |> Preprocessor.preprocess
    |> Lexer.lex_text in

  tokens |> Debug.lex_tokens_as_string_with_index |> Stdio.print_endline;

  let bounds = find_bounds tokens if_bounds 0 in

  bounds |> Batteries.dump |> Stdio.print_endline;

  Stdio.print_endline "";