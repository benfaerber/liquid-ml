open Keyword
open Tools
open Syntax

type bound_finders =
  { start: lex_token
  ; stop: lex_token
  ; other: lex_token list option
  }

let pair_to_bounds start stop = Some { start = start; stop = stop; other = None }

let bounds_from_opener = function
  | If -> Some
    { start = If
    ; stop = EndIf
    ; other = Some [Else; ElseIf]
    }
  | Case -> Some
    { start = Case
    ; stop = EndCase
    ; other = Some [When; Else]
    }
  | Unless -> pair_to_bounds Unless EndUnless
  | Capture -> pair_to_bounds Capture EndCapture
  | Paginate -> pair_to_bounds Paginate EndPaginate
  | TableRow -> pair_to_bounds TableRow EndTableRow
  | _ -> None

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


let find_next_opener tokens =
  let (index, _) = tokens
  |> List.mapi (fun i x -> i, bounds_from_opener x)
  |> List.find (function _, Some _ -> true | _ -> false) in
  index

let is_opener token =
  match bounds_from_opener token with
  | Some _ -> true
  | None -> false

let pair_up_bounds bounds =
  let rec aux acc =
    function
    | ft :: sd :: td :: tl -> aux (acc @ [(ft, sd); (sd, td)]) (td :: tl)
    | ft :: sd :: tl -> aux (acc @ [(ft, sd)]) tl
    | [] | [_] -> acc
  in aux [] bounds

let find_bounds tokens start_point =
  let first_token = nth tokens start_point in
  let bounds =
    match bounds_from_opener first_token with
    | Some b -> b
    | None -> raise (Failure "This is not an opening tag") in

  let other_bounds = unwrap_or bounds.other [] in

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
  found |> pair_up_bounds

let bounds_to_chunks tokens =
  List.map (fun (start_i, end_i) -> sub_list tokens start_i end_i)

let scan_until_newline tokens =
  let rec aux acc = function
    | hd :: _ when hd = Newline -> acc
    | hd :: tl -> aux (acc @ [hd]) tl
    | [] -> acc
  in aux [] tokens

let build_if_statement chunk =
  let statement = scan_until_newline chunk in
  statement |> Debug.print_lex_tokens;
  (AlwaysTrue, InProgress [])


let build_if_chain chunks =

  let rec aux pool =
    match pool with
    | fs :: tl ->
      let (condition, body) = build_if_statement fs in
      Some (Test (condition, body, aux tl))
    | [] -> None
  in

  let chain = aux chunks in
  match chain with
  | Some c -> c
  | None -> raise (Failure "Failed to build if chain")


(* let build_condition_tree tokens =
  let folder acc pool =
    match pool with
    |  *)


let test () =
  (* let tokens =
    "liquid/if_else_test.liquid"
    |> File.read
    |> Preprocessor.preprocess
    |> Lexer.lex_text in

  tokens |> Debug.lex_tokens_as_string_with_index |> Stdio.print_endline;
  Debug.print_line ();

  let bounds = find_bounds tokens 2 in

  bounds
  |> bounds_to_chunks tokens
  |> List.map (fun t -> Debug.lex_tokens_as_string t)
  |> List.iter (Stdio.printf "ENTRY:\n%s------------------------\n\n");
  Debug.print_line ();
  bounds |> bounds_to_chunks tokens |> build_if_chain |> Debug.print_ast; *)

  "x == 99 and t > 3 or pet == \"dot\"" |> Lexer.lex_line_tokens |> Debug.print_lex_tokens;

  Stdio.print_endline "";