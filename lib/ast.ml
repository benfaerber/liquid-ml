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

let pack_if chunks =
  let test_cond = Some (Var "x", Eq, Number 4.) in
  let rec aux pool =
    match pool with
    | fs :: tl -> Some (Test (None, InProgress fs, aux tl))
    | [] -> None in

  match chunks with
  | fs :: tl -> (
    Test (test_cond, InProgress fs, aux tl)
  )
  | _ -> raise (Failure "No chunks")


let test () =
  let tokens =
    "liquid/if_else_test.liquid"
    |> File.read
    |> Preprocessor.preprocess
    |> Lexer.lex_text in

  tokens |> Debug.lex_tokens_as_string_with_index |> Stdio.print_endline;
  Debug.print_line ();

  let bounds = find_bounds tokens 2 in
  bounds |> Batteries.dump |> Stdio.print_endline;

  bounds
  |> bounds_to_chunks tokens
  |> List.map (fun t -> Debug.lex_tokens_as_string t)
  |> List.iter (Stdio.printf "ENTRY:\n%s------------------------\n\n");
  Debug.print_line ();
  (* bounds |> bounds_to_chunks tokens |> pack_if |> Debug.ast_as_string |> Stdio.print_endline; *)

  Stdio.print_endline "";