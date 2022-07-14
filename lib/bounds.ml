open Base
open Keyword
open Tools

type bound_finders =
  { start: lex_token
  ; stop: lex_token
  ; other: lex_token list option
  }

let pair_to_bounds start stop = Some { start = start; stop = stop; other = None }

(* TODO: Else Keyword used as base case could break nested if else chain *)
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
  | For -> Some
    { start = For
    ; stop = EndFor
    ; other = Some [Else]
    }
  | Raw -> pair_to_bounds Raw EndRaw
  | Capture -> pair_to_bounds Capture EndCapture
  | Paginate -> pair_to_bounds Paginate EndPaginate
  | TableRow -> pair_to_bounds TableRow EndTableRow
  | _ -> None

let remove_last lst =
  match List.rev lst with
  | _ :: hds -> List.rev hds
  | [] -> []

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
    (* Stdio.printf " %d " index;
    Debug.print_lex_tokens_with_index tokens; *)
    let token = nth tokens index in
    match token with
    | _ when token = bounds.start ->
      let acc = (tally @ [index], found) in
      Next (acc, index+1)
    | _ when token = bounds.stop -> (
      let nt = remove_last tally in
      let nf =
        if List.hd_exn found = List.last_exn tally then found @ [index] else found in
      let acc = (nt, nf) in
      match nt with
      | [] -> Stop (acc)
      | _ -> Next (acc, index+1)
    )
    | _ when contains other_bounds token ->
      let nf =
        if first found = List.last_exn tally then found @ [index] else found in
      let acc = (tally, nf) in
      Next (acc, index+1)
    | _ ->
      let acc = (tally, found) in
      Next (acc, index+1)
  in

  let (_, found) = unfold ([start_point], [start_point]) (start_point+1) folder in
  found |> pair_up_bounds

let bounds_to_chunks tokens bounds =
  List.map bounds ~f:(fun (start_i, end_i) -> sub_list tokens start_i end_i)

let stop_point_from_bounds bounds =
  let (_, sp) = List.last_exn bounds in
  sp+1