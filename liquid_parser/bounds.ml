open Base
open Liquid_syntax

open Syntax
open Tools

type bound_finders =
  { start: lex_token
  ; stop: lex_token
  ; other: lex_token list option
  ; conflicts: bound_finders list option
  }

let p_to_b start stop = { start = start; stop = stop; other = None; conflicts = None}
let pair_to_bounds start stop = Some (p_to_b start stop)

(* Several tags rely on the 'else' keyword. This list allows these tags to be tallied when needed *)
let else_conflicts =
  Some [p_to_b If EndIf; p_to_b Case EndCase; p_to_b Unless EndUnless; p_to_b LexFor LexEndFor]


let bounds_from_opener = function
  | If -> Some
    { start = If
    ; stop = EndIf
    ; other = Some [Else; ElseIf]
    ; conflicts = else_conflicts
    }
  | Case -> Some
    { start = Case
    ; stop = EndCase
    ; other = Some [When; Else]
    ; conflicts = else_conflicts
    }
  | Unless -> Some
    { start = Unless
    ; stop = EndUnless
    ; other = Some [Else]
    ; conflicts = else_conflicts
    }
  | LexFor -> Some
    { start = LexFor
    ; stop = LexEndFor
    ; other = Some [Else]
    ; conflicts = else_conflicts
    }
  | Raw -> pair_to_bounds Raw EndRaw
  | LexStyle -> pair_to_bounds LexStyle LexEndStyle
  | LexForm -> pair_to_bounds LexForm LexEndForm
  | Capture -> pair_to_bounds Capture EndCapture
  | Paginate -> pair_to_bounds Paginate EndPaginate
  | TableRow -> pair_to_bounds TableRow EndTableRow
  | _ -> None


let remove_last lst =
  match List.rev lst with
  | _ :: hds -> List.rev hds
  | [] -> []

let pair_up_bounds bounds =
  let rec aux acc =
    function
    | ft :: sd :: td :: tl -> aux (acc @ [(ft, sd); (sd, td)]) (td :: tl)
    | ft :: sd :: tl -> aux (acc @ [(ft, sd)]) tl
    | [] | [_] -> acc
  in aux [] bounds

type conflict_type = Start | End
let has_conflict conflict_type bounds token =
  match bounds.conflicts with
  | Some conflicts -> (
    let extract_tag b = match conflict_type with Start -> b.start | End -> b.stop in
    let starts = List.map conflicts ~f:extract_tag in
    contains starts token
  )
  | _ -> false
let has_start_conflict = has_conflict Start
let has_end_conflict = has_conflict End

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
    | _ when token = bounds.start || has_start_conflict bounds token ->
      let acc = (tally @ [index], found) in
      Next (acc, index+1)
    | _ when token = bounds.stop || has_end_conflict bounds token  -> (
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