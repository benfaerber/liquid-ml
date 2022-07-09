open Base

type block =
  | Text of string
  | Statement of string
  | Expression of string
  | Liquid of string

type keywords =
  | If
  | Else
  | EndIf
  | Unless
  | EndUnless
  | Case
  | EndCase
  | When
  | For
  | EndFor
  | Break
  | KwContinue
  | Cycle
  | TableRow
  | EndTableRow
  | Paginate
  | EndPaginate
  | In
  | Contains
  | By
  | Pipe
  | Colon

let range x =
  Batteries.(--) 0 x |> Batteries.List.of_enum

let sub_prefix text x = String.sub text ~pos:x
let sub_suffix text x = String.sub text ~pos:0 ~len:(String.length text - x)

type ('acc, 'curr) loop =
  | Next of ('acc * 'curr)
  | Stop of 'acc

let rec unfold acc curr func =
  match func acc curr with
  | Next((nacc, ncurr)) -> unfold nacc ncurr func
  | Stop(acc) -> acc

let consume_until start_index text stop_point =
  let folder _ index =
    let chunk = String.sub text ~pos:start_index ~len:index in
    if String.is_suffix chunk ~suffix:stop_point then
      let clean_chunk = sub_suffix chunk (String.length stop_point) in
      Stop (clean_chunk)
    else
      Next ((chunk, index+1))
  in

  unfold "" start_index folder

let tokenize text =
  let folder acc index =
    let raw_block =
      match String.sub text ~pos:index ~len:(index+2) with
      | "{{" -> consume_until (index+2) text "}}"
      | "{%" -> consume_until (index+2) text "%}"
      | other -> other in

    Stdio.printf "%s\n%d %d\n\n" raw_block (String.length (text)) index;

    if (index+2) < String.length (text) then
      Next ((acc @ [raw_block], index + (String.length raw_block)))
    else begin
      Stdio.print_endline "Stop";
      Stop(acc @ [raw_block])
    end
  in

  unfold [] 0 folder


let test () =
  "liquid/block_test.liquid"
  |> File.read
  |> tokenize
  |> Batteries.dump
  |> Stdio.print_string;;

  (* consume_until 0 "cool thing }} asdf asdf" "}}"
  |> Batteries.dump
  |> Stdio.print_string; *)

  Stdio.print_string "\n"
