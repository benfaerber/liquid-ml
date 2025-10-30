open Alcotest
open Liquid_ml
open Liquid

(* String Filters *)

let test_upcase () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | upcase }}" in
  check string "upcase filter" "HELLO" result

let test_downcase () =
  let context = Ctx.empty |> Ctx.add "text" (String "HELLO") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | downcase }}" in
  check string "downcase filter" "hello" result

let test_capitalize () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello world") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | capitalize }}" in
  check string "capitalize filter" "Hello world" result

let test_append () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | append: ' world' }}" in
  check string "append filter" "hello world" result

let test_prepend () =
  let context = Ctx.empty |> Ctx.add "text" (String "world") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | prepend: 'hello ' }}" in
  check string "prepend filter" "hello world" result

let test_replace () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello world") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | replace: 'world', 'OCaml' }}" in
  check string "replace filter" "hello OCaml" result

let test_remove () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello world") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | remove: ' world' }}" in
  check string "remove filter" "hello" result

let test_strip () =
  let context = Ctx.empty |> Ctx.add "text" (String "  hello  ") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | strip }}" in
  check string "strip filter" "hello" result

let test_lstrip () =
  let context = Ctx.empty |> Ctx.add "text" (String "  hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | lstrip }}" in
  check string "lstrip filter" "hello" result

let test_rstrip () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello  ") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | rstrip }}" in
  check string "rstrip filter" "hello" result

let test_handleize () =
  let context = Ctx.empty |> Ctx.add "text" (String "Hello World! 123") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | handleize }}" in
  check string "handleize filter" "hello-world-123" result

let test_camelcase () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello-world_test") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | camelcase }}" in
  check string "camelcase filter" "HelloWorldTest" result

let test_base64_encode () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | base64_encode }}" in
  check string "base64_encode filter" "aGVsbG8=" result

let test_base64_decode () =
  let context = Ctx.empty |> Ctx.add "text" (String "aGVsbG8=") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | base64_decode }}" in
  check string "base64_decode filter" "hello" result

let test_split () =
  let context = Ctx.empty |> Ctx.add "text" (String "a,b,c") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | split: ',' | join: '-' }}" in
  check string "split filter" "a-b-c" result

let test_truncate () =
  let context =
    Ctx.empty |> Ctx.add "text" (String "Ground control to Major Tom.")
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | truncate: 20 }}" in
  check string "truncate filter" "Ground control to..." result

let test_size () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | size }}" in
  check string "size filter" "5" result

(* Number Filters *)

let test_plus () =
  let context = Ctx.empty |> Ctx.add "num" (Number 5.0) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | plus: 3 }}" in
  check string "plus filter" "8" result

let test_minus () =
  let context = Ctx.empty |> Ctx.add "num" (Number 10.0) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | minus: 3 }}" in
  check string "minus filter" "7" result

let test_times () =
  let context = Ctx.empty |> Ctx.add "num" (Number 4.0) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | times: 3 }}" in
  check string "times filter" "12" result

let test_divided_by () =
  let context = Ctx.empty |> Ctx.add "num" (Number 12.0) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | divided_by: 3 }}" in
  check string "divided_by filter" "4" result

let test_modulo () =
  let context = Ctx.empty |> Ctx.add "num" (Number 10.0) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | modulo: 3 }}" in
  (* Float modulo may return 1.0 *)
  check string "modulo filter" "1" result

let test_abs () =
  let context = Ctx.empty |> Ctx.add "num" (Number (-5.0)) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | abs }}" in
  check string "abs filter" "5" result

let test_round () =
  let context = Ctx.empty |> Ctx.add "num" (Number 3.7) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | round }}" in
  check string "round filter" "4" result

let test_floor () =
  let context = Ctx.empty |> Ctx.add "num" (Number 3.7) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | floor }}" in
  check string "floor filter" "3" result

let test_ceil () =
  let context = Ctx.empty |> Ctx.add "num" (Number 3.2) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | ceil }}" in
  check string "ceil filter" "4" result

let test_at_least () =
  let context = Ctx.empty |> Ctx.add "num" (Number 3.0) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | at_least: 5 }}" in
  check string "at_least filter" "5" result

let test_at_most () =
  let context = Ctx.empty |> Ctx.add "num" (Number 8.0) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ num | at_most: 5 }}" in
  check string "at_most filter" "5" result

(* List Filters *)

let test_first () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "A"; String "B"; String "C" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ items | first }}" in
  check string "first filter" "A" result

let test_last () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "A"; String "B"; String "C" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ items | last }}" in
  check string "last filter" "C" result

let test_join () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "A"; String "B"; String "C" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ items | join: ', ' }}" in
  check string "join filter" "A, B, C" result

let test_reverse () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "A"; String "B"; String "C" ])
  in
  let settings = Settings.make ~context () in
  let template =
    "{% assign rev = items | reverse %}{% for item in rev %}{{ item }}{% \
     endfor %}"
  in
  let result = render_text ~settings template in
  check string "reverse filter" "CBA" result

let test_sort () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "C"; String "A"; String "B" ])
  in
  let settings = Settings.make ~context () in
  let template =
    "{% assign sorted = items | sort %}{% for item in sorted %}{{ item }}{% \
     endfor %}"
  in
  let result = render_text ~settings template in
  check string "sort filter" "ABC" result

(* Custom Filter Test *)

let test_custom_filter () =
  let double _ = function
    | Number n :: _ -> Ok (Number (n *. 2.0))
    | _ -> Error "double accepts a number"
  in
  let custom_filters = function "double" -> Some double | _ -> None in
  let context = Ctx.empty |> Ctx.add "num" (Number 5.0) in
  let settings = Settings.make ~context ~filters:custom_filters () in
  let result = render_text ~settings "{{ num | double }}" in
  check string "custom filter" "10" result

(* Chained Filters *)

let test_chained_filters () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | upcase | append: '!' }}" in
  check string "chained filters" "HELLO!" result

let suite =
  ( "Filter Tests",
    [
      (* String filters *)
      test_case "upcase" `Quick test_upcase;
      test_case "downcase" `Quick test_downcase;
      test_case "capitalize" `Quick test_capitalize;
      test_case "append" `Quick test_append;
      test_case "prepend" `Quick test_prepend;
      test_case "replace" `Quick test_replace;
      test_case "remove" `Quick test_remove;
      test_case "strip" `Quick test_strip;
      test_case "lstrip" `Quick test_lstrip;
      test_case "rstrip" `Quick test_rstrip;
      test_case "handleize" `Quick test_handleize;
      test_case "camelcase" `Quick test_camelcase;
      test_case "base64_encode" `Quick test_base64_encode;
      test_case "base64_decode" `Quick test_base64_decode;
      test_case "split" `Quick test_split;
      test_case "truncate" `Quick test_truncate;
      test_case "size" `Quick test_size;
      (* Number filters *)
      test_case "plus" `Quick test_plus;
      test_case "minus" `Quick test_minus;
      test_case "times" `Quick test_times;
      test_case "divided_by" `Quick test_divided_by;
      test_case "modulo" `Quick test_modulo;
      test_case "abs" `Quick test_abs;
      test_case "round" `Quick test_round;
      test_case "floor" `Quick test_floor;
      test_case "ceil" `Quick test_ceil;
      test_case "at_least" `Quick test_at_least;
      test_case "at_most" `Quick test_at_most;
      (* List filters *)
      test_case "first" `Quick test_first;
      test_case "last" `Quick test_last;
      test_case "join" `Quick test_join;
      test_case "reverse" `Quick test_reverse;
      test_case "sort" `Quick test_sort;
      (* Custom and chained *)
      test_case "custom filter" `Quick test_custom_filter;
      test_case "chained filters" `Quick test_chained_filters;
    ] )
