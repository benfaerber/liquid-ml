open Alcotest
open Liquid_ml
open Liquid

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

(* Test suite *)
let suite =
  ( "Number Filter Tests",
    [
      test_case "plus" `Quick test_plus;
      test_case "minus" `Quick test_minus;
      test_case "times" `Quick test_times;
      test_case "divided_by" `Quick test_divided_by;
      test_case "modulo" `Quick test_modulo;
      test_case "abs" `Quick test_abs;
      test_case "round" `Quick test_round;
      test_case "floor" `Quick test_floor;
      test_case "ceil" `Quick test_ceil;
    ] )
