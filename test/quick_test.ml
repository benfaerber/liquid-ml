open Liquid_ml
open Liquid

let () =
  (* Test 1: modulo *)
  let ctx1 = Ctx.empty |> Ctx.add "num" (Number 10.0) in
  let settings1 = Settings.make ~context:ctx1 () in
  let result1 = render_text ~settings:settings1 "{{ num | modulo: 3 }}" in
  Printf.printf "Modulo test: '%s'\n" result1;

  (* Test 2: empty object *)
  let obj = Object.empty in
  let ctx2 = Ctx.empty |> Ctx.add "empty" (Object obj) in
  let settings2 = Settings.make ~context:ctx2 () in
  let result2 = render_text ~settings:settings2 "{{ empty.missing }}" in
  Printf.printf "Empty object test: '%s'\n" result2;

  (* Test 3: variable scope *)
  let items = List [String "A"; String "B"; String "C"] in
  let ctx3 = Ctx.empty |> Ctx.add "items" items in
  let settings3 = Settings.make ~context:ctx3 () in
  let template3 = "{% for item in items %}{{ item }}{% endfor %}{{ item }}" in
  let result3 = render_text ~settings:settings3 template3 in
  Printf.printf "Variable scope test: '%s'\n" result3
