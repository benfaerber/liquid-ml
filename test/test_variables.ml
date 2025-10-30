open Alcotest
open Liquid_ml
open Liquid

(* Test context creation and manipulation *)

let test_empty_context () =
  let ctx = Ctx.empty in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ undefined }}" in
  check string "empty context" "nil" result

let test_add_string () =
  let ctx = Ctx.empty |> Ctx.add "name" (String "Alice") in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ name }}" in
  check string "add string" "Alice" result

let test_add_number () =
  let ctx = Ctx.empty |> Ctx.add "age" (Number 25.0) in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ age }}" in
  check string "add number" "25" result

let test_add_boolean () =
  let ctx = Ctx.empty |> Ctx.add "active" (Bool true) in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ active }}" in
  check string "add boolean" "true" result

let test_add_list () =
  let items = List [String "apple"; String "banana"; String "cherry"] in
  let ctx = Ctx.empty |> Ctx.add "fruits" items in
  let settings = Settings.make ~context:ctx () in
  let template = "{% for fruit in fruits %}{{ fruit }} {% endfor %}" in
  let result = render_text ~settings template in
  check string "add list" "apple banana cherry " result

let test_add_object () =
  let obj = Object.empty |> Object.add "x" (Number 10.0) |> Object.add "y" (Number 20.0) in
  let ctx = Ctx.empty |> Ctx.add "point" (Object obj) in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ point.x }},{{ point.y }}" in
  check string "add object" "10,20" result

(* Test object manipulation *)

let test_empty_object () =
  let obj = Object.empty in
  let ctx = Ctx.empty |> Ctx.add "empty" (Object obj) in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ empty.missing }}" in
  check string "empty object" "nil" result

let test_object_add_multiple () =
  let obj =
    Object.empty
    |> Object.add "name" (String "Bob")
    |> Object.add "age" (Number 30.0)
    |> Object.add "city" (String "NYC")
  in
  let ctx = Ctx.empty |> Ctx.add "user" (Object obj) in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ user.name }} is {{ user.age }} in {{ user.city }}" in
  check string "object with multiple fields" "Bob is 30 in NYC" result

let test_nested_objects () =
  let address =
    Object.empty
    |> Object.add "street" (String "Main St")
    |> Object.add "city" (String "Boston")
  in
  let user =
    Object.empty
    |> Object.add "name" (String "Charlie")
    |> Object.add "address" (Object address)
  in
  let ctx = Ctx.empty |> Ctx.add "user" (Object user) in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ user.name }} lives on {{ user.address.street }}" in
  check string "nested objects" "Charlie lives on Main St" result

(* Test nil handling *)

let test_nil_value () =
  let ctx = Ctx.empty |> Ctx.add "nothing" Nil in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{{ nothing }}" in
  check string "nil value" "nil" result

let test_nil_in_comparison () =
  let ctx = Ctx.empty |> Ctx.add "value" Nil in
  let settings = Settings.make ~context:ctx () in
  let result = render_text ~settings "{% if value %}yes{% else %}no{% endif %}" in
  check string "nil is falsy" "no" result

(* Test variable scoping in loops *)

let test_variable_scope_in_for () =
  let items = List [String "A"; String "B"; String "C"] in
  let ctx = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context:ctx () in
  let template = "{% for item in items %}{{ item }}{% endfor %}{{ item }}" in
  let result = render_text ~settings template in
  (* After loop, 'item' contains the last value from the loop *)
  check string "variable scope in for" "ABCC" result

(* Test forloop object *)

let test_forloop_index () =
  let items = List [String "A"; String "B"; String "C"] in
  let ctx = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context:ctx () in
  let template = "{% for item in items %}{{ forloop.index }}{% endfor %}" in
  let result = render_text ~settings template in
  check string "forloop.index" "123" result

let test_forloop_index0 () =
  let items = List [String "A"; String "B"; String "C"] in
  let ctx = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context:ctx () in
  let template = "{% for item in items %}{{ forloop.index0 }}{% endfor %}" in
  let result = render_text ~settings template in
  check string "forloop.index0" "012" result

let test_forloop_first () =
  let items = List [String "A"; String "B"; String "C"] in
  let ctx = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context:ctx () in
  let template = "{% for item in items %}{% if forloop.first %}{{ item }}{% endif %}{% endfor %}" in
  let result = render_text ~settings template in
  check string "forloop.first" "A" result

let test_forloop_last () =
  let items = List [String "A"; String "B"; String "C"] in
  let ctx = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context:ctx () in
  let template = "{% for item in items %}{% if forloop.last %}{{ item }}{% endif %}{% endfor %}" in
  let result = render_text ~settings template in
  check string "forloop.last" "C" result

(* Test variable assignment and reassignment *)

let test_assign_new_variable () =
  let template = "{% assign x = 'hello' %}{{ x }}" in
  let result = render_text template in
  check string "assign new variable" "hello" result

let test_assign_from_context () =
  let ctx = Ctx.empty |> Ctx.add "original" (String "value") in
  let settings = Settings.make ~context:ctx () in
  let template = "{% assign copy = original %}{{ copy }}" in
  let result = render_text ~settings template in
  check string "assign from context" "value" result

let test_assign_with_filter () =
  let ctx = Ctx.empty |> Ctx.add "text" (String "hello") in
  let settings = Settings.make ~context:ctx () in
  let template = "{% assign upper = text | upcase %}{{ upper }}" in
  let result = render_text ~settings template in
  check string "assign with filter" "HELLO" result

(* Test capture tag *)

let test_capture_simple () =
  let template = "{% capture my_var %}Hello World{% endcapture %}{{ my_var }}" in
  let result = render_text template in
  check string "capture simple" "Hello World" result

let test_capture_with_variables () =
  let ctx = Ctx.empty |> Ctx.add "name" (String "Alice") in
  let settings = Settings.make ~context:ctx () in
  let template = "{% capture greeting %}Hello, {{ name }}!{% endcapture %}{{ greeting }}" in
  let result = render_text ~settings template in
  check string "capture with variables" "Hello, Alice!" result

let suite =
  ( "Variable Tests",
    [
      (* Context tests *)
      test_case "Empty context" `Quick test_empty_context;
      test_case "Add string" `Quick test_add_string;
      test_case "Add number" `Quick test_add_number;
      test_case "Add boolean" `Quick test_add_boolean;
      test_case "Add list" `Quick test_add_list;
      test_case "Add object" `Quick test_add_object;
      (* Object tests *)
      test_case "Empty object" `Quick test_empty_object;
      test_case "Object with multiple fields" `Quick test_object_add_multiple;
      test_case "Nested objects" `Quick test_nested_objects;
      (* Nil tests *)
      test_case "Nil value" `Quick test_nil_value;
      test_case "Nil is falsy" `Quick test_nil_in_comparison;
      (* Scoping tests *)
      test_case "Variable scope in for" `Quick test_variable_scope_in_for;
      (* Forloop tests *)
      test_case "forloop.index" `Quick test_forloop_index;
      test_case "forloop.index0" `Quick test_forloop_index0;
      test_case "forloop.first" `Quick test_forloop_first;
      test_case "forloop.last" `Quick test_forloop_last;
      (* Assignment tests *)
      test_case "Assign new variable" `Quick test_assign_new_variable;
      test_case "Assign from context" `Quick test_assign_from_context;
      test_case "Assign with filter" `Quick test_assign_with_filter;
      (* Capture tests *)
      test_case "Capture simple" `Quick test_capture_simple;
      test_case "Capture with variables" `Quick test_capture_with_variables;
    ] )
