open Alcotest
open Liquid_ml
open Liquid

(* Test basic text rendering *)
let test_plain_text () =
  let result = render_text "Hello, World!" in
  check string "plain text" "Hello, World!" result

(* Test variable interpolation *)
let test_variable_interpolation () =
  let context = Ctx.empty |> Ctx.add "name" (String "Alice") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "Hello, {{ name }}!" in
  check string "variable interpolation" "Hello, Alice!" result

(* Test multiple variables *)
let test_multiple_variables () =
  let context =
    Ctx.empty
    |> Ctx.add "first" (String "John")
    |> Ctx.add "last" (String "Doe")
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "Name: {{ first }} {{ last }}" in
  check string "multiple variables" "Name: John Doe" result

(* Test number variable *)
let test_number_variable () =
  let context = Ctx.empty |> Ctx.add "count" (Number 42.0) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "Count: {{ count }}" in
  check string "number variable" "Count: 42" result

(* Test boolean variable *)
let test_boolean_variable () =
  let context = Ctx.empty |> Ctx.add "active" (Bool true) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "Active: {{ active }}" in
  check string "boolean variable" "Active: true" result

(* Test undefined variable returns nil *)
let test_undefined_variable () =
  let result = render_text "{{ undefined_var }}" in
  check string "undefined variable" "nil" result

(* Test if statement - true condition *)
let test_if_true () =
  let context = Ctx.empty |> Ctx.add "show" (Bool true) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% if show %}Visible{% endif %}" in
  check string "if true" "Visible" result

(* Test if statement - false condition *)
let test_if_false () =
  let context = Ctx.empty |> Ctx.add "show" (Bool false) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% if show %}Visible{% endif %}" in
  check string "if false" "" result

(* Test if-else statement *)
let test_if_else () =
  let context = Ctx.empty |> Ctx.add "value" (Number 5.0) in
  let settings = Settings.make ~context () in
  let template = "{% if value > 10 %}Large{% else %}Small{% endif %}" in
  let result = render_text ~settings template in
  check string "if-else" "Small" result

(* Test unless statement *)
let test_unless () =
  let context = Ctx.empty |> Ctx.add "hide" (Bool false) in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{% unless hide %}Visible{% endunless %}"
  in
  check string "unless" "Visible" result

(* Test for loop with list *)
let test_for_loop () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "A"; String "B"; String "C" ])
  in
  let settings = Settings.make ~context () in
  let template = "{% for item in items %}{{ item }}{% endfor %}" in
  let result = render_text ~settings template in
  check string "for loop" "ABC" result

(* Test case statement *)
let test_case_statement () =
  let context = Ctx.empty |> Ctx.add "fruit" (String "apple") in
  let settings = Settings.make ~context () in
  let template =
    "{% case fruit %}{% when 'apple' %}Red{% when 'banana' %}Yellow{% endcase \
     %}"
  in
  let result = render_text ~settings template in
  check string "case statement" "Red" result

(* Test assign tag *)
let test_assign () =
  let template = "{% assign x = 'test' %}{{ x }}" in
  let result = render_text template in
  check string "assign" "test" result

(* Test comment tag *)
let test_comment () =
  let template = "Before{% comment %}Hidden{% endcomment %}After" in
  let result = render_text template in
  check string "comment" "BeforeAfter" result

(* Test raw tag *)
let test_raw () =
  let template = "{% raw %}{{ not_a_variable }}{% endraw %}" in
  let result = render_text template in
  check string "raw" "{{ not_a_variable }}" result

(* Test object access with dot notation *)
let test_object_access () =
  let obj = Object.empty |> Object.add "name" (String "Alice") in
  let context = Ctx.empty |> Ctx.add "user" (Object obj) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ user.name }}" in
  check string "object access" "Alice" result

(* Test nested object access *)
let test_nested_object_access () =
  let address = Object.empty |> Object.add "city" (String "NYC") in
  let user = Object.empty |> Object.add "address" (Object address) in
  let context = Ctx.empty |> Ctx.add "user" (Object user) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ user.address.city }}" in
  check string "nested object access" "NYC" result

(* Test list index access *)
let test_list_index_access () =
  let context =
    Ctx.empty
    |> Ctx.add "items"
         (List [ String "First"; String "Second"; String "Third" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ items[1] }}" in
  check string "list index access" "Second" result

let suite =
  ( "Rendering Tests"
  , [
      test_case "Plain text" `Quick test_plain_text
    ; test_case "Variable interpolation" `Quick test_variable_interpolation
    ; test_case "Multiple variables" `Quick test_multiple_variables
    ; test_case "Number variable" `Quick test_number_variable
    ; test_case "Boolean variable" `Quick test_boolean_variable
    ; test_case "Undefined variable" `Quick test_undefined_variable
    ; test_case "If statement (true)" `Quick test_if_true
    ; test_case "If statement (false)" `Quick test_if_false
    ; test_case "If-else statement" `Quick test_if_else
    ; test_case "Unless statement" `Quick test_unless
    ; test_case "For loop" `Quick test_for_loop
    ; test_case "Case statement" `Quick test_case_statement
    ; test_case "Assign tag" `Quick test_assign
    ; test_case "Comment tag" `Quick test_comment
    ; test_case "Raw tag" `Quick test_raw
    ; test_case "Object access" `Quick test_object_access
    ; test_case "Nested object access" `Quick test_nested_object_access
    ; test_case "List index access" `Quick test_list_index_access
    ] )
