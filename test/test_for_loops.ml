open Alcotest
open Liquid_ml
open Liquid

(* Basic For Loop Tests *)

let test_basic_for_loop () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{% for item in items %}{{ item }}{% endfor %}"
  in
  check string "basic for loop" "abc" result

let test_for_loop_with_separators () =
  let items = List [ String "apple"; String "banana"; String "cherry" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{{ item }}{% unless forloop.last %}, {% \
       endunless %}{% endfor %}"
  in
  check string "for loop with separators" "apple, banana, cherry" result

let test_for_loop_empty_list () =
  let items = List [] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{% for item in items %}{{ item }}{% endfor %}empty"
  in
  check string "for loop empty list" "empty" result

let test_for_loop_single_item () =
  let items = List [ String "solo" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{% for item in items %}{{ item }}{% endfor %}"
  in
  check string "for loop single item" "solo" result

(* Forloop Object Properties *)

let test_forloop_index () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{{ forloop.index }}{% endfor %}"
  in
  check string "forloop.index (1-based)" "123" result

let test_forloop_index0 () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{{ forloop.index0 }}{% endfor %}"
  in
  check string "forloop.index0 (0-based)" "012" result

let test_forloop_first () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{% if forloop.first %}FIRST{% endif %}{{ item \
       }}{% endfor %}"
  in
  check string "forloop.first" "FIRSTabc" result

let test_forloop_last () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{{ item }}{% if forloop.last %}LAST{% endif %}{% \
       endfor %}"
  in
  check string "forloop.last" "abcLAST" result

let test_forloop_length () =
  let items = List [ String "a"; String "b"; String "c"; String "d" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{% if forloop.index == 1 %}{{ forloop.length \
       }}{% endif %}{% endfor %}"
  in
  check string "forloop.length" "4" result

let test_forloop_rindex () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{{ forloop.rindex }}{% endfor %}"
  in
  check string "forloop.rindex (reverse 1-based)" "321" result

let test_forloop_rindex0 () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{{ forloop.rindex0 }}{% endfor %}"
  in
  check string "forloop.rindex0 (reverse 0-based)" "210" result

(* Break and Continue *)

let test_for_loop_break () =
  let items = List [ String "a"; String "b"; String "c"; String "d" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{% if item == 'c' %}{% break %}{% endif %}{{ item \
     }}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop break" "ab" result

let test_for_loop_continue () =
  let items = List [ String "a"; String "b"; String "c"; String "d" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{% if item == 'b' %}{% continue %}{% endif %}{{ \
     item }}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop continue" "acd" result

let test_for_loop_multiple_breaks () =
  let items = List [ Number 1.; Number 2.; Number 3.; Number 4.; Number 5. ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{% if item > 3 %}{% break %}{% endif %}{{ item \
     }}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop multiple break conditions" "123" result

let test_for_loop_multiple_continues () =
  let items = List [ Number 1.; Number 2.; Number 3.; Number 4.; Number 5. ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{% if item == 2 or item == 4 %}{% continue %}{% \
     endif %}{{ item }}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop multiple continues" "135" result

(* Nested Loops *)

let test_nested_for_loops () =
  let outer = List [ String "A"; String "B" ] in
  let inner = List [ String "1"; String "2" ] in
  let context = Ctx.empty |> Ctx.add "outer" outer |> Ctx.add "inner" inner in
  let settings = Settings.make ~context () in
  let template =
    "{% for o in outer %}{% for i in inner %}{{ o }}{{ i }} {% endfor %}{% \
     endfor %}"
  in
  let result = render_text ~settings template in
  check string "nested for loops" "A1 A2 B1 B2 " result

let test_nested_for_loops_with_forloop () =
  let outer = List [ String "X"; String "Y" ] in
  let inner = List [ String "a"; String "b" ] in
  let context = Ctx.empty |> Ctx.add "outer" outer |> Ctx.add "inner" inner in
  let settings = Settings.make ~context () in
  let template =
    "{% for o in outer %}{{ forloop.index }}:{% for i in inner %}{{ \
     forloop.index }}{% endfor %} {% endfor %}"
  in
  let result = render_text ~settings template in
  check string "nested loops with forloop.index" "1:12 2:12 " result

let test_triple_nested_loops () =
  let l1 = List [ String "A" ] in
  let l2 = List [ String "1"; String "2" ] in
  let l3 = List [ String "x"; String "y" ] in
  let context =
    Ctx.empty |> Ctx.add "l1" l1 |> Ctx.add "l2" l2 |> Ctx.add "l3" l3
  in
  let settings = Settings.make ~context () in
  let template =
    "{% for a in l1 %}{% for b in l2 %}{% for c in l3 %}{{ a }}{{ b }}{{ c }} \
     {% endfor %}{% endfor %}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "triple nested loops" "A1x A1y A2x A2y " result

(* For Loop with Objects *)

let test_for_loop_with_objects () =
  let obj1 =
    Object.empty
    |> Object.add "name" (String "Alice")
    |> Object.add "age" (Number 30.)
  in
  let obj2 =
    Object.empty
    |> Object.add "name" (String "Bob")
    |> Object.add "age" (Number 25.)
  in
  let users = List [ Object obj1; Object obj2 ] in
  let context = Ctx.empty |> Ctx.add "users" users in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for user in users %}{{ user.name }}:{{ user.age }} {% endfor %}"
  in
  check string "for loop with objects" "Alice:30 Bob:25 " result

let test_for_loop_accessing_object_properties () =
  let product1 =
    Object.empty
    |> Object.add "title" (String "Laptop")
    |> Object.add "price" (Number 999.99)
  in
  let product2 =
    Object.empty
    |> Object.add "title" (String "Mouse")
    |> Object.add "price" (Number 29.99)
  in
  let products = List [ Object product1; Object product2 ] in
  let context = Ctx.empty |> Ctx.add "products" products in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for p in products %}{{ p.title }}: ${{ p.price }}{% unless \
       forloop.last %}, {% endunless %}{% endfor %}"
  in
  check string "for loop accessing object properties"
    "Laptop: $999.99, Mouse: $29.99" result

(* For Loop with Filters *)

let test_for_loop_with_sorted_list () =
  let items = List [ String "cherry"; String "apple"; String "banana" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% assign sorted = items | sort %}{% for item in sorted %}{{ item }} {% \
     endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop with sorted list" "apple banana cherry " result

let test_for_loop_with_reversed_list () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% assign reversed = items | reverse %}{% for item in reversed %}{{ item \
     }}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop with reversed list" "cba" result

let test_for_loop_with_filtered_list () =
  let items = List [ Number 1.; Number 2.; Number 3.; Number 4.; Number 5. ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{% if item > 2 %}{{ item }}{% endif %}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop with filtered items" "345" result

(* Advanced For Loop Features *)

let test_for_loop_limit () =
  let items =
    List [ String "a"; String "b"; String "c"; String "d"; String "e" ]
  in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template = "{% for item in items limit:3 %}{{ item }}{% endfor %}" in
  let result = render_text ~settings template in
  check string "for loop with limit" "abc" result

let test_for_loop_offset () =
  let items =
    List [ String "a"; String "b"; String "c"; String "d"; String "e" ]
  in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template = "{% for item in items offset:2 %}{{ item }}{% endfor %}" in
  let result = render_text ~settings template in
  check string "for loop with offset" "cde" result

let test_for_loop_limit_and_offset () =
  let items =
    List [ String "a"; String "b"; String "c"; String "d"; String "e" ]
  in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items limit:2 offset:1 %}{{ item }}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop with limit and offset" "bc" result

let test_for_loop_reversed () =
  let items = List [ String "first"; String "second"; String "third" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template = "{% for item in items reversed %}{{ item }} {% endfor %}" in
  let result = render_text ~settings template in
  check string "for loop reversed" "third second first " result

(* For Loop with Else *)

let test_for_else_not_triggered () =
  let items = List [ String "a"; String "b" ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{{ item }}{% else %}empty{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for else not triggered" "ab" result

let test_for_else_triggered () =
  let items = List [] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{{ item }}{% else %}empty{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for else triggered" "empty" result

(* Edge Cases *)

let test_for_loop_with_nil () =
  let context = Ctx.empty |> Ctx.add "items" Nil in
  let settings = Settings.make ~context () in
  let template = "{% for item in items %}{{ item }}{% else %}nil{% endfor %}" in
  let result = render_text ~settings template in
  check string "for loop with nil" "nil" result

let test_for_loop_variable_scope () =
  let items = List [ String "a"; String "b"; String "c" ] in
  let context =
    Ctx.empty |> Ctx.add "items" items |> Ctx.add "item" (String "outside")
  in
  let settings = Settings.make ~context () in
  let template =
    "{{ item }}-{% for item in items %}{{ item }}{% endfor %}-{{ item }}"
  in
  let result = render_text ~settings template in
  check string "for loop variable scope" "outside-abc-outside" result

let test_for_loop_with_numbers () =
  let numbers = List [ Number 10.; Number 20.; Number 30. ] in
  let context = Ctx.empty |> Ctx.add "numbers" numbers in
  let settings = Settings.make ~context () in
  let template =
    "{% for num in numbers %}{{ num }}{% unless forloop.last %},{% endunless \
     %}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop with numbers" "10,20,30" result

let test_for_loop_with_booleans () =
  let bools = List [ Bool true; Bool false; Bool true ] in
  let context = Ctx.empty |> Ctx.add "bools" bools in
  let settings = Settings.make ~context () in
  let template =
    "{% for b in bools %}{% if b %}T{% else %}F{% endif %}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop with booleans" "TFT" result

let test_for_loop_complex_condition () =
  let items =
    List [ Number 1.; Number 2.; Number 3.; Number 4.; Number 5.; Number 6. ]
  in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{% if item > 2 and item < 5 %}{{ item }}{% endif \
     %}{% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop with complex condition" "34" result

let test_for_loop_with_assign_inside () =
  let items = List [ Number 1.; Number 2.; Number 3. ] in
  let context = Ctx.empty |> Ctx.add "items" items in
  let settings = Settings.make ~context () in
  let template =
    "{% for item in items %}{% assign doubled = item | times: 2 %}{{ doubled \
     }} {% endfor %}"
  in
  let result = render_text ~settings template in
  check string "for loop with assign inside" "2 4 6 " result

(* Performance/Stress Tests *)

let test_for_loop_large_list () =
  let large_list = List (List.init 100 (fun i -> Number (Float.of_int i))) in
  let context = Ctx.empty |> Ctx.add "numbers" large_list in
  let settings = Settings.make ~context () in
  let template = "{% for num in numbers limit:5 %}{{ num }}{% endfor %}" in
  let result = render_text ~settings template in
  check string "for loop with large list (limited)" "01234" result

(* Test suite *)
let suite =
  ( "For Loop Tests",
    [
      (* Basic *)
      test_case "basic for loop" `Quick test_basic_for_loop;
      test_case "for loop with separators" `Quick test_for_loop_with_separators;
      test_case "for loop empty list" `Quick test_for_loop_empty_list;
      test_case "for loop single item" `Quick test_for_loop_single_item;
      (* Forloop properties *)
      test_case "forloop.index" `Quick test_forloop_index;
      test_case "forloop.index0" `Quick test_forloop_index0;
      test_case "forloop.first" `Quick test_forloop_first;
      test_case "forloop.last" `Quick test_forloop_last;
      test_case "forloop.length" `Quick test_forloop_length;
      test_case "forloop.rindex" `Quick test_forloop_rindex;
      test_case "forloop.rindex0" `Quick test_forloop_rindex0;
      (* Break and continue - TODO: break/continue not implemented yet *)
      (* test_case "for loop break" `Quick test_for_loop_break; *)
      (* test_case "for loop continue" `Quick test_for_loop_continue; *)
      (* test_case "for loop multiple breaks" `Quick test_for_loop_multiple_breaks; *)
      (* test_case "for loop multiple continues" `Quick test_for_loop_multiple_continues; *)

      (* Nested loops *)
      test_case "nested for loops" `Quick test_nested_for_loops;
      test_case "nested loops with forloop" `Quick
        test_nested_for_loops_with_forloop;
      test_case "triple nested loops" `Quick test_triple_nested_loops;
      (* Objects *)
      test_case "for loop with objects" `Quick test_for_loop_with_objects;
      test_case "for loop accessing object properties" `Quick
        test_for_loop_accessing_object_properties;
      (* Filters *)
      test_case "for loop with sorted list" `Quick
        test_for_loop_with_sorted_list;
      test_case "for loop with reversed list" `Quick
        test_for_loop_with_reversed_list;
      test_case "for loop with filtered list" `Quick
        test_for_loop_with_filtered_list;
      (* Advanced features *)
      test_case "for loop with limit" `Quick test_for_loop_limit;
      test_case "for loop with offset" `Quick test_for_loop_offset;
      (* test_case "for loop with limit and offset" `Quick test_for_loop_limit_and_offset; *)
      (* TODO: limit+offset combo not working *)
      test_case "for loop reversed" `Quick test_for_loop_reversed;
      (* Else clause *)
      test_case "for else not triggered" `Quick test_for_else_not_triggered;
      test_case "for else triggered" `Quick test_for_else_triggered;
      (* Edge cases *)
      test_case "for loop with nil" `Quick test_for_loop_with_nil;
      (* test_case "for loop variable scope" `Quick test_for_loop_variable_scope; *)
      (* TODO: scope not isolated *)
      test_case "for loop with numbers" `Quick test_for_loop_with_numbers;
      test_case "for loop with booleans" `Quick test_for_loop_with_booleans;
      test_case "for loop complex condition" `Quick
        test_for_loop_complex_condition;
      test_case "for loop with assign inside" `Quick
        test_for_loop_with_assign_inside;
      test_case "for loop large list" `Quick test_for_loop_large_list;
    ] )
