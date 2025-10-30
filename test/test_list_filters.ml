open Alcotest
open Liquid_ml
open Liquid

(* List Filters *)

let test_compact () =
  let context =
    Ctx.empty
    |> Ctx.add "items" (List [ String "a"; Nil; String "b"; Nil; String "c" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ items | compact | join: ',' }}" in
  check string "compact filter" "a,b,c" result

let test_concat () =
  let context =
    Ctx.empty
    |> Ctx.add "list1" (List [ String "a"; String "b" ])
    |> Ctx.add "list2" (List [ String "c"; String "d" ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{{ list1 | concat: list2 | join: ',' }}"
  in
  check string "concat filter" "a,b,c,d" result

let test_map () =
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
  let context =
    Ctx.empty |> Ctx.add "users" (List [ Object obj1; Object obj2 ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ users | map: 'name' | join: ', ' }}" in
  check string "map filter" "Alice, Bob" result

let test_slice_list () =
  let context =
    Ctx.empty
    |> Ctx.add "items" (List [ String "a"; String "b"; String "c"; String "d" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ items | slice: 1, 3 | join: ',' }}" in
  check string "slice filter on list" "b,c,d" result

let test_slice_string () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello world") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | slice: 0, 5 }}" in
  check string "slice filter on string" "hello" result

let test_slice_single_index () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | slice: 1, 1 }}" in
  check string "slice filter single index" "e" result

let test_uniq () =
  let context =
    Ctx.empty
    |> Ctx.add "items"
         (List [ String "a"; String "b"; String "a"; String "c"; String "b" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ items | uniq | join: ',' }}" in
  check string "uniq filter" "a,b,c" result

let test_where_with_value () =
  let obj1 =
    Object.empty
    |> Object.add "name" (String "Alice")
    |> Object.add "active" (Bool true)
  in
  let obj2 =
    Object.empty
    |> Object.add "name" (String "Bob")
    |> Object.add "active" (Bool false)
  in
  let obj3 =
    Object.empty
    |> Object.add "name" (String "Charlie")
    |> Object.add "active" (Bool true)
  in
  let context =
    Ctx.empty
    |> Ctx.add "users" (List [ Object obj1; Object obj2; Object obj3 ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% assign active_users = users | where: 'active', true %}{{ \
       active_users | map: 'name' | join: ', ' }}"
  in
  check string "where filter with value" "Alice, Charlie" result

let test_where_truthy () =
  let obj1 =
    Object.empty
    |> Object.add "name" (String "Alice")
    |> Object.add "active" (Bool true)
  in
  let obj2 =
    Object.empty
    |> Object.add "name" (String "Bob")
    |> Object.add "active" (Bool false)
  in
  let obj3 =
    Object.empty
    |> Object.add "name" (String "Charlie")
    |> Object.add "active" (Bool true)
  in
  let context =
    Ctx.empty
    |> Ctx.add "users" (List [ Object obj1; Object obj2; Object obj3 ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% assign active_users = users | where: 'active' %}{{ active_users | \
       map: 'name' | join: ', ' }}"
  in
  check string "where filter truthy" "Alice, Charlie" result

let test_sort_natural () =
  let context =
    Ctx.empty
    |> Ctx.add "items"
         (List [ String "item 10"; String "item 2"; String "item 1" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ items | sort_natural | join: ',' }}" in
  check string "sort_natural filter" "item 1,item 10,item 2" result

let test_sort_natural_objects () =
  let obj1 = Object.empty |> Object.add "name" (String "item 10") in
  let obj2 = Object.empty |> Object.add "name" (String "item 2") in
  let obj3 = Object.empty |> Object.add "name" (String "item 1") in
  let context =
    Ctx.empty
    |> Ctx.add "items" (List [ Object obj1; Object obj2; Object obj3 ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{{ items | sort_natural: 'name' | map: 'name' | join: ',' }}"
  in
  check string "sort_natural filter with key" "item 1,item 10,item 2" result

(* Test suite *)
let suite =
  ( "List Filter Tests"
  , [
      test_case "compact" `Quick test_compact
    ; test_case "concat" `Quick test_concat
    ; test_case "map" `Quick test_map
    ; test_case "slice list" `Quick test_slice_list
    ; test_case "slice string" `Quick test_slice_string
    ; test_case "slice single index" `Quick test_slice_single_index
    ; test_case "uniq" `Quick test_uniq
    ; test_case "where with value" `Quick test_where_with_value
    ; test_case "where truthy" `Quick test_where_truthy
    ; test_case "sort_natural" `Quick test_sort_natural
    ; test_case "sort_natural with key" `Quick test_sort_natural_objects
    ] )
