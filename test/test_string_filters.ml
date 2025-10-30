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

let test_newline_to_br () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello\nworld") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | newline_to_br }}" in
  check string "newline_to_br filter" "hello<br />\nworld" result

let test_replace_first () =
  let context = Ctx.empty |> Ctx.add "text" (String "dog dog dog") in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{{ text | replace_first: 'dog', 'cat' }}"
  in
  check string "replace_first filter" "cat dog dog" result

let test_remove_first () =
  let context = Ctx.empty |> Ctx.add "text" (String "dog dog dog") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | remove_first: 'dog' }}" in
  check string "remove_first filter" " dog dog" result

let test_url_encode () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello world") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | url_encode }}" in
  check string "url_encode filter" "hello+world" result

let test_url_encode_special () =
  let context =
    Ctx.empty |> Ctx.add "text" (String "hello world?foo=bar&baz=qux")
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | url_encode }}" in
  check string "url_encode filter with special chars"
    "hello+world%3Ffoo%3Dbar%26baz%3Dqux" result

let test_url_decode () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello+world") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | url_decode }}" in
  check string "url_decode filter" "hello world" result

let test_url_decode_special () =
  let context =
    Ctx.empty |> Ctx.add "text" (String "hello+world%3Ffoo%3Dbar%26baz%3Dqux")
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ text | url_decode }}" in
  check string "url_decode filter with special chars"
    "hello world?foo=bar&baz=qux" result

(* Test suite *)
let suite =
  ( "String Filter Tests",
    [
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
      test_case "newline_to_br" `Quick test_newline_to_br;
      test_case "replace_first" `Quick test_replace_first;
      test_case "remove_first" `Quick test_remove_first;
      test_case "url_encode" `Quick test_url_encode;
      test_case "url_encode_special" `Quick test_url_encode_special;
      test_case "url_decode" `Quick test_url_decode;
      test_case "url_decode_special" `Quick test_url_decode_special;
    ] )
