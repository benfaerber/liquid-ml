open Alcotest
open Liquid_ml
open Liquid
open Base

(* Date Filter Tests *)

let test_date_default_format () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date }}" in
  check string "date default format" "01/15/2025" result

let test_date_custom_format () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%Y-%m-%d' }}" in
  check string "date custom format YYYY-MM-DD" "2025-01-15" result

let test_date_year_only () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%Y' }}" in
  check string "date year only" "2025" result

let test_date_month_only () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%m' }}" in
  check string "date month only" "01" result

let test_date_day_only () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%d' }}" in
  check string "date day only" "15" result

let test_date_full_month_name () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%B' }}" in
  check string "date full month name" "January" result

let test_date_abbreviated_month () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%b' }}" in
  check string "date abbreviated month" "Jan" result

let test_date_weekday_name () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%A' }}" in
  check string "date weekday name" "Wednesday" result

let test_date_abbreviated_weekday () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%a' }}" in
  check string "date abbreviated weekday" "Wed" result

let test_date_complex_format () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ date_str | date: '%B %d, %Y' }}" in
  check string "date complex format" "January 15, 2025" result

let test_date_now_keyword () =
  let settings = Settings.make () in
  (* We can't test exact output for "now", but we can check it doesn't error *)
  let result = render_text ~settings "{{ 'now' | date: '%Y' }}" in
  (* Should be a 4-digit year *)
  check int "date now keyword returns 4 digits" 4 (String.length result)

let test_date_now_default_format () =
  let settings = Settings.make () in
  (* Check "now" with default format renders something *)
  let result = render_text ~settings "{{ 'now' | date }}" in
  (* Default format is MM/DD/YYYY, so should be 10 characters *)
  check int "date now default format length" 10 (String.length result)

(* Default Filter Tests *)

let test_default_with_value () =
  let context = Ctx.empty |> Ctx.add "var" (String "hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | default: 'fallback' }}" in
  check string "default with value" "hello" result

let test_default_with_nil () =
  let context = Ctx.empty |> Ctx.add "var" Nil in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | default: 'fallback' }}" in
  check string "default with nil" "fallback" result

let test_default_with_undefined () =
  let context = Ctx.empty in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{{ undefined_var | default: 'fallback' }}"
  in
  check string "default with undefined" "fallback" result

let test_default_with_empty_string () =
  let context = Ctx.empty |> Ctx.add "var" (String "") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | default: 'fallback' }}" in
  (* Empty string is truthy in Liquid, so should use the empty string *)
  check string "default with empty string" "" result

let test_default_with_false () =
  let context = Ctx.empty |> Ctx.add "var" (Bool false) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | default: 'fallback' }}" in
  (* false is falsy, so should use default *)
  check string "default with false" "fallback" result

let test_default_with_true () =
  let context = Ctx.empty |> Ctx.add "var" (Bool true) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | default: 'fallback' }}" in
  check string "default with true" "true" result

let test_default_with_zero () =
  let context = Ctx.empty |> Ctx.add "var" (Number 0.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | default: 'fallback' }}" in
  (* 0 should be truthy in Liquid *)
  check string "default with zero" "0" result

let test_default_with_number () =
  let context = Ctx.empty |> Ctx.add "var" (Number 42.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | default: 'fallback' }}" in
  check string "default with number" "42" result

let test_default_allow_false_true () =
  let context = Ctx.empty |> Ctx.add "var" (Bool false) in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{{ var | default: 'fallback', allow_false: true }}"
  in
  (* With allow_false: true, false should not use default *)
  check string "default allow_false with false" "false" result

let test_default_allow_false_with_nil () =
  let context = Ctx.empty |> Ctx.add "var" Nil in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{{ var | default: 'fallback', allow_false: true }}"
  in
  (* Even with allow_false, nil should use default *)
  check string "default allow_false with nil" "fallback" result

(* JSON Filter Tests *)

let test_json_string () =
  let context = Ctx.empty |> Ctx.add "var" (String "hello") in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json string" "\"hello\"" result

let test_json_number () =
  let context = Ctx.empty |> Ctx.add "var" (Number 42.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json number" "42" result

let test_json_boolean_true () =
  let context = Ctx.empty |> Ctx.add "var" (Bool true) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json boolean true" "true" result

let test_json_boolean_false () =
  let context = Ctx.empty |> Ctx.add "var" (Bool false) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json boolean false" "false" result

let test_json_nil () =
  let context = Ctx.empty |> Ctx.add "var" Nil in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json nil" "null" result

let test_json_list () =
  let context =
    Ctx.empty |> Ctx.add "var" (List [ String "a"; String "b"; String "c" ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json list" "[\"a\", \"b\", \"c\"]" result

let test_json_list_mixed () =
  let context =
    Ctx.empty
    |> Ctx.add "var" (List [ String "hello"; Number 42.; Bool true; Nil ])
  in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json list mixed types" "[\"hello\", 42, true, null]" result

let test_json_object () =
  let obj =
    Object.empty
    |> Object.add "name" (String "Alice")
    |> Object.add "age" (Number 30.)
  in
  let context = Ctx.empty |> Ctx.add "var" (Object obj) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  (* JSON object order might vary, so check it contains the right parts *)
  check bool "json object contains name" true
    (String.is_substring result ~substring:"\"name\": \"Alice\"");
  check bool "json object contains age" true
    (String.is_substring result ~substring:"\"age\": 30")

let test_json_nested_object () =
  let inner =
    Object.empty
    |> Object.add "city" (String "NYC")
    |> Object.add "zip" (Number 10001.)
  in
  let outer =
    Object.empty
    |> Object.add "name" (String "Bob")
    |> Object.add "address" (Object inner)
  in
  let context = Ctx.empty |> Ctx.add "var" (Object outer) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check bool "json nested object contains city" true
    (String.is_substring result ~substring:"\"city\": \"NYC\"")

let test_json_empty_list () =
  let context = Ctx.empty |> Ctx.add "var" (List []) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json empty list" "[]" result

let test_json_empty_object () =
  let context = Ctx.empty |> Ctx.add "var" (Object Object.empty) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json }}" in
  check string "json empty object" "{\n\n}" result

(* Combined Filter Tests *)

let test_date_then_default () =
  let context = Ctx.empty |> Ctx.add "date_str" (String "2025-01-15") in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{{ date_str | date: '%Y-%m-%d' | default: 'no date' }}"
  in
  check string "date then default" "2025-01-15" result

let test_default_then_date () =
  let context = Ctx.empty in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{{ undefined_var | default: '2025-01-15' | date: '%Y' }}"
  in
  check string "default then date" "2025" result

let test_json_then_default () =
  let context = Ctx.empty |> Ctx.add "var" (Number 42.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{{ var | json | default: 'empty' }}" in
  check string "json then default" "42" result

(* Test suite *)
let suite =
  ( "Helper Filter Tests"
  , [
      (* Date filter tests *)
      test_case "date default format" `Quick test_date_default_format
    ; test_case "date custom format" `Quick test_date_custom_format
    ; test_case "date year only" `Quick test_date_year_only
    ; test_case "date month only" `Quick test_date_month_only
    ; test_case "date day only" `Quick test_date_day_only
    ; test_case "date full month name" `Quick test_date_full_month_name
    ; test_case "date abbreviated month" `Quick test_date_abbreviated_month
    ; test_case "date weekday name" `Quick test_date_weekday_name
    ; test_case "date abbreviated weekday" `Quick test_date_abbreviated_weekday
    ; test_case "date complex format" `Quick test_date_complex_format
    ; test_case "date now keyword" `Quick test_date_now_keyword
    ; test_case "date now default format" `Quick test_date_now_default_format
      (* Default filter tests *)
    ; test_case "default with value" `Quick test_default_with_value
    ; test_case "default with nil" `Quick test_default_with_nil
    ; test_case "default with undefined" `Quick test_default_with_undefined
    ; test_case "default with empty string" `Quick
        test_default_with_empty_string
    ; test_case "default with false" `Quick test_default_with_false
    ; test_case "default with true" `Quick test_default_with_true
    ; test_case "default with zero" `Quick test_default_with_zero
    ; test_case "default with number" `Quick test_default_with_number
    ; test_case "default allow_false true" `Quick test_default_allow_false_true
    ; test_case "default allow_false with nil" `Quick
        test_default_allow_false_with_nil
      (* JSON filter tests *)
    ; test_case "json string" `Quick test_json_string
    ; test_case "json number" `Quick test_json_number
    ; test_case "json boolean true" `Quick test_json_boolean_true
    ; test_case "json boolean false" `Quick test_json_boolean_false
    ; test_case "json nil" `Quick test_json_nil
    ; test_case "json list" `Quick test_json_list
    ; test_case "json list mixed" `Quick test_json_list_mixed
    ; test_case "json object" `Quick test_json_object
    ; test_case "json nested object" `Quick test_json_nested_object
    ; test_case "json empty list" `Quick test_json_empty_list
    ; test_case "json empty object" `Quick test_json_empty_object
      (* Combined filter tests *)
    ; test_case "date then default" `Quick test_date_then_default
    ; test_case "default then date" `Quick test_default_then_date
    ; test_case "json then default" `Quick test_json_then_default
    ] )
