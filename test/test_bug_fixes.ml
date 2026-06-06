open Alcotest
open Liquid_ml
open Liquid

(* Regression tests for bugs fixed while building a self-hosting compiler.
   See the project that motivated these (liquid-in-liquid). *)

let r t = render_text t

(* --- append / prepend coerce non-string arguments (Shopify behaviour) --- *)

let test_append_number () =
  check string "append number" "n=5" (r "{{ 'n=' | append: 5 }}")

let test_append_chain () =
  check string "append chain"
    "12"
    (r "{%- assign a = '' -%}{%- assign a = a | append: 1 | append: 2 -%}{{ a }}")

let test_prepend_number () =
  check string "prepend number" "7x" (r "{{ 'x' | prepend: 7 }}")

(* --- `for` has no implicit limit (used to silently cap at 50) --- *)

let test_for_no_default_limit () =
  let out = r "{% for i in (1..60) %}.{% endfor %}" in
  check int "for renders all 60 iterations" 60 (String.length out)

(* --- variable ranges --- *)

let test_range_literal () =
  check string "literal range" "12345" (r "{% for i in (1..5) %}{{ i }}{% endfor %}")

let test_range_var_stop () =
  check string "(1..n)" "1234"
    (r "{%- assign n = 4 -%}{% for i in (1..n) %}{{ i }}{% endfor %}")

let test_range_var_both () =
  check string "(a..b)" "3456"
    (r
       "{%- assign a = 3 -%}{%- assign b = 6 -%}{% for i in (a..b) %}{{ i }}{% \
        endfor %}")

let test_range_empty () =
  check string "empty range when hi<lo" "[]"
    (r "{%- assign n = 0 -%}[{% for i in (1..n) %}{{ i }}{% endfor %}]")

(* --- delimiters inside string literals must not break the lexer --- *)

let test_braces_in_string_assign () =
  check string "{{ in a string" "{{x}}"
    (r "{%- assign o = '{{x}}' -%}{{ o }}")

let test_stmt_delims_in_string () =
  check string "{% %} in a string" "[{% if %}]"
    (r "{%- assign o = '{% if %}' -%}[{{ o }}]")

let test_braces_in_expression_literal () =
  check string "braces in expression literal" "{{x}}" (r "{{ '{{x}}' }}")

let test_apostrophe_in_raw_text () =
  check string "apostrophe in raw text" "it's a test x"
    (r "it's a test {{ 'x' }}")

(* --- if/for bodies do not scope assignments (Liquid semantics) --- *)

let test_assign_in_if_persists () =
  check string "assign in if persists" "5"
    (r "{% if true %}{% assign x = 5 %}{% endif %}{{ x }}")

let test_assign_in_else_persists () =
  check string "assign in else persists" "7"
    (r "{% if false %}a{% else %}{% assign y = 7 %}{% endif %}{{ y }}")

let test_assign_in_for_persists () =
  check string "assign in for persists after loop" "3"
    (r "{% for i in (1..3) %}{% assign last = i %}{% endfor %}{{ last }}")

let test_loop_var_not_leaked () =
  check string "loop var does not leak" "[nil]"
    (r "{% for i in (1..3) %}{% endfor %}[{{ i }}]")

let test_break_does_not_leak () =
  check string "break confined to inner loop" "1-1-"
    (r
       "{% for i in (1..2) %}{% for j in (1..3) %}{% if j == 2 %}{% break %}{% \
        endif %}{{ j }}{% endfor %}-{% endfor %}")

let test_nested_forloop_index () =
  check string "nested forloop.index" "12|12|"
    (r
       "{% for i in (1..2) %}{% for j in (1..2) %}{{ forloop.index }}{% endfor \
        %}|{% endfor %}")

let suite =
  ( "Bug Fix Regressions"
  , [
      test_case "append number" `Quick test_append_number
    ; test_case "append chain" `Quick test_append_chain
    ; test_case "prepend number" `Quick test_prepend_number
    ; test_case "for no default limit" `Quick test_for_no_default_limit
    ; test_case "range literal" `Quick test_range_literal
    ; test_case "range (1..n)" `Quick test_range_var_stop
    ; test_case "range (a..b)" `Quick test_range_var_both
    ; test_case "range empty" `Quick test_range_empty
    ; test_case "braces in string assign" `Quick test_braces_in_string_assign
    ; test_case "stmt delims in string" `Quick test_stmt_delims_in_string
    ; test_case "braces in expression literal" `Quick
        test_braces_in_expression_literal
    ; test_case "apostrophe in raw text" `Quick test_apostrophe_in_raw_text
    ; test_case "assign in if persists" `Quick test_assign_in_if_persists
    ; test_case "assign in else persists" `Quick test_assign_in_else_persists
    ; test_case "assign in for persists" `Quick test_assign_in_for_persists
    ; test_case "loop var not leaked" `Quick test_loop_var_not_leaked
    ; test_case "break does not leak" `Quick test_break_does_not_leak
    ; test_case "nested forloop.index" `Quick test_nested_forloop_index
    ] )
