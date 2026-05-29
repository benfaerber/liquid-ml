open Alcotest
open Liquid_ml
open Liquid
open Base

(* Helper to get test templates directory - use absolute path *)
let test_templates_dir =
  let open Stdlib in
  (* Get project root by looking for dune-project *)
  let rec find_root dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then failwith "Could not find project root"
      else find_root parent
  in
  let cwd = Sys.getcwd () in
  let root = find_root cwd in
  Filename.concat (Filename.concat root "test") "templates"

(* Cycle Tag Tests *)

let test_cycle_basic () =
  let settings = Settings.make () in
  let result =
    render_text ~settings
      "{% cycle 'one', 'two', 'three' %}{% cycle 'one', 'two', 'three' %}{% \
       cycle 'one', 'two', 'three' %}"
  in
  check string "cycle basic" "onetwothree" result

let test_cycle_with_group () =
  let settings = Settings.make () in
  let result =
    render_text ~settings
      "{% cycle 'group1': 'a', 'b' %}{% cycle 'group1': 'a', 'b' %}{% cycle \
       'group2': 'x', 'y' %}{% cycle 'group1': 'a', 'b' %}"
  in
  check string "cycle with groups" "abxa" result

let test_cycle_in_loop () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "1"; String "2"; String "3" ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{% cycle 'odd', 'even' %}{% endfor %}"
  in
  check string "cycle in loop" "oddevenodd" result

let test_cycle_multiple_groups_in_loop () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "1"; String "2"; String "3" ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{% cycle 'g1': 'a', 'b' %}-{% cycle 'g2': 'x', \
       'y' %} {% endfor %}"
  in
  check string "cycle multiple groups in loop" "a-x b-y a-x " result

let test_cycle_reset_between_loops () =
  let context =
    Ctx.empty
    |> Ctx.add "items1" (List [ String "1"; String "2" ])
    |> Ctx.add "items2" (List [ String "a"; String "b" ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items1 %}{% cycle 'x', 'y' %}{% endfor %}|{% for item in \
       items2 %}{% cycle 'x', 'y' %}{% endfor %}"
  in
  (* Cycle state persists across loops for the same default group *)
  check string "cycle persists across loops" "xy|xy" result

(* Increment/Decrement Tests *)

let test_increment_basic () =
  let settings = Settings.make () in
  let result =
    render_text ~settings
      "{% increment counter %}{% increment counter %}{% increment counter %}"
  in
  check string "increment basic" "012" result

let test_decrement_basic () =
  let settings = Settings.make () in
  let result =
    render_text ~settings
      "{% decrement counter %}{% decrement counter %}{% decrement counter %}"
  in
  check string "decrement basic" "-1-2-3" result

let test_increment_multiple_counters () =
  let settings = Settings.make () in
  let result =
    render_text ~settings
      "{% increment a %}{% increment b %}{% increment a %}{% increment b %}{% \
       increment a %}"
  in
  (* Each counter starts at 0 independently *)
  check string "increment multiple counters" "00112" result

let test_decrement_multiple_counters () =
  let settings = Settings.make () in
  let result =
    render_text ~settings
      "{% decrement a %}{% decrement b %}{% decrement a %}{% decrement b %}"
  in
  check string "decrement multiple counters" "-1-1-2-2" result

let test_increment_and_decrement_separate () =
  let settings = Settings.make () in
  let result =
    render_text ~settings
      "{% increment counter %}{% decrement counter %}{% increment counter %}"
  in
  (* Increment and decrement use separate counter namespaces *)
  check string "increment and decrement separate" "0-10" result

let test_increment_in_loop () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "a"; String "b"; String "c" ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% for item in items %}{% increment counter %}{% endfor %}"
  in
  check string "increment in loop" "012" result

(* Comparison Operator Tests *)

let test_eq_operator () =
  let context = Ctx.empty |> Ctx.add "x" (Number 5.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% if x == 5 %}yes{% endif %}" in
  check string "eq operator" "yes" result

let test_ne_operator () =
  let context = Ctx.empty |> Ctx.add "x" (Number 5.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% if x != 3 %}yes{% endif %}" in
  check string "ne operator" "yes" result

let test_gt_operator () =
  let context = Ctx.empty |> Ctx.add "x" (Number 10.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% if x > 5 %}yes{% endif %}" in
  check string "gt operator" "yes" result

let test_gte_operator () =
  let context = Ctx.empty |> Ctx.add "x" (Number 5.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% if x >= 5 %}yes{% endif %}" in
  check string "gte operator" "yes" result

let test_lt_operator () =
  let context = Ctx.empty |> Ctx.add "x" (Number 3.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% if x < 5 %}yes{% endif %}" in
  check string "lt operator" "yes" result

let test_lte_operator () =
  let context = Ctx.empty |> Ctx.add "x" (Number 5.) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% if x <= 5 %}yes{% endif %}" in
  check string "lte operator" "yes" result

let test_contains_operator_string () =
  let context = Ctx.empty |> Ctx.add "text" (String "hello world") in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{% if text contains 'world' %}yes{% endif %}"
  in
  check string "contains operator string" "yes" result

let test_contains_operator_list () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "a"; String "b"; String "c" ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{% if items contains 'b' %}yes{% endif %}"
  in
  check string "contains operator list" "yes" result

(* Complex Condition Tests *)

let test_and_operator () =
  let context =
    Ctx.empty |> Ctx.add "x" (Number 5.) |> Ctx.add "y" (Number 10.)
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{% if x == 5 and y == 10 %}yes{% endif %}"
  in
  check string "and operator" "yes" result

let test_or_operator () =
  let context = Ctx.empty |> Ctx.add "x" (Number 5.) in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings "{% if x == 3 or x == 5 %}yes{% endif %}"
  in
  check string "or operator" "yes" result

let test_not_with_condition () =
  let context = Ctx.empty |> Ctx.add "flag" (Bool false) in
  let settings = Settings.make ~context () in
  let result = render_text ~settings "{% unless flag %}yes{% endunless %}" in
  check string "unless (not) operator" "yes" result

let test_complex_and_or () =
  let context =
    Ctx.empty |> Ctx.add "a" (Number 5.) |> Ctx.add "b" (Number 10.)
    |> Ctx.add "c" (Number 15.)
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% if (a == 5 and b == 10) or c == 20 %}yes{% endif %}"
  in
  check string "complex and/or" "yes" result

let test_nested_conditions () =
  let context =
    Ctx.empty |> Ctx.add "x" (Number 5.) |> Ctx.add "y" (Number 10.)
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% if x > 0 %}{% if y > 5 %}both{% endif %}{% endif %}"
  in
  check string "nested conditions" "both" result

(* Advanced Capture Tests *)

let test_capture_with_filters () =
  let context = Ctx.empty |> Ctx.add "name" (String "john") in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% capture greeting %}Hello {{ name | upcase }}{% endcapture %}{{ \
       greeting }}"
  in
  check string "capture with filters" "Hello JOHN" result

let test_capture_with_for_loop () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "a"; String "b"; String "c" ])
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% capture list %}{% for item in items %}{{ item }}-{% endfor %}{% \
       endcapture %}{{ list }}"
  in
  check string "capture with for loop" "a-b-c-" result

let test_capture_reuse () =
  let settings = Settings.make () in
  let result =
    render_text ~settings
      "{% capture var %}first{% endcapture %}{{ var }}{% capture var \
       %}second{% endcapture %}{{ var }}"
  in
  check string "capture reuse" "firstsecond" result

let test_capture_with_expressions () =
  let context =
    Ctx.empty |> Ctx.add "x" (Number 5.) |> Ctx.add "y" (Number 3.)
  in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% capture sum %}{{ x | plus: y }}{% endcapture %}Result: {{ sum }}"
  in
  check string "capture with expressions" "Result: 8" result

let test_capture_empty () =
  let settings = Settings.make () in
  let result =
    render_text ~settings "{% capture empty %}{% endcapture %}[{{ empty }}]"
  in
  check string "capture empty" "[]" result

(* Case/When Tests *)

let test_case_simple () =
  let context = Ctx.empty |> Ctx.add "x" (Number 2.) in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% case x %}{% when 1 %}one{% when 2 %}two{% when 3 %}three{% endcase %}"
  in
  check string "case simple" "two" result

let test_case_with_else () =
  let context = Ctx.empty |> Ctx.add "x" (Number 5.) in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% case x %}{% when 1 %}one{% when 2 %}two{% else %}other{% endcase %}"
  in
  check string "case with else" "other" result

let test_case_multiple_values () =
  let context = Ctx.empty |> Ctx.add "x" (Number 2.) in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% case x %}{% when 1, 2, 3 %}low{% when 4, 5, 6 %}high{% endcase %}"
  in
  check string "case multiple values" "low" result

let test_case_with_strings () =
  let context = Ctx.empty |> Ctx.add "color" (String "red") in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% case color %}{% when 'red' %}stop{% when 'yellow' %}slow{% when \
       'green' %}go{% endcase %}"
  in
  check string "case with strings" "stop" result

(* Include Tests *)

let test_include_basic () =
  let context = Ctx.empty |> Ctx.add "name" (String "World") in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render_text ~settings "{% include 'greeting' %}" in
  check string "include basic" "Hello World!" result

let test_include_with_context_variables () =
  let context = Ctx.empty |> Ctx.add "title" (String "Welcome") in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render_text ~settings "{% include 'header' %}" in
  check string "include with context variables" "<header>Welcome</header>"
    result

let test_include_with_loop () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "a"; String "b"; String "c" ])
  in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render_text ~settings "{% include 'list' %}" in
  check string "include with loop" "<ul><li>a</li><li>b</li><li>c</li></ul>"
    result

let test_include_nested () =
  let context =
    Ctx.empty
    |> Ctx.add "title" (String "Header")
    |> Ctx.add "content" (String "Body text")
  in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render_text ~settings "{% include 'page' %}" in
  check string "include nested" "<header>Header</header><main>Body text</main>"
    result

(* Render Tests *)

let test_render_basic () =
  let context = Ctx.empty |> Ctx.add "name" (String "Alice") in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render_text ~settings "{% render 'greeting' %}" in
  (* Render uses isolated context, so outer 'name' is not available, shows as nil *)
  check string "render basic" "Hello nil!" result

let test_render_with_variable () =
  let context = Ctx.empty in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result =
    render_text ~settings "{% render 'greeting' with name: 'Bob' %}"
  in
  check string "render with variable" "Hello Bob!" result

let test_render_with_object () =
  let product =
    Object.empty
    |> Object.add "name" (String "Widget")
    |> Object.add "price" (Number 19.99)
  in
  let context = Ctx.empty |> Ctx.add "item" (Object product) in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result =
    render_text ~settings "{% render 'product' with product: item %}"
  in
  check string "render with object" "Product: Widget - $19.99" result

let test_render_isolated_context () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "x"; String "y" ])
  in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  (* items is in outer context but not passed to render, so render should see empty *)
  let result = render_text ~settings "{% render 'list' %}" in
  check string "render isolated context" "<ul></ul>" result

(* Section Tests *)

let test_section_basic () =
  let context = Ctx.empty |> Ctx.add "year" (Number 2025.) in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render_text ~settings "{% section 'footer' %}" in
  (* Section uses isolated context, so outer 'year' is not available *)
  check string "section basic" "<footer>Copyright nil</footer>" result

(* Style Tests *)

let test_style_basic () =
  let settings = Settings.make () in
  let result =
    render_text ~settings "{% style %}body { margin: 0; }{% endstyle %}"
  in
  check string "style basic" "<style data-liquid>body { margin: 0; }</style>"
    result

let test_style_with_liquid () =
  let context = Ctx.empty |> Ctx.add "color" (String "red") in
  let settings = Settings.make ~context () in
  let result =
    render_text ~settings
      "{% style %}.box { color: {{ color }}; }{% endstyle %}"
  in
  check string "style with liquid"
    "<style data-liquid>.box { color: red; }</style>" result

let test_style_empty () =
  let settings = Settings.make () in
  let result = render_text ~settings "{% style %}{% endstyle %}" in
  check string "style empty" "<style data-liquid></style>" result

(* Liquid tag tests *)

let test_liquid_tag_no_space () =
  let template =
    "{%liquid\nassign x = \"hello\"\necho x\n%}after"
  in
  let result = render_text template in
  check string "{%liquid (no space)" "\nhello\nafter" result

let test_liquid_tag_with_space () =
  let template =
    "{% liquid\nassign x = \"hello\"\necho x\n%}after"
  in
  let result = render_text template in
  check string "{% liquid (with space)" "\nhello\nafter" result

let test_liquid_tag_with_multiple_spaces () =
  let template =
    "{%   liquid\nassign x = \"hello\"\necho x\n%}after"
  in
  let result = render_text template in
  check string "{%   liquid (multiple spaces)" "\nhello\nafter" result

let test_liquid_tag_with_trim_and_space () =
  let template =
    "prefix {%- liquid\nassign x = \"hello\"\necho x\n-%} after"
  in
  let result = render_text template in
  check string "{%- liquid (trim + space)" "prefix\nhello\nafter" result

(* Whitespace Control Tests *)

let test_ws_trim_right_basic () =
  let result = render_text "before {{ \"x\" -}}   after" in
  check string "-}} trims right" "before xafter" result

let test_ws_trim_left_basic () =
  let result = render_text "before   {{- \"x\" }} after" in
  check string "{{- trims left" "beforex after" result

let test_ws_trim_both_basic () =
  let result = render_text "before   {{- \"x\" -}}   after" in
  check string "{{- ... -}} trims both" "beforexafter" result

let test_ws_no_trim_keeps_whitespace () =
  let result = render_text "before   {{ \"x\" }}   after" in
  check string "no trim preserves whitespace" "before   x   after" result

let test_ws_statement_trim_strips_newlines () =
  let template =
    "a\n\n\n{%- assign x = 1 -%}\n\n\nb"
  in
  let result = render_text template in
  check string "{%- -%} strips newlines" "ab" result

let test_ws_consecutive_trim_tags () =
  let template = "{%- if true -%}A{%- else -%}B{%- endif -%}" in
  let result = render_text template in
  check string "consecutive trim tags" "A" result

let test_ws_only_trims_whitespace () =
  (* Trim should only strip whitespace, not other characters. *)
  let result = render_text "xx{%- assign a = 1 -%}yy" in
  check string "{%- -%} doesn't eat non-whitespace" "xxyy" result

(* Real-world Liquid templates: load .liquid files from disk and verify
   the rendered output is what Shopify-style whitespace control would
   produce. These also serve as smoke tests that the templates parse. *)

let test_template_issue_11 () =
  let settings =
    Settings.make ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_issue11.liquid" in
  check string "issue #11 template" "Hello world bar\n" result

let test_template_blog_published_with_subtitle () =
  let post =
    Object.empty
    |> Object.add "published" (Bool true)
    |> Object.add "title" (String "First Post")
    |> Object.add "subtitle" (String "An intro")
    |> Object.add "body" (String "Hello, world!")
  in
  let context = Ctx.empty |> Ctx.add "post" (Object post) in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_blog.liquid" in
  check string "blog published with subtitle"
    "<article>\n\
    \  <h1>First Post</h1>\n\
    \  <h2>An intro</h2>\n\
    \  <div class=\"body\">Hello, world!</div>\n\
     </article>"
    result

let test_template_blog_published_without_subtitle () =
  (* Missing `subtitle` field resolves to Nil which is falsy, so the
     subtitle block is omitted entirely. *)
  let post =
    Object.empty
    |> Object.add "published" (Bool true)
    |> Object.add "title" (String "Second")
    |> Object.add "body" (String "Body")
  in
  let context = Ctx.empty |> Ctx.add "post" (Object post) in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_blog.liquid" in
  check string "blog published without subtitle"
    "<article>\n\
    \  <h1>Second</h1>\n\
    \  <div class=\"body\">Body</div>\n\
     </article>"
    result

let test_template_blog_draft () =
  let post =
    Object.empty
    |> Object.add "published" (Bool false)
    |> Object.add "title" (String "WIP")
  in
  let context = Ctx.empty |> Ctx.add "post" (Object post) in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_blog.liquid" in
  check string "blog draft" "<p>Draft: WIP</p>" result

let test_template_nav_menu () =
  let mk_link url label active =
    Object
      (Object.empty
      |> Object.add "url" (String url)
      |> Object.add "label" (String label)
      |> Object.add "active" (Bool active))
  in
  let context =
    Ctx.empty
    |> Ctx.add "links"
         (List
            [
              mk_link "/" "Home" true
            ; mk_link "/about" "About" false
            ; mk_link "/contact" "Contact" false
            ])
  in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_nav.liquid" in
  check string "nav menu"
    "<nav>\n\
    \    <a class=\"active\" href=\"/\">Home</a>\n\
    \    <a href=\"/about\">About</a>\n\
    \    <a href=\"/contact\">Contact</a>\n\
     </nav>\n"
    result

let test_template_csv_multi () =
  let context =
    Ctx.empty
    |> Ctx.add "items" (List [ String "a"; String "b"; String "c" ])
  in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_csv.liquid" in
  check string "csv multi" "a,b,c" result

let test_template_csv_single () =
  let context =
    Ctx.empty |> Ctx.add "items" (List [ String "only" ])
  in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_csv.liquid" in
  check string "csv single" "only" result

let test_template_invoice_vip () =
  let mk_item name price =
    Object
      (Object.empty
      |> Object.add "name" (String name)
      |> Object.add "price" (Number price))
  in
  let customer =
    Object.empty
    |> Object.add "name" (String "Alice")
    |> Object.add "vip" (Bool true)
  in
  let context =
    Ctx.empty
    |> Ctx.add "customer" (Object customer)
    |> Ctx.add "items"
         (List [ mk_item "Widget" 10.0; mk_item "Gizmo" 25.0 ])
    |> Ctx.add "total" (Number 35.0)
  in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_invoice.liquid" in
  check string "invoice VIP"
    "Invoice for Alice (VIP)\n\n\
     Items:\n\
     - Widget: $10\n\
     - Gizmo: $25\n\n\
     Total: $35\n"
    result

let test_template_invoice_regular () =
  let mk_item name price =
    Object
      (Object.empty
      |> Object.add "name" (String name)
      |> Object.add "price" (Number price))
  in
  let customer =
    Object.empty
    |> Object.add "name" (String "Bob")
    |> Object.add "vip" (Bool false)
  in
  let context =
    Ctx.empty
    |> Ctx.add "customer" (Object customer)
    |> Ctx.add "items" (List [ mk_item "Thing" 5.0 ])
    |> Ctx.add "total" (Number 5.0)
  in
  let settings =
    Settings.make ~context ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_invoice.liquid" in
  check string "invoice regular"
    "Invoice for Bob\n\n\
     Items:\n\
     - Thing: $5\n\n\
     Total: $5\n"
    result

let test_template_inline_trim () =
  let settings =
    Settings.make ~template_directory:test_templates_dir ()
  in
  let result = render ~settings "whitespace_inline.liquid" in
  check string "inline {{- -}} trim" "A B C\nD  E  FG1H\n" result

(* Test suite *)
let suite =
  ( "Interpreter Tests"
  , [
      (* Cycle tests *)
      test_case "cycle basic" `Quick test_cycle_basic
    ; test_case "cycle with group" `Quick test_cycle_with_group
    ; test_case "cycle in loop" `Quick test_cycle_in_loop
    ; test_case "cycle multiple groups in loop" `Quick
        test_cycle_multiple_groups_in_loop
    ; test_case "cycle reset between loops" `Quick
        test_cycle_reset_between_loops
      (* Increment/Decrement tests *)
    ; test_case "increment basic" `Quick test_increment_basic
    ; test_case "decrement basic" `Quick test_decrement_basic
    ; test_case "increment multiple counters" `Quick
        test_increment_multiple_counters
    ; test_case "decrement multiple counters" `Quick
        test_decrement_multiple_counters
    ; test_case "increment and decrement separate" `Quick
        test_increment_and_decrement_separate
    ; test_case "increment in loop" `Quick test_increment_in_loop
      (* Comparison operator tests *)
    ; test_case "eq operator" `Quick test_eq_operator
    ; test_case "ne operator" `Quick test_ne_operator
    ; test_case "gt operator" `Quick test_gt_operator
    ; test_case "gte operator" `Quick test_gte_operator
    ; test_case "lt operator" `Quick test_lt_operator
    ; test_case "lte operator" `Quick test_lte_operator
    ; test_case "contains operator string" `Quick test_contains_operator_string
    ; test_case "contains operator list" `Quick test_contains_operator_list
      (* Complex condition tests *)
    ; test_case "and operator" `Quick test_and_operator
    ; test_case "or operator" `Quick test_or_operator
    ; test_case "unless (not) operator" `Quick test_not_with_condition
    ; test_case "complex and/or" `Quick test_complex_and_or
    ; test_case "nested conditions" `Quick test_nested_conditions
      (* Advanced capture tests *)
    ; test_case "capture with filters" `Quick test_capture_with_filters
    ; test_case "capture with for loop" `Quick test_capture_with_for_loop
    ; test_case "capture reuse" `Quick test_capture_reuse
    ; test_case "capture with expressions" `Quick test_capture_with_expressions
    ; test_case "capture empty" `Quick test_capture_empty (* Case/when tests *)
    ; test_case "case simple" `Quick test_case_simple
    ; test_case "case with else" `Quick test_case_with_else
    ; test_case "case multiple values" `Quick test_case_multiple_values
    ; test_case "case with strings" `Quick test_case_with_strings
      (* Include tests *)
    ; test_case "include basic" `Quick test_include_basic
    ; test_case "include with context variables" `Quick
        test_include_with_context_variables
    ; test_case "include with loop" `Quick test_include_with_loop
    ; test_case "include nested" `Quick test_include_nested (* Render tests *)
    ; test_case "render basic" `Quick test_render_basic
    ; test_case "render with variable" `Quick test_render_with_variable
    ; test_case "render with object" `Quick test_render_with_object
    ; test_case "render isolated context" `Quick test_render_isolated_context
      (* Section tests *)
    ; test_case "section basic" `Quick test_section_basic (* Style tests *)
    ; test_case "style basic" `Quick test_style_basic
    ; test_case "style with liquid" `Quick test_style_with_liquid
    ; test_case "style empty" `Quick test_style_empty
      (* Liquid tag tests *)
    ; test_case "{%liquid (no space)" `Quick test_liquid_tag_no_space
    ; test_case "{% liquid (with space)" `Quick test_liquid_tag_with_space
    ; test_case "{%   liquid (multiple spaces)" `Quick
        test_liquid_tag_with_multiple_spaces
    ; test_case "{%- liquid (trim + space)" `Quick
        test_liquid_tag_with_trim_and_space
      (* Whitespace control tests *)
    ; test_case "-}} trims right" `Quick test_ws_trim_right_basic
    ; test_case "{{- trims left" `Quick test_ws_trim_left_basic
    ; test_case "{{- -}} trims both" `Quick test_ws_trim_both_basic
    ; test_case "no trim preserves whitespace" `Quick
        test_ws_no_trim_keeps_whitespace
    ; test_case "{%- -%} strips newlines" `Quick
        test_ws_statement_trim_strips_newlines
    ; test_case "consecutive trim tags" `Quick test_ws_consecutive_trim_tags
    ; test_case "trim only strips whitespace" `Quick
        test_ws_only_trims_whitespace
      (* Real-world template tests *)
    ; test_case "template: issue #11" `Quick test_template_issue_11
    ; test_case "template: blog published w/ subtitle" `Quick
        test_template_blog_published_with_subtitle
    ; test_case "template: blog published w/o subtitle" `Quick
        test_template_blog_published_without_subtitle
    ; test_case "template: blog draft" `Quick test_template_blog_draft
    ; test_case "template: nav menu" `Quick test_template_nav_menu
    ; test_case "template: csv multi" `Quick test_template_csv_multi
    ; test_case "template: csv single" `Quick test_template_csv_single
    ; test_case "template: invoice VIP" `Quick test_template_invoice_vip
    ; test_case "template: invoice regular" `Quick test_template_invoice_regular
    ; test_case "template: inline trim" `Quick test_template_inline_trim
    ] )
