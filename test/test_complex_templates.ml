open Alcotest
open Liquid_ml
open Liquid

(* Real-world complex templates from Shopify's Ruby Liquid benchmark suite
   (performance/tests/{tribble,vogue,dropify,ripen}). These exercise the
   renderer end-to-end: nested for-loops, cycle, forloop.first/last, nested
   object access, hyphenated keys (linklists.main-menu), list .size/.first,
   money/pluralize/truncate filters, and conditionals.

   The Shopify-specific filters (asset_url, product_img_url, link_to, within,
   stylesheet_tag, ...) are not implemented by liquid-ml, so we register a
   universal passthrough stub for them. liquid-ml consults the standard
   library first, so real filters (money, pluralize, ...) keep their behavior;
   only unknown names hit the stub. *)

let stub _ params = match params with v :: _ -> Ok v | [] -> Ok (String "")
let filters _ = Some stub

(* Locate test/templates/complex relative to the project root. *)
let complex_dir =
  let rec find_root dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then failwith "Could not find project root"
      else find_root parent
  in
  let root = find_root (Sys.getcwd ()) in
  List.fold_left Filename.concat root [ "test"; "templates"; "complex" ]

(* substring check without pulling in Base *)
let contains_sub s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec aux i =
    i + lsub <= ls && (String.equal (String.sub s i lsub) sub || aux (i + 1))
  in
  lsub = 0 || aux 0

(* --- fixture builders --- *)

let obj kvs =
  Object (List.fold_left (fun acc (k, v) -> Object.add k v acc) Object.empty kvs)

let variant id title price =
  obj [ ("id", Number id); ("title", String title); ("price", Number price) ]

let product ?(available = true) title price =
  obj
    [ ("title", String title)
    ; ("url", String ("/products/" ^ title))
    ; ("description", String (title ^ " is a wonderful product you will love."))
    ; ("featured_image", String "/img/feat.png")
    ; ("vendor", String "Acme")
    ; ("type", String "Gadget")
    ; ("available", Bool available)
    ; ("price", Number price)
    ; ("price_min", Number price)
    ; ("price_max", Number (price +. 5.))
    ; ("price_varies", Bool true)
    ; ("compare_at_price", Number (price +. 10.))
    ; ("images", List [ String "/img/a.png"; String "/img/b.png" ])
    ; ( "variants"
      , List [ variant 1. "Small" price; variant 2. "Large" (price +. 2.) ] )
    ; ("tags", List [ String "new"; String "sale" ])
    ]

let link title url = obj [ ("title", String title); ("url", String url) ]
let page content = obj [ ("content", String content) ]

let products =
  List
    [ product "Awesome Shirt" 25.
    ; product "Cool Hat" 15.
    ; product ~available:false "Sold Pants" 40.
    ]

let context () =
  let shop =
    obj
      [ ("name", String "Tribble Store")
      ; ("currency", String "USD")
      ; ("url", String "https://tribble.example")
      ; ("money_with_currency_format", String "${{amount}} USD")
      ]
  in
  let cart_item title qty price =
    obj
      [ ("title", String title)
      ; ("quantity", Number qty)
      ; ("line_price", Number price)
      ; ("variant", variant 1. title price)
      ; ( "product"
        , obj
            [ ("url", String "/p")
            ; ("featured_image", String "/i.png")
            ; ("title", String title)
            ] )
      ]
  in
  let cart =
    obj
      [ ("item_count", Number 2.)
      ; ("total_price", Number 60.)
      ; ( "items"
        , List [ cart_item "Awesome Shirt" 1. 25.; cart_item "Cool Hat" 1. 15. ]
        )
      ]
  in
  let collections = obj [ ("frontpage", obj [ ("products", products) ]) ] in
  let collection =
    obj
      [ ("title", String "Summer")
      ; ("tags", List [ String "hot"; String "fresh" ])
      ; ("products", products)
      ]
  in
  let linklists =
    obj
      [ ( "main-menu"
        , obj [ ("links", List [ link "Home" "/"; link "About" "/about" ]) ] )
      ; ( "footer"
        , obj
            [ ("links", List [ link "Privacy" "/privacy"; link "Terms" "/terms" ])
            ] )
      ]
  in
  let pages =
    obj
      [ ("alert", page "Big sale today!")
      ; ("about-us", page "We are great.")
      ; ("shopping-cart", page "Cart info.")
      ]
  in
  let articles =
    List
      [ obj
          [ ("url", String "/a1")
          ; ("title", String "News One")
          ; ("content", String "First story body.")
          ]
      ; obj
          [ ("url", String "/a2")
          ; ("title", String "News Two")
          ; ("content", String "Second story body.")
          ]
      ]
  in
  let blogs = obj [ ("news", obj [ ("articles", articles) ]) ] in
  Ctx.empty
  |> Ctx.add "shop" shop
  |> Ctx.add "cart" cart
  |> Ctx.add "collections" collections
  |> Ctx.add "collection" collection
  |> Ctx.add "linklists" linklists
  |> Ctx.add "pages" pages
  |> Ctx.add "blogs" blogs
  |> Ctx.add "product" (product "Awesome Shirt" 25.)
  |> Ctx.add "page_title" (String "Home Page")
  |> Ctx.add "template" (String "index")
  |> Ctx.add "current_tags" (List [])
  |> Ctx.add "additional_checkout_buttons" (Bool true)
  |> Ctx.add "content_for_additional_checkout_buttons" (String "[paypal]")

let render_complex file =
  let settings =
    Settings.make ~error_policy:Silent ~filters ~context:(context ())
      ~template_directory:complex_dir ()
  in
  render ~settings file

(* Assert the render produced each expected marker substring. *)
let check_markers file markers =
  let out = render_complex file in
  check bool (file ^ ": non-empty") true (String.length out > 0);
  List.iter
    (fun m ->
      check bool (Printf.sprintf "%s: contains %S" file m) true
        (contains_sub out m))
    markers

(* --- tests --- *)

let test_theme () =
  check_markers "theme.liquid"
    [ "Tribble Store &mdash; Home Page"
    ; "Featured Products"
    ; "Awesome Shirt"
    ; "More news"
    ; "News Two" (* "News One" is skipped by `offset: 1` in the loop *)
    ; "About"
    ; "All Rights Reserved"
    ]

let test_minicart_theme () =
  check_markers "minicart_theme.liquid"
    [ "Tribble Store"; "All prices are in USD"; "Home"; "your cart" ]

let test_index () =
  check_markers "index.liquid"
    [ "Three Great Reasons"; "Awesome Shirt"; "Cool Hat"; "Add to Basket" ]

let test_product () =
  check_markers "product.liquid"
    [ "Summer Awesome Shirt"; "Product Options:"; "Small"; "Large"; "new" ]

let test_product_variants () =
  check_markers "product_variants.liquid"
    [ "Awesome Shirt"; "Vendor:"; "Acme"; "Small"; "Add to Cart" ]

let test_cart () =
  check_markers "cart.liquid"
    [ "Your Cart"
    ; "2 items"
    ; "$60.00 USD"
    ; "Awesome Shirt"
    ; "Other Products You Might Enjoy"
    ]

let suite =
  ( "Complex Template Tests"
  , [ test_case "vogue/theme" `Quick test_theme
    ; test_case "dropify/minicart theme" `Quick test_minicart_theme
    ; test_case "tribble/index" `Quick test_index
    ; test_case "tribble/product" `Quick test_product
    ; test_case "ripen/product variants" `Quick test_product_variants
    ; test_case "tribble/cart" `Quick test_cart
    ] )
