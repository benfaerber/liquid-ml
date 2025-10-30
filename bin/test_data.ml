open Liquid_ml
open Liquid

let test_collection =
  let product_a =
    Object.empty
    |> Object.add "handle" (String "my-product-a")
    |> Object.add "title" (String "My Product A")
    |> Object.add "status" (String "available")
    |> Object.add "published_at" (Date (Date.now ()))
    |> Object.add "id" (liquid_int 1)
    |> pack_object
  in

  let product_b =
    Object.empty
    |> Object.add "handle" (String "my-product-b")
    |> Object.add "title" (String "My Product B")
    |> Object.add "category" (String "Food")
    |> Object.add "status" (String "available")
    |> Object.add "published_at" (Date (Date.now ()))
    |> Object.add "id" (liquid_int 2)
    |> pack_object
  in

  let product_c =
    Object.empty
    |> Object.add "handle" (String "my-product-c")
    |> Object.add "title" (String "My Product C")
    |> Object.add "category" (String "Toy")
    |> Object.add "status" (String "sold_out")
    |> Object.add "published_at" (Date (Date.now ()))
    |> Object.add "id" (liquid_int 3)
    |> pack_object
  in

  let all_products = [ product_a; product_b; product_c ] in

  Object.empty
  |> Object.add "handle" (String "all-products")
  |> Object.add "title" (String "All Products")
  |> Object.add "products" (List all_products)
  |> pack_object
