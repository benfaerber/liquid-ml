open Liquid_ml
open Liquid

let test_collection =
  let product_a = Obj.empty
    |> Obj.add "handle" (String "my-product-a")
    |> Obj.add "title" (String "My Product A")
    |> Obj.add "status" (String "available")
    |> Obj.add "published_at" (Date (Date.now ()))
    |> Obj.add "id" (liquid_int 1)
    |> pack_object
  in

  let product_b = Obj.empty
    |> Obj.add "handle" (String "my-product-b")
    |> Obj.add "title" (String "My Product B")
    |> Obj.add "category" (String "Food")
    |> Obj.add "status" (String "available")
    |> Obj.add "published_at" (Date (Date.now ()))
    |> Obj.add "id" (liquid_int 2)
    |> pack_object
  in

  let product_c = Obj.empty
    |> Obj.add "handle" (String "my-product-c")
    |> Obj.add "title" (String "My Product C")
    |> Obj.add "category" (String "Toy")
    |> Obj.add "status" (String "sold_out")
    |> Obj.add "published_at" (Date (Date.now ()))
    |> Obj.add "id" (liquid_int 3)
    |> pack_object
  in

  let all_products = [product_a; product_b; product_c] in

  Object (
    Obj.empty
    |> Obj.add "handle" (String "all-products")
    |> Obj.add "title" (String "All Products")
    |> Obj.add "products" (List all_products)
  )