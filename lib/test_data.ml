open Syntax

let test_collection =
  let product_a = Object (
    Obj.empty
    |> Obj.add "handle" (String "my-product-a")
    |> Obj.add "title" (String "My Product A")
    |> Obj.add "id" (Number 1.)
  ) in

  let product_b = Object (
    Obj.empty
    |> Obj.add "handle" (String "my-product-b")
    |> Obj.add "title" (String "My Product B")
    |> Obj.add "id" (Number 2.)
  ) in

  let product_c = Object (
    Obj.empty
    |> Obj.add "handle" (String "my-product-c")
    |> Obj.add "title" (String "My Product C")
    |> Obj.add "id" (Number 3.)
  ) in

  let all_products = [product_a; product_b; product_c] in

  Object (
    Obj.empty
    |> Obj.add "handle" (String "all-products")
    |> Obj.add "title" (String "All Products")
    |> Obj.add "products" (List all_products)
  )