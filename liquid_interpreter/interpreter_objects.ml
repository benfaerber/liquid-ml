open Base
open Liquid_syntax
open Syntax
open Values

let make_forloop_ctx ctx index length =
  let forloop_obj = Object (
    Object.empty
    |> Object.add "index" (num_int (index + 1))
    |> Object.add "length" (num_int length)
    |> Object.add "first" (Bool (index = 0))
    |> Object.add "index0" (num_int index)
    |> Object.add "last" (Bool (index = length - 1))
    |> Object.add "rindex" (num_int (length - index))
    |> Object.add "rindex0" (num_int (length - index - 1))
  ) in

  ctx
  |> Ctx.add Settings.forloop forloop_obj


let request () =
  let request_obj = Object (
    Object.empty
    |> Object.add "design_mode" (Bool false)
    |> Object.add "host" (String "")
    |> Object.add "origin" (String "")
    |> Object.add "page_type" (String "")
    |> Object.add "path" (String "")
  ) in

  request_obj