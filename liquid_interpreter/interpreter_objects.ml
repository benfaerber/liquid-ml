open Base
open Liquid_syntax
open Syntax
open Values

let make_forloop_ctx ctx index length =
  let forloop_obj = Object (
    Obj.empty
    |> Obj.add "index" (num_int (index + 1))
    |> Obj.add "length" (num_int length)
    |> Obj.add "first" (Bool (index = 0))
    |> Obj.add "index0" (num_int index)
    |> Obj.add "last" (Bool (index = length - 1))
    |> Obj.add "rindex" (num_int (length - index))
    |> Obj.add "rindex0" (num_int (length - index - 1))
  ) in

  ctx
  |> Ctx.add Settings.forloop forloop_obj


let request () =
  let request_obj = Object (
    Obj.empty
    |> Obj.add "design_mode" (Bool false)
    |> Obj.add "host" (String "")
    |> Obj.add "origin" (String "")
    |> Obj.add "page_type" (String "")
    |> Obj.add "path" (String "")
  ) in

  request_obj