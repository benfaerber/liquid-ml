open Base
open Liquid_syntax
open Syntax
open Values

let make_forloop_ctx ctx index length =
  let forloop_obj = make_obj
  [ p "index" (num_int (index + 1))
  ; p "length" (num_int length)
  ; p "first" (Bool (index = 0))
  ; p "index0" (num_int index)
  ; p "last" (Bool (index = length - 1))
  ; p "rindex" (num_int (length - index))
  ; p "rindex0" (num_int (length - index - 1))
  ] in

  ctx
  |> Ctx.add Settings.forloop forloop_obj


let request () =
  let request_obj = make_obj
  [ p "design_mode" (Bool false)
  ; p "host" (String "")
  ; p "origin" (String "")
  ; p "page_type" (String "")
  ; p "path" (String "")
  ] in

  request_obj