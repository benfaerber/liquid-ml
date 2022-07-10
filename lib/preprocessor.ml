let (~/) = Re2.create_exn

let remove_comments inp =
  let exp = ~/"{%-? ?comment ?-?%}(.|\n)+{%-? ?endcomment ?-?%}" in
  Re2.rewrite_exn exp ~template:"" inp

let remove_liquid_comments inp =
  let exp = ~/"^comment(.|\n)+^endcomment" in
  Re2.rewrite_exn exp ~template:"" inp

let add_eof text = text ^ "  "

let preprocess text = text |> remove_comments |> add_eof