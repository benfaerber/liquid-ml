let (~/) = Re2.create_exn

let tag name = "{%-? ?" ^ name ^ " ?-?%}"

let remove exp text = Re2.rewrite_exn exp text ~template:""

let remove_comments inp =
  let tag_comment = ~/(tag "comment" ^ "(.|\n)+?" ^ tag "endcomment") in
  let hash_comment = ~/("{%\\s+?((\\s|\\s+)?#.+)+\\s+?%}") in
  remove tag_comment inp
  |> remove hash_comment

let remove_liquid_comments inp =
  let exp = ~/"^comment(.|\n)+^endcomment" in
  remove exp inp

let add_eof text = text ^ "  "

let preprocess text = text |> remove_comments |> add_eof

let raw_open = tag "raw"
let raw_close = tag "endraw"

let is_raw text =
  let exp = ~/("^" ^ raw_open ^ "(.|\n)+") in
  Re2.matches exp text

let until_end_raw text =
  let exp = ~/("(.|\n)+" ^ raw_close) in
  Re2.find_first_exn exp text

let trim_raw_tags text =
  let start_exp = ~/("^" ^ raw_open) in
  let end_exp = ~/(raw_close ^ "$") in
  remove start_exp text
  |> remove end_exp
