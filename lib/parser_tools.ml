let scan_until_eos tokens =
  let rec aux acc = function
    | hd :: tl when hd = Keyword.EOS -> acc, tl
    | hd :: tl -> aux (acc @ [hd]) tl
    | [] -> acc, []
  in aux [] tokens