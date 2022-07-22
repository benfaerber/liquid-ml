open Liquid_ml

let example_1 =
  Liquid.render "liquid_templates/block_test.liquid" () |> Stdio.print_endline

let () = example_1