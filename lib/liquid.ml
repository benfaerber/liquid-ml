(* This is gonna eventually have settings such as preferred currency, error handling etc *)
let render_file filename =
  Interpreter.interpret_from_file filename