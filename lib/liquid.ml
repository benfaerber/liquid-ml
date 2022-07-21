(* This is gonna eventually have settings such as preferred currency, error handling etc *)

let vflog policy func arg =
  match policy with
  | Global.Verbose -> func arg
  | _ -> ()

let mflog policy func arg =
  match policy with
  | Global.Verbose | Global.Minimal -> func arg
  | _ -> ()

let pwrite fname text = File.write ("logs/" ^ fname) text

let vlog policy arg = vflog policy Stdio.print_endline arg
let mlog policy arg = mflog policy Stdio.print_endline arg

let vgroup p title fname func arg =
  let text = func arg in
  vlog p title;
  pwrite fname text;
  vlog p text;
  vflog p Debug.print_line ();
  ()


let render_file filename =
  let interpreter_settings =
    { Global.error_policy = Global.Warn
    ; log_policy = Global.None
    } in

  let s x = x in
  let p = interpreter_settings.log_policy in
  let raw_text = filename |> File.read |> Preprocessor.preprocess in
  vgroup p "Raw Text:" "raw_text.txt" s raw_text;

  let lex_tokens = Lexer.lex raw_text in
  vgroup p "Lex Tokens:" "tokens.txt" Debug.lex_tokens_as_string_with_index lex_tokens;

  let ast = Parser.parse lex_tokens in
  vgroup p "Abstract Syntax Tree:" "ast.txt" Debug.ast_as_string ast;

  let rendered_text = Interpreter.start interpreter_settings ast in
  vgroup p "Render:" "render.txt" s rendered_text;

  rendered_text

let test () =
  (* render_file "liquid/interpreter_test.liquid" |> ignore; *)
  (* render_file "liquid/forloop_vars.liquid" |> ignore; *)
  (* render_file "liquid/render_test.liquid" |> ignore; *)
  (* render_file "liquid/scope_test.liquid" |> ignore; *)
  (* render_file "liquid/number_to_text.liquid" |> ignore; *)
  render_file "liquid/std_test.liquid" |> ignore;
  ()