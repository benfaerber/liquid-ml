open Liquid_syntax
open Liquid_parser
open Liquid_interpreter
open Syntax
open Values

let vflog policy func arg =
  match policy with
  | Settings.Verbose -> func arg
  | _ -> ()

let mflog policy func arg =
  match policy with
  | Settings.Verbose | Settings.Minimal -> func arg
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

let default_settings = Settings.make ()

let render_file filename ?(settings = default_settings) () =
  let s x = x in
  let p = settings.log_policy in
  let raw_text = filename |> File.read |> Preprocessor.preprocess in
  vgroup p "Raw Text:" "raw_text.txt" s raw_text;

  let lex_tokens = Lexer.lex raw_text in
  vgroup p "Lex Tokens:" "tokens.txt" Debug.lex_tokens_as_string_with_index lex_tokens;

  let ast = Parser.parse lex_tokens in
  vgroup p "Abstract Syntax Tree:" "ast.txt" Debug.ast_as_string ast;

  let rendered_text = Interpreter.start settings ast in
  vgroup p "Render:" "render.txt" s rendered_text;

  rendered_text

let test () =
  let greet ctx params =
    match unwrap_all ctx params with
    | String person :: _ ->
      Ok (String ("Hello " ^ person ^ "!"))
    | _ -> Error "greet accepts a string"
  in

  let custom_filters = function
    | "greet" -> Some greet
    | _ -> None
  in

  let settings = Settings.make ~error_policy:Warn ~filters:custom_filters () in
  render_file "liquid_templates/std_test.liquid" ~settings () |> ignore;

  (* render_file "liquid_templates/interpreter_test.liquid" |> ignore; *)
  (* render_file "liquid_templates/forloop_vars.liquid" |> ignore; *)
  (* render_file "liquid_templates/render_test.liquid" |> ignore; *)
  (* render_file "liquid_templates/scope_test.liquid" |> ignore; *)
  (* render_file "liquid_templates/number_to_text.liquid" |> ignore; *)
  (* render_file "liquid_templates/std_test.liquid" () |> ignore; *)
  ()