open Base
open Liquid_syntax
open Liquid_parser
open Liquid_interpreter

let default_settings = Settings.make ()

let render_text ?(settings = default_settings) text =
  let const x = x in
  let p = settings.log_policy in
  let ld = settings.log_directory in
  let raw_text = text |> Preprocessor.preprocess in
  Logger.vgroup p ld "Raw Text:" "raw_text.txt" const raw_text;

  let lex_tokens = Lexer.lex raw_text in
  Logger.vgroup p ld "Lex Tokens:" "tokens.txt"
    Debug.lex_tokens_as_string_with_index lex_tokens;

  let ast = Parser.parse lex_tokens in
  Logger.vgroup p ld "Abstract Syntax Tree:" "ast.txt" Debug.show_ast ast;

  let rendered_text = Interpreter.start settings ast in

  (* Logger.vgroup p ld "Render:" "render.txt" const rendered_text; *)
  rendered_text

let render ?(settings = default_settings) filename =
  let filepath = settings.template_directory ^ "/" ^ filename in
  let raw_text = filepath |> File.read in
  render_text ~settings raw_text

include Exports
