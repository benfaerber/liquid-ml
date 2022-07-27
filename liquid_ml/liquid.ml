open Base
open Liquid_syntax
open Liquid_parser
open Liquid_interpreter

(* Reexports *)
include Syntax
module Date = Date
module Settings = Settings
module Values = Values
(* End Reexports *)

let vflog policy func arg =
  match policy with
  | Settings.Verbose -> func arg
  | _ -> ()

let mflog policy func arg =
  match policy with
  | Settings.Verbose | Minimal -> func arg
  | _ -> ()

let pwrite fname text = File.write fname text

let vlog policy arg = vflog policy Stdio.print_endline arg
let mlog policy arg = mflog policy Stdio.print_endline arg

let vgroup p ld title fname func arg =
  let text = func arg in
  vlog p title;
  match ld with
  | Some log_dir ->
    let fpath = log_dir ^ "/" ^ fname in
    File.write fpath text
  | None -> ();
  vlog p text;
  vflog p Debug.print_line ()


let default_settings = Settings.make ()

let render_text ?(settings = default_settings) text =
  let s x = x in
  let p = settings.log_policy in
  let ld = settings.log_directory in
  let raw_text = text |> Preprocessor.preprocess in
  vgroup p ld "Raw Text:" "raw_text.txt" s raw_text;

  let lex_tokens = Lexer.lex raw_text in
  vgroup p ld "Lex Tokens:" "tokens.txt" Debug.lex_tokens_as_string_with_index lex_tokens;

  let ast = Parser.parse lex_tokens in
  vgroup p ld "Abstract Syntax Tree:" "ast.txt" Debug.ast_as_string ast;

  let rendered_text = Interpreter.start settings ast in
  vgroup p ld "Render:" "render.txt" s rendered_text;

  rendered_text

let render ?(settings = default_settings) filename =
  let filepath = settings.template_directory ^ "/" ^ filename in
  let raw_text = filepath |> File.read in
  render_text ~settings raw_text
