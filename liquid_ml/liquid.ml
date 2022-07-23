open Base
open Liquid_syntax
open Liquid_parser
open Liquid_interpreter
open Syntax

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

let render ?(settings = default_settings) filename =
  let s x = x in
  let p = settings.log_policy in
  let filepath = settings.template_directory ^ "/" ^ filename in
  let raw_text = filepath |> File.read |> Preprocessor.preprocess in
  let ld = settings.log_directory in
  vgroup p ld "Raw Text:" "raw_text.txt" s raw_text;

  let lex_tokens = Lexer.lex raw_text in
  vgroup p ld "Lex Tokens:" "tokens.txt" Debug.lex_tokens_as_string_with_index lex_tokens;

  let ast = Parser.parse lex_tokens in
  vgroup p ld "Abstract Syntax Tree:" "ast.txt" Debug.ast_as_string ast;

  let rendered_text = Interpreter.start settings ast in
  vgroup p ld "Render:" "render.txt" s rendered_text;

  rendered_text

let test () =
  let greet _ = function
    | String person :: _ ->
      Ok (String ("Hello " ^ person ^ "!"))
    | List people :: _ ->
      let greet_person = function
        | String person -> String ("Hello " ^ person ^ "!")
        | _ -> Nil
      in

      let greeted = List.map people ~f:greet_person in
      Ok (List greeted)
    | _ -> Error "greet accepts a string or a list of strings"
  in

  let is_even _ = function
    | Number n :: _ ->
      let even = (Float.to_int n) % 2 = 0 in
      Ok (Bool even)
    | String s :: _ ->
      let n = Float.of_string s in
      let even = (Float.to_int n) % 2 = 0 in
      Ok (Bool even)
    | _ -> Error "is_even accepts a number"
  in

  let custom_filters = function
    | "greet" -> Some greet
    | "is_even" -> Some is_even
    | _ -> None
  in

  let enviroment =
    Obj.empty
    |> Obj.add "language" (String "OCaml")
    |> Obj.add "version" (String "4.14.0")
  in

  let context =
    Ctx.empty
    |> Ctx.add "favorite_animal" (String "horse")
    |> Ctx.add "enviroment" (Object enviroment)
    |> Ctx.add "collection" Test_data.test_collection
  in

  let settings = Settings.make
    ~error_policy:Warn
    ~log_policy:Minimal
    ~filters:custom_filters
    ~template_directory:"liquid_templates"
    ~log_directory:"logs"
    ~context
    ()
  in
  render ~settings "std_test.liquid" |> ignore