open Base
open Liquid_ml
open Liquid


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
      let even = Stdlib.Int.rem (Float.to_int n) 2 = 0 in
      Ok (Bool even)
    | String s :: _ ->
      let n = Float.of_string s in
      let even = Stdlib.Int.rem (Float.to_int n) 2 = 0 in
      Ok (Bool even)
    | _ -> Error "is_even accepts a number"
  in

  let custom_filters = function
    | "greet" -> Some greet
    | "is_even" -> Some is_even
    | _ -> None
  in

  let enviroment =
    Object.empty
    |> Object.add "language" (String "OCaml")
    |> Object.add "version" (String "4.14.0")
  in

  enviroment |> show_liquid_object |> Stdio.print_endline; 
  String "hello" |> show_value |> Stdio.print_endline;

  let context =
    Ctx.empty
    |> Ctx.add "favorite_animal" (String "horse")
    |> Ctx.add "enviroment" (Object enviroment)
    |> Ctx.add "collection" Test_data.test_collection
  in

  let settings = Settings.make
    ~error_policy:Warn
    ~filters:custom_filters
    ~log_policy:Verbose
    ~template_directory:"liquid_templates"
    ~log_directory:"logs"
    ~preferred_currency:Eur
    ~timezone:Date.Timezone.MST
    ~context
    ()
  in
  settings |> Settings.show |> Stdio.print_endline;

  render ~settings "std_test.liquid"
  (* |> Stdio.print_endline *)
  |> ignore

let () = test ()
