# Liquid ML

[Shopify's Liquid](https://shopify.dev/api/liquid/) templating language for the OCaml!

## !!!! NOTICE !!!!
This is a work in progress. You are welcome to use it but the port is only ~70% complete so I can not guarantee that the API / Interpreter rules will remain consistant.


### Getting Started
This basic example renders a Liquid file with the default settings. The render is returned as a string.

### Default Settings:
```ocaml
open Liquid_ml

let () =
  Liquid.render "liquid_templates/block_test.liquid" ()
  |> Stdio.print_endline
```

### With Settings:
```ocaml
open Liquid_ml

let () =
  let settings = Settings.make ~error_policy:Warn ~log_policy:Never in
  render "liquid_templates/std_test.liquid" ~settings ()
  |> Stdio.print_endline

```

You have access to the following settings:

error_policy:
- Strict - A Liquid Syntax error will raise an exception
- Warn - A Liquid Syntax error will print an error message
- Silent - Errors will be ignored

log_policy:
- Verbose - Everything will be logged
- Minimal - The most important things will be logged
- Never - Log nothing

filters:
- A function that maps filter names to filter functions
- `string -> liquid_filter option`


### Custom Filters
A filter is a function that accepts the execution context and a list of params and returns a result of a Value.
This is what a filter looks like in Liquid:
```liquid
{{ "my cool dog" | replace: "dog", "cat" }}
```
This is transformed into a list of a parameters and passed to the filter:
```ocaml
[String "my cool dog"; String "dog"; String "cat"]
```
We then can use pattern matching to type check the filter.
If the wrong type / wrong contents are passed into the filter we can return an error.

### Filter Example

```ocaml
open Liquid_ml
(* defined in syntax.ML *)
type liquid_filter = value Ctx.t -> value list -> (value, string) Result.t

let () =
  (* This function accepts a string, anything else will throw an error *)
  let greet _ = function
    | String person :: _ -> Ok (String ("Hello " ^ person ^ "!"))
    | _ -> Error "greet accepts a string or a list of strings"
  in

  (* This maps the liquid name to our function *)
  let filter_lookup = function
    | "greet" -> Some greet
    | "say_hello" -> Some greet (* we can create an alias to our filter *)
    | _ -> None
  in

  let settings = Settings.make ~filters:filter_lookup in
  render "liquid_templates/std_test.liquid" ~settings ()
  |> Stdio.print_endline

```

### Lookup Function
A filter lookup function maps filter names (the name in liquid) to the OCaml filter function in the example above we map the string `greet` to the function `greet`.

```liquid
Liquid Code:
{{ "John" | greet }}
Render:
Hello John!
```

### Liquid Types

``` ocaml
type value =
| Bool of bool
| String of string
| Number of float
| Var of string list
| List of value list
| Date of Date.t
| Object of liquid_object
| Nil
```

These are all the possible values that can be passed to a filter or stored in the execution context. Date is powered by the library [Calendar](https://github.com/ocaml-community/calendar). Object is a custom `Caml.Map` defined in the file `syntax.ML`.

When a list of parameters is passed to a filter it will never contain the `Var` type. Variables are unpacked before they are passed to filters. If the variable is undefined `Nil` is returned.

### Execution Context
The type `Ctx.t` is used to store the execution context. All variables active in the current scope are stored here. Certain events such as `break` and `continue` are also stored in the execution context. `Ctx.t` is a `Caml.Map` learn more here: [OCaml Map Docs](https://ocaml.org/docs/map)

### Compatibility
This is not a complete port of Liquid. Here is a list of everything that has been ported:
#### Tags
- for
- if
- case
- unless
- capture
- raw
- render
- include
- section
- assign
- cycle

#### Filters
- Most filters not explicitly labeled "Shopify" in the [Liquid Filter Docs](https://shopify.dev/api/liquid/filters) have been ported. A complete list can be viewed in `liquid_std/std.ml`

#### Object
- forloop
