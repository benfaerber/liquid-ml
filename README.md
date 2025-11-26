# Liquid ML [![](https://img.shields.io/badge/opam-v0.1.2-orange?logo=ocaml)](https://ocaml.org/p/liquid_ml/latest) [![CI](https://github.com/benfaerber/liquid-ml/actions/workflows/ci.yml/badge.svg)](https://github.com/benfaerber/liquid-ml/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/benfaerber/liquid-ml/branch/main/graph/badge.svg)](https://codecov.io/gh/benfaerber/liquid-ml)

[Shopify's Liquid](https://shopify.dev/api/liquid/) templating language for the OCaml programming language

Featured on [Awesome OCaml](https://github.com/ocaml-community/awesome-ocaml)!
Liquid ML was recently added to [YOCaml](https://github.com/xhtmlboi/yocaml) (a great static site generator).

### Getting Started
Install via OPAM:
```
opam install liquid_ml
```
This basic example renders a Liquid file with the default settings. The render is returned as a string.

### Default Settings:
```ocaml
open Liquid_ml

let () =
  Liquid.render "liquid_templates/test.liquid"
  |> Stdio.print_endline
```
(or use `Liquid.render_text` to render a template stored as string instead of a file)

### With Settings:
```ocaml
open Liquid_ml

let () =
  let settings = Settings.make ~error_policy:Warn ~log_policy:Never in
  Liquid.render ~settings "liquid_templates/test.liquid"
  |> Stdio.print_endline

```

### Custom Variable Context
The variable context provides the template with variables accessible in the global scope.
```ocaml
open Liquid_ml
open Liquid_ml.Exports

let () =
  (* Create an object that can be accessed in Liquid using dot notation (environment.language -> "OCaml") *)
  let environment =
    Object.empty
    |> Object.add "language" (String "OCaml")
    |> Object.add "version" (String "4.14.0")
  in

  (* Here we include our favorite_animal as a string and our environment as an object *)
  let context =
    Ctx.empty
    |> Ctx.add "favorite_animal" (String "horse")
    |> Ctx.add "environment" (Object environment)
  in

  let settings = Settings.make ~context () in
  render ~settings "liquid_templates/test.liquid"
  |> Stdio.print_endline

```
Now we can access these variables from the template:
```liquid
My favorite animal is {{ favorite_animal }}!
This template was rendered using {{ environment.language }} Version {{ environment.version }}!
```
This renders as:
```
My favorite animal is horse!
This template was rendered using OCaml Version 4.14.0
```
We can perform logic operations using our variables:
```ocaml
(* OCaml *)
let () =
  let context =
    Ctx.empty
    |> Ctx.add "beatles" (List [String "John"; String "Paul"; String "Ringo"; String "George"])
    |> Ctx.add "my_money" (Number 15.0)
    |> Ctx.add "apple_price" (Number 5.0)
  in

  let settings = Settings.make ~context () in
  render ~settings "liquid_templates/test.liquid"
  |> Stdio.print_endline
```
```liquid
{% comment %}Liquid{% endcomment %}
{% for beatle in beatles %}
    {{ beatle }} lives in a yellow submarine!
{% endfor %}

{% if my_money >= apple_price %}
   You bought an apple for {{ apple_price | money }}
   {% assign after_purchase = my_money | minus: apple_price %}
   You now have {{ after_purchase | money }}
{% else %}
   You don't have enough money!
{% endif %}
```
This template renders to:
```html
John lives in a yellow submarine!
Paul lives in a yellow submarine!
Ringo lives in a yellow submarine!
George lives in a yellow submarine!

You bought an apple for $5.00
You now have $10.00
```

### Settings
You have access to the following settings:

template_directory
- The directory that contains template files. This is used both for the initial lookup (ie Liquid.render "yada.liquid") and for
the `render` tag used within liquid. Default is project root.

log_directory
- Where log files are written to. This must be set if log policy is set to `Verbose`.

error_policy:
- `Strict` - A Liquid Syntax error will raise an exception
- `Warn` - A Liquid Syntax error will print an error message
- `Silent` - Errors will be ignored
- `Custom of (handler: string -> unit)` - Accepts a custom handler function

log_policy:
- `Verbose` - Everything will be logged
- `Minimal` - The most important things will be logged
- `Never` - Log nothing

filters:
- A function that maps filter names to filter functions
- `string -> liquid_filter option`

context:
- Variable Context available in the global scope
- `value Ctx.t` aka `variable_context`

preferred_currency:
- Used in money formatting filters
- `Usd`, `Eur`, `Cad`, `Aud`, `Gbp`
  

### Execution Context
The type `Ctx.t` is used to store the execution context. All variables active in the current scope are stored here. Certain events such as `break` and `continue` are also stored in the execution context. `Ctx.t` is a `Stdlib.Map` learn more here: [OCaml Map Docs](https://ocaml.org/docs/map)



### Custom Filters
A filter is a function that accepts the execution context (`value Ctx.t`) and a list of params (`value list`) and returns a result of a `value`.
This is what a filter looks like in Liquid:
```liquid
{{ "my cool dog" | replace: "dog", "cat" }}
```
This is transformed into a list of a parameters and passed to the filter. Notice how the value on the left side of the pipe is the first in the list. This is how all filters work.
You can think of this filter as function: `replace "my cool dog" "dog" "cat"`. This is the parameter list that will be passed to the filter:
```ocaml
[String "my cool dog"; String "dog"; String "cat"]
```
We then can use pattern matching to type check the filter.
If the wrong type / wrong contents are passed into the filter we can return an error. The error will be processed based on the error policy you set. The default is `Warn` which causes the filter to return `Nil` and print an error message to the console.

### Filter Example

```ocaml
open Liquid_ml
(* defined in syntax.ML *)
type liquid_filter = value Ctx.t -> value list -> (value, string) Result.t

let () =
  (*
    This function accepts a string, anything else will throw an error.
    Note: since we discard the tail, extra params are simply ignored
  *)
  let greet _ = function
    | String person :: _ -> Ok (String ("Hello " ^ person ^ "!"))
    | _ -> Error "greet accepts a string"
  in

  (* This maps the liquid name to our function *)
  let filter_lookup = function
    | "greet" -> Some greet
    | "say_hello" -> Some greet (* we can create an alias to our filter *)
    | _ -> None
  in

  let settings = Settings.make ~filters:filter_lookup in
  render ~settings "liquid_templates/test.liquid"
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
and liquid_object = value Object.t
```

These are all the possible values that can be passed to a filter or stored in the execution context. Date is powered by the library [Calendar](https://github.com/ocaml-community/calendar). Object is a custom `Stdlib.Map` defined in the file `syntax.ML`.

Identifiers are represented as string lists. The ID `apple` is represented as `["apple"]`. The ID `collection.products[0].title` is represented as `["collection"; "products"; "0"; "title"]`.
This means you can refer to indexes using syntax: `collections.products.1.title` which is standard in Liquid.

When a list of parameters is passed to a filter it will never contain the `Var` type. Variables are unpacked before they are passed to filters. If the variable is undefined `Nil` is returned.

### Development:
- Testing - `dune test`
- Build - `dune build`

### Compatibility
This is not a complete port of Liquid. Here is a list of everything that has been ported:
#### Tags
- for
- if
- case
- unless
- capture
- raw
- comment (comment tag, hash comments)
- render
- include
- section
- assign
- cycle
- style
- liquid tag, allowing you to write a block of liquid code without using `{% TAG_NAME %}`
  
#### Filters
- Most filters not explicitly labeled "Shopify" in the [Liquid Filter Docs](https://shopify.dev/api/liquid/filters) have been ported. A complete list can be viewed in `liquid_std/std.ml`

#### Object
- forloop
