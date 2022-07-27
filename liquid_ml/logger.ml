open Base
open Liquid_syntax

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

let if_some opt f =
  match opt with
  | Some x -> f x |> ignore
  | None -> ()

let vgroup p ld title fname func arg =
  let text = func arg in
  vlog p title;
  if_some ld (fun log_dir ->
    let fpath = log_dir ^ "/" ^ fname in
    File.write fpath text
  );
  vlog p text;
  vflog p Debug.print_line ()
