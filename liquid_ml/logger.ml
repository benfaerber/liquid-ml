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
