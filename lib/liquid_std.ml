open Base

let identity _ params = List.hd_exn params

let function_from_string = function
  | "captialize"
  | _ -> identity