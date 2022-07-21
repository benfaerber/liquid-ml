open Core

let read file_name =
  In_channel.read_all file_name

let write file_name data =
  Out_channel.write_all file_name ~data:data