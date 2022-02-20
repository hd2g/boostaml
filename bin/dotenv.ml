module Envs = Map.Make(String)

let filename = ".env"

let re = Re.Posix.({re|^([^=]*)="?([^"]*)"?$|re} |> re |> compile)

let ( >>= ) = Option.bind

let envs_of_file filename =
  assert (Sys.file_exists filename);
  filename
  |> Core.In_channel.read_lines
  |> List.filter_map
       (fun line ->
         Re.exec_opt re line >>= (fun line ->
          line |> Re.Group.all |> Array.to_list
          |> (function
              | [ _; key; value ] -> Some (key, value)
              | _ -> None)))
  |> List.to_seq
  |> Envs.of_seq

let config () =
  let envs = envs_of_file filename in
  fun name ->
  match Sys.getenv_opt name with
  | Some _ as value -> value
  | None -> Envs.find_opt name envs
